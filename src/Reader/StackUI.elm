module Reader.StackUI exposing (StackUI, fromTrace, handleExprClick, handleOpenChildFrame, handleExprHover, handleExprUnHover, viewStackUI)

import Debug

import Elm.Kernel.Reader
import Elm.Kernel.Debugger

import Reader.SourceMap as SourceMap exposing (SourceMap)
import Reader.SourceMap.Ids as SourceMapIds
import Reader.TraceData as TraceData exposing (TraceData)
import Reader.TraceData.Value as Value exposing (Value)
import Reader.Msg as Msg exposing (Msg)
import Reader.TraceData.FrameDict as FrameDict exposing (FrameDict)
import Reader.SourceMap.ExprDict as ExprDict exposing (ExprDict)
import Reader.StackUI.RenderedFrameMap as RenderedFrameMap exposing (RenderedFrameMap)
import Reader.StackUI.OpenFrame as OpenFrame exposing (OpenFrame)
import Reader.StackUI.StackTree as StackTree exposing (StackTree)

import Array exposing (Array)
import Set exposing (Set)
import Dict exposing (Dict)

import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E

import Json.Decode as JD

-- EXPR UI

type alias ExprUI = String


-- STACK UI

type alias StackUI =
    { stackTree : StackTree
    , renderedFrames : RenderedFrameMap
    , stackFrames :
        FrameDict
            { exprs : ExprDict ActiveExprData
            , deadExprs : ExprDict ()
            , topY : Int
            , height : Int
            }
    , previewedExpr : Maybe ( TraceData.FrameId, SourceMap.ExprId )
    -- TODO: , pinnedExprs : List ( TraceData.FrameId, SourceMap.ExprId )
    }


type alias ActiveExprData =
    { line : Int
    , value : Value
    , model : Maybe ExprUI
    , childFrame : Maybe TraceData.FrameThunk
    }


exprsFromTrace : SourceMap -> TraceData.InstrumentedFrameData -> ( ExprDict ActiveExprData, ExprDict () )
exprsFromTrace srcMap frame =
    case SourceMap.getFrame frame.sourceId srcMap of
        Nothing ->
            ( ExprDict.empty, ExprDict.empty )

        Just sourceFrame ->
            let
                frameRegions = sourceFrame.exprRegions

                deadExprs =
                    -- Get the expressions in the sourcemap that weren't traced
                    ExprDict.keys sourceFrame.exprRegions
                    |> List.filterMap
                        (\exprId ->
                            case ExprDict.get exprId frame.exprs |> Maybe.map TraceData.exprValue of
                                Just value ->
                                    Nothing

                                Nothing ->
                                    Just ( exprId, () )
                        )
                    |> ExprDict.fromList

                exprRegion exprId =
                    ExprDict.get exprId frameRegions
                    |> Maybe.andThen List.head

                liveExprs =
                    frame.exprs
                        |> ExprDict.toList
                        |> List.filterMap
                            (\(exprId, expr) ->
                                case (TraceData.exprValue expr, exprRegion exprId) of
                                    (Just value, Just { start }) ->
                                        Just
                                            ( exprId
                                            , { line = start.line - sourceFrame.region.start.line
                                              , value = value
                                              , model = Nothing
                                              , childFrame = TraceData.exprChildFrame expr
                                              }
                                            )

                                    (_, _) ->
                                        Nothing
                            )
                        |> ExprDict.fromList
            in
            ( liveExprs, deadExprs )


fromTrace : SourceMap -> TraceData.Frame -> Maybe StackUI
fromTrace srcMap frameTrace =
    let
        openFrame = OpenFrame.fromTrace frameTrace

        ( stackFrames, sourceFrameIds ) =
            case frameTrace of
                TraceData.Instrumented data ->
                    let
                        ( liveExprs, deadExprs ) = exprsFromTrace srcMap data
                        height =
                            -- On error, default to 10 arbitrarily
                            SourceMap.getFrameHeight data.sourceId srcMap |> Maybe.withDefault 10
                    in
                    ( FrameDict.fromList
                        [ ( data.runtimeId
                          , { topY = 1
                            , height = height
                            , exprs = liveExprs
                            , deadExprs = deadExprs
                            }
                          )
                        ]
                    , [ data.sourceId ]
                    )

                TraceData.NonInstrumented _ subframes ->
                    ( subframes
                        |> List.map
                            (\frame ->
                                let
                                    ( liveExprs, deadExprs ) = exprsFromTrace srcMap frame
                                    height =
                                        -- On error, default to 10 arbitrarily
                                        SourceMap.getFrameHeight frame.sourceId srcMap |> Maybe.withDefault 10
                                in
                                ( frame.runtimeId
                                , { topY = 1
                                  , height = height
                                  , exprs = liveExprs
                                  , deadExprs = deadExprs
                                  }
                                )
                            )
                        |> FrameDict.fromList
                    , List.map .sourceId subframes
                        -- Deduplicate:
                        |> List.foldr
                            (\item accum ->
                                if List.member item accum then
                                    accum
                                else
                                    item :: accum)
                            []
                    )
    in
    case StackTree.singleton openFrame of
        Just singleton ->
            Just
                { stackTree = singleton
                , renderedFrames = RenderedFrameMap.ensure srcMap sourceFrameIds RenderedFrameMap.empty
                , stackFrames = stackFrames
                , previewedExpr = Nothing
                }

        Nothing ->
            Nothing



frameLineSpacing : Int
frameLineSpacing = 2


frameLinePadding : Float
frameLinePadding = 0.5

nonInstrumentedFrameHeight : Int
nonInstrumentedFrameHeight = 2


openChildFrame : SourceMap -> StackUI -> TraceData.FrameId -> TraceData.Frame -> StackUI
openChildFrame srcMap stackUI parentFrameId childFrame =
    let
        childsOpenFrame =
            OpenFrame.fromTrace childFrame

        ( newRenderedFrames, childFrameHeight ) =
            case childFrame of
                TraceData.Instrumented { sourceId } ->
                    let
                        ( renders, renderedChild ) =
                            RenderedFrameMap.ensureOne srcMap sourceId stackUI.renderedFrames
                    in
                    ( renders, renderedChild.lines )

                TraceData.NonInstrumented _ _ ->
                    ( stackUI.renderedFrames, nonInstrumentedFrameHeight )

        newStackFrames =
            if FrameDict.member (TraceData.frameIdOf childFrame) stackUI.stackFrames then
                stackUI.stackFrames
            else
                let
                    ( liveExprs, deadExprs ) =
                        case childFrame of
                            TraceData.Instrumented data ->
                                exprsFromTrace srcMap data

                            TraceData.NonInstrumented _ _ ->
                                -- As a non-instrumented frame, there are no exprs:
                                ( ExprDict.empty, ExprDict.empty )

                    childFrameTopY =
                        case FrameDict.get parentFrameId stackUI.stackFrames of
                            Nothing ->
                                1 -- unreachable so long as stackFrames is properly maintained

                            Just parentFrame ->
                                parentFrame.topY + parentFrame.height + frameLineSpacing
                in
                FrameDict.set
                    (OpenFrame.runtimeFrameIdOf childsOpenFrame)
                    { topY = childFrameTopY
                    , height = childFrameHeight
                    , exprs = liveExprs
                    , deadExprs = deadExprs
                    }
                    stackUI.stackFrames

        newStackUI =
            { stackUI
                | stackTree = StackTree.openChildFrame parentFrameId childsOpenFrame stackUI.stackTree
                , stackFrames = newStackFrames
                , renderedFrames = newRenderedFrames
                }
    in
    case OpenFrame.childFrameOf childsOpenFrame of
        Nothing ->
            newStackUI

        Just grandchildFrame ->
            -- When childFrame is NonInstrumented, since it has just been opened,
            -- its first child (the grandchild) must also be opened
            openChildFrame srcMap newStackUI (TraceData.frameIdOf childFrame) grandchildFrame


handleExprClick : StackUI -> SourceMap -> TraceData.FrameId -> SourceMap.ExprId -> StackUI
handleExprClick stackUI srcMap parentFrameId exprId =
    let
        expr =
            FrameDict.get parentFrameId stackUI.stackFrames
            |> Maybe.map .exprs
            |> Maybe.andThen (ExprDict.get exprId)
    in
    case expr |> Maybe.andThen .childFrame of
        Just childFrame ->
            let
                evaluatedFrame =
                    TraceData.evaluate childFrame

                newStackFrames =
                    -- Update exprId's childFrame thunk with the evaluated version
                    let
                        setExprChildFrameCache e = { e | childFrame = Just (TraceData.Evaluated evaluatedFrame) }
                    in
                    stackUI.stackFrames
                        |> FrameDict.update
                            parentFrameId
                            (\parentFrameData ->
                                let newExprs = ExprDict.update exprId setExprChildFrameCache parentFrameData.exprs in
                                { parentFrameData | exprs = newExprs })
            in
            openChildFrame srcMap { stackUI | stackFrames = newStackFrames } parentFrameId evaluatedFrame

        Nothing ->
            stackUI


handleOpenChildFrame : StackUI -> SourceMap -> TraceData.FrameId -> TraceData.InstrumentedFrameData -> StackUI
handleOpenChildFrame stackUI srcMap parentId child =
    openChildFrame srcMap stackUI parentId (TraceData.Instrumented child)


handleExprHover : StackUI -> TraceData.FrameId -> SourceMap.ExprId -> StackUI
handleExprHover stackUI frame expr =
    let
        maybeExprData =
            FrameDict.get frame stackUI.stackFrames
            |> Maybe.map .exprs
            |> Maybe.andThen (ExprDict.get expr)

        exprModel =
            Maybe.andThen .model maybeExprData
    in
    case ( maybeExprData, exprModel ) of
        ( Nothing, _ ) ->
            { stackUI | previewedExpr = Nothing }

        ( Just _, Just _ ) ->
            { stackUI | previewedExpr = Just ( frame, expr ) }

        ( Just exprData, Nothing ) ->
            let
                newExprModel =
                    Value.toString exprData.value

                newStackFrames =
                    FrameDict.update
                        frame
                        (\frameData ->
                            { frameData
                                | exprs =
                                    frameData.exprs
                                    |> ExprDict.set expr { exprData | model = Just newExprModel }
                                }
                        )
                        stackUI.stackFrames
            in
            { stackUI
                | previewedExpr = Just ( frame, expr )
                , stackFrames = newStackFrames
                }


handleExprUnHover : StackUI -> StackUI
handleExprUnHover stackUI =
    { stackUI | previewedExpr = Nothing }


-- VIEW

previewedExprCSSRule = """ {
  border-radius: 3px;
  background-color: rgb(230, 230, 200) !important;
}
"""

deadExprsCSSRule = """ {
  color: #bbb;
  background-color: none;
}
"""

functionCallExprsCSSRule = """ {
    cursor: pointer;
}
"""

openFunctionCallExprsCSSRule = """ {
  border-radius: 3px;
  text-decoration: underline;
  background-color: rgba(0,0,0,0.07);
}
"""

cssRuleForExprs : List ( TraceData.FrameId, SourceMap.ExprId ) -> String -> List (Html msg)
cssRuleForExprs exprs rule =
    if List.isEmpty exprs then
        []
    else
        let
            selector =
                List.map
                    (\(TraceData.FrameId frameId _, SourceMapIds.ExprId exprId) ->
                        ".elm-reader-frame-"
                            ++ String.fromInt frameId
                            ++ " .elm-reader-expr-"
                            ++ String.fromInt exprId
                    )
                    exprs
                |> String.join ", "
        in
        [ Html.text selector, Html.text rule ]

viewStyle : StackUI -> List (Html msg)
viewStyle stackUI =
    -- TODO:
    -- - check expr.childFrame on each expr to decide whether to `cursor: pointer;`
    -- - highlight exprs whose childFrame is open
    let
        previewedExprStyle =
            case stackUI.previewedExpr of
                Nothing ->
                    []

                Just ( TraceData.FrameId frameId _, SourceMapIds.ExprId exprId ) ->
                    let
                        selector =
                            ".elm-reader-frame-"
                                ++ String.fromInt frameId
                                ++ " .elm-reader-expr-"
                                ++ String.fromInt exprId
                    in
                    [ Html.text selector, Html.text previewedExprCSSRule ]

        exprsWithChildFrames =
            StackTree.map
                (\openFrame ->
                    let
                        runtimeId =
                            OpenFrame.runtimeFrameIdOf openFrame
                    in
                    case FrameDict.get runtimeId stackUI.stackFrames of
                        Just { exprs } ->
                            ExprDict.toList exprs
                            |> List.filterMap
                                (\(exprId, expr) ->
                                    Maybe.map (\child -> ( runtimeId, exprId, child )) expr.childFrame
                                )

                        Nothing ->
                            []
                )
                stackUI.stackTree
            |> List.concatMap (\x -> x)

        functionCallExprs =
            exprsWithChildFrames
            |> List.map (\(runtimeId, exprId, child) -> (runtimeId, exprId))

        openFunctionCallExprs =
            exprsWithChildFrames
            |> List.filter (\(runtimeId, exprId, child) -> StackTree.isOpen (TraceData.frameIdOfThunk child) stackUI.stackTree)
            |> List.map (\(runtimeId, exprId, child) -> (runtimeId, exprId))

        displayedDeadExprs =
            StackTree.map
                (\openFrame ->
                    let
                        runtimeId =
                            OpenFrame.runtimeFrameIdOf openFrame
                    in
                    case FrameDict.get runtimeId stackUI.stackFrames of
                        Just { deadExprs } ->
                            Just ( runtimeId, deadExprs )

                        Nothing ->
                            Nothing
                )
                stackUI.stackTree
            |> List.concatMap
                (\maybeExpr ->
                    case maybeExpr of
                        Just ( frameId, exprIds ) ->
                            ExprDict.keys exprIds
                            |> List.map (\exprId -> ( frameId, exprId ))

                        Nothing ->
                            []
                )
    in
    previewedExprStyle
        ++ cssRuleForExprs displayedDeadExprs deadExprsCSSRule
        ++ cssRuleForExprs functionCallExprs functionCallExprsCSSRule
        ++ cssRuleForExprs openFunctionCallExprs openFunctionCallExprsCSSRule


onMouseOver =
    E.on "mouseover" <|
        JD.andThen Elm.Kernel.Reader.mouseEventToMessage JD.value


onClick =
    E.on "mousedown" <|
        JD.andThen Elm.Kernel.Reader.mouseEventToMessage JD.value


maybeToList : Maybe a -> List a
maybeToList maybe =
    case maybe of
        Just a ->
            [ a ]

        Nothing ->
            []


viewStackUI : StackUI -> Html Msg
viewStackUI stackUI =
    let
        previewedExpr =
            case stackUI.previewedExpr of
                Nothing ->
                    []

                Just ( frameId, exprId ) ->
                    case FrameDict.get frameId stackUI.stackFrames of
                        Just frameData ->
                            case ExprDict.get exprId frameData.exprs of
                                Just exprData ->
                                    let
                                        top = String.fromInt (frameData.topY + exprData.line) ++ "em"
                                        content =
                                            Maybe.map Html.text exprData.model
                                            |> maybeToList
                                    in
                                    [ Html.div
                                        [ A.style "position" "absolute"
                                        , A.style "top" top
                                        ]
                                        content
                                    ]

                                Nothing ->
                                    []
                        Nothing ->
                            []
    in
    Html.div
        [ A.class "elm-reader-container" ]
        [ Html.div
            [ A.class "elm-reader-details" ]
            previewedExpr
        , Html.div
            [ A.class "elm-reader-stack"
            , E.onMouseOut Msg.UnHoverExpr
            , onMouseOver
            , onClick
            ]
            (StackTree.map (viewOpenFrame stackUI) stackUI.stackTree)
        , Html.node
            "style"
            []
            (viewStyle stackUI)
        ]


viewOpenFrame : StackUI -> OpenFrame -> Html Msg
viewOpenFrame stackUI openFrame =
    let
        ((TraceData.FrameId numRuntimeId _) as runtimeId) =
            OpenFrame.runtimeFrameIdOf openFrame

        ( topY, height ) =
            FrameDict.get runtimeId stackUI.stackFrames
            |> Maybe.map (\f -> ( f.topY, f.height ))
            |> Maybe.withDefault (1, 1)

        frameWrapper =
            Html.div
                [ A.class "elm-reader-frame"
                , A.class ("elm-reader-frame-" ++ String.fromInt numRuntimeId)
                , A.style "top" (String.fromFloat (toFloat topY - frameLinePadding) ++ "em")
                , A.style "height" (String.fromInt height ++ "em")
                ]
    in
    case openFrame of
        OpenFrame.NonInstrumented _ idx subframes ->
            frameWrapper
                [ Html.text "This frame is not instrumented, but has "
                , Html.text (String.fromInt (Array.length subframes))
                , Html.text " instrumented sub-frames."
                , Html.br [] []
                , Html.text "Showing frame "
                , Html.input
                    [ A.value (String.fromInt idx), A.type_ "number"
                    , E.onInput
                        (\value ->
                            case String.toInt value |> Maybe.andThen (\i -> Array.get i subframes) of
                                Just subFrameData ->
                                    Msg.OpenChildFrame runtimeId subFrameData

                                Nothing ->
                                    Msg.NoOp
                        )
                    ]
                    []
                , Html.text " out of "
                , Html.text (String.fromInt (Array.length subframes))
                ]

        OpenFrame.Instrumented { sourceId } ->
            case RenderedFrameMap.get sourceId stackUI.renderedFrames of
                Just { html } ->
                    frameWrapper [ html ]

                Nothing ->
                    frameWrapper [ Html.text "Frame failed to render -- it is not present in RenderedFrameMap!" ]
