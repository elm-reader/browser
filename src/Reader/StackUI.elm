module Reader.StackUI
    exposing
        ( StackUI
        , fromTrace
        , handleExprOpen
        , handleExprPin
        , handleExprHover
        , handleExprUnHover
        , handleExprUIMessage
        , handleOpenChildFrame
        , viewStackUI
        )

import Debug

import Elm.Kernel.Reader

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

import Debugger.Expando as Expando exposing (Expando)

import Array exposing (Array)
import Set exposing (Set)
import Dict exposing (Dict)

import Html exposing (Html)
import Html.Keyed
import Html.Attributes as A
import Html.Events as E

import Json.Decode as JD

-- EXPR UI

type alias ExprUI = Expando

viewExprUI
    : { hovered : Bool, pinned : Bool, y : Int, frameId : TraceData.FrameId, exprId : SourceMap.ExprId, ui : ExprUI }
    -> Html Msg
viewExprUI { hovered, y, frameId, exprId, ui } =
    let
        ( hoverClass, hoverBorder ) =
            if hovered then
                ( [ A.class "elm-reader-expr-ui--hovered" ]
                , [ Html.div [ A.class "elm-reader-expr-ui--hovered-rightborder" ] [] ]
                )
            else
                ( [], [ Html.div [ A.class "elm-reader-expr-ui-rightborder" ] [] ] )
    in
    Html.div
        (hoverClass ++
            [ A.class "elm-reader-expr-ui"
            , A.style "top" (String.fromInt y ++ "em")
            , E.onMouseOver (Msg.HoverExpr frameId exprId)
            , E.onMouseOut Msg.UnHoverExpr
            ])
        ( Html.map (Msg.ExprUIMsg frameId exprId) (Expando.view Nothing ui)
            :: hoverBorder )


-- STACK UI

type alias StackUI =
    { stackTree : StackTree
    , renderedFrames : RenderedFrameMap
    -- TODO: move stackFrames data into the StackTree. The logarithmic
    -- lookup matters only helps to look up expressions on hover or click,
    -- but searching linearly for the frame among *open frames* is fine.
    , stackFrames :
        FrameDict
            { exprs : ExprDict ActiveExprData
            , deadExprs : ExprDict ()
            , topY : Int
            , height : Int
            }
    , hoveredExpr : Maybe ( TraceData.FrameId, SourceMap.ExprId )
    -- TODO: , pinnedExprs : List ( TraceData.FrameId, SourceMap.ExprId )
    }


type alias ActiveExprData =
    { line : Int
    , value : Value
    , model : Maybe { pinned : Bool, ui : ExprUI }
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

                exprStartLine exprId =
                    ExprDict.get exprId frameRegions
                    |> Maybe.andThen (List.map (\r -> ( r.start.line, r.start.col )) >> List.minimum)
                    |> Maybe.map Tuple.first

                liveExprs =
                    frame.exprs
                        |> ExprDict.toList
                        |> List.filterMap
                            (\(exprId, expr) ->
                                case (TraceData.exprValue expr, exprStartLine exprId) of
                                    (Just value, Just line) ->
                                        Just
                                            ( exprId
                                            , { line = line - sourceFrame.region.start.line
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
                        ( liveExprs_, deadExprs ) = exprsFromTrace srcMap data
                        -- Open the arguments to `update`:
                        -- TODO: do this properly
                        liveExprs =
                            ExprDict.map
                                (\(SourceMapIds.ExprId i) expr ->
                                    if i == 1 || i == 2 then
                                        { expr | model = Just { pinned = True, ui = Value.toExpando expr.value } }
                                    else
                                        expr
                                )
                                liveExprs_
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
                , hoveredExpr = Nothing
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


handleExprOpen : StackUI -> SourceMap -> TraceData.FrameId -> SourceMap.ExprId -> StackUI
handleExprOpen stackUI srcMap parentFrameId exprId =
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


updateExpr : StackUI -> TraceData.FrameId -> SourceMap.ExprId -> (ActiveExprData -> ActiveExprData) -> StackUI
updateExpr stackUI frameId exprId updater =
    { stackUI
        | stackFrames =
            FrameDict.update
                frameId
                (\frameInfo ->
                    { frameInfo
                        | exprs =
                            ExprDict.update
                                exprId
                                updater
                                frameInfo.exprs
                        }
                )
                stackUI.stackFrames
        }

checkExpr : StackUI -> TraceData.FrameId -> SourceMap.ExprId -> (ActiveExprData -> Bool) -> Bool
checkExpr stackUI frameId exprId f =
    FrameDict.get
        frameId
        stackUI.stackFrames
    |> Maybe.map .exprs
    |> Maybe.andThen (ExprDict.get exprId)
    |> Maybe.map f
    |> Maybe.withDefault False


handleExprPin : StackUI -> TraceData.FrameId -> SourceMap.ExprId -> StackUI
handleExprPin stackUI frameId exprId =
    let
        newStackUI =
            updateExpr
                stackUI
                frameId
                exprId
                (\expr ->
                    { expr
                        | model =
                            Just
                                { pinned = not (expr.model |> Maybe.map .pinned |> Maybe.withDefault False)
                                , ui =
                                    case expr.model of
                                        Just { ui } ->
                                            ui

                                        Nothing ->
                                            Value.toExpando expr.value
                                }
                        }
                )
    in
    if not (checkExpr newStackUI frameId exprId (.model >> Maybe.map .pinned >> Maybe.withDefault False)) then
        { newStackUI | hoveredExpr = Nothing }
    else
        newStackUI


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
            { stackUI | hoveredExpr = Nothing }

        ( Just _, Just _ ) ->
            { stackUI | hoveredExpr = Just ( frame, expr ) }

        ( Just exprData, Nothing ) ->
            let
                newExprModel =
                    { pinned = False, ui = Value.toExpando exprData.value }

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
                | hoveredExpr = Just ( frame, expr )
                , stackFrames = newStackFrames
                }


handleExprUnHover : StackUI -> StackUI
handleExprUnHover stackUI =
    { stackUI | hoveredExpr = Nothing }


handleExprUIMessage : StackUI -> TraceData.FrameId -> SourceMap.ExprId -> Expando.Msg -> StackUI
handleExprUIMessage stackUI frameId exprId expandoMsg =
    updateExpr
        stackUI
        frameId
        exprId
        (\expr ->
            { expr
                | model =
                    expr.model
                        |> Maybe.map (\model -> { model | ui = Expando.update expandoMsg model.ui })
                }

        )


-- VIEW

hoveredExprCSSRule = """ {
  border-radius: 3px;
  background-color: rgb(232, 232, 240) !important;
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
            case stackUI.hoveredExpr of
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
                    [ Html.text selector, Html.text hoveredExprCSSRule ]

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


onMouseDown =
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
        stackHeight =
            FrameDict.get (StackTree.lastOpenChild stackUI.stackTree) stackUI.stackFrames
            |> Maybe.map
                (\{ topY, height } ->
                    let
                        linesToMiddleOfLastFrame =
                            String.fromInt (topY + height // 2) ++ "em"

                        total =
                            "calc("
                            ++ linesToMiddleOfLastFrame
                            ++ " + max(50vh, 25vh + "
                            ++ String.fromInt (height // 2)
                            ++ "em))"
                    in
                    [ A.style "height" total ]
                )
            |> Maybe.withDefault []
    in
    Html.div
        [ A.class "elm-reader-container" ]
        [ viewDetailsSidebar stackUI
        , Html.div
            ( [ A.class "elm-reader-stack"
              , E.onMouseOut Msg.UnHoverExpr
              , onMouseOver
              , onMouseDown
              ] ++ stackHeight)
            (StackTree.map (viewOpenFrame stackUI) stackUI.stackTree)
        , Html.node
            "style"
            []
            (viewStyle stackUI)
        ]


exprIdsToKey : TraceData.FrameId -> SourceMap.ExprId -> String
exprIdsToKey (TraceData.FrameId f _) (SourceMapIds.ExprId e) =
    "frame-" ++ String.fromInt f ++ "--expr" ++ String.fromInt e


viewDetailsSidebar : StackUI -> Html Msg
viewDetailsSidebar stackUI =
    let
        isHovered =
            case stackUI.hoveredExpr of
                Just ( hoveredFrameId, hoveredExprId ) ->
                    (\frameId exprId ->
                        TraceData.frameIdsEqual frameId hoveredFrameId
                            && exprId == hoveredExprId)

                Nothing ->
                    (\_ _ -> False)

        displayedExprs =
            StackTree.map
                (\openFrame ->
                    let
                        frameId =
                            OpenFrame.runtimeFrameIdOf openFrame
                    in
                    case FrameDict.get frameId stackUI.stackFrames of
                        Nothing ->
                            []

                        Just { topY, exprs } ->
                            exprs
                            |> ExprDict.toList
                            |> List.filterMap
                                (\( exprId, { line, model } ) ->
                                    case model of
                                        Just { pinned, ui } ->
                                            if pinned || isHovered frameId exprId then
                                                Just
                                                    { frameId = frameId
                                                    , exprId = exprId
                                                    , y = topY + line
                                                    , ui = ui
                                                    , hovered = isHovered frameId exprId
                                                    , pinned = pinned
                                                    }
                                            else
                                                Nothing

                                        _ ->
                                            Nothing 
                                )
                )
                stackUI.stackTree
            |> List.concatMap identity
            |> List.sortBy (\{ y } -> y)
            |> List.foldl
                (\exprViewInfo (prevElems, bottomOfPrev) ->
                    let
                        topOfThis =
                            max (bottomOfPrev + 1) exprViewInfo.y
                    in
                    ( { exprViewInfo | y = topOfThis } :: prevElems
                    , topOfThis + Expando.viewHeight Nothing exprViewInfo.ui
                    )
                )
                ([], 0)
            |> Tuple.first
            |> List.map viewExprUI
    in
    Html.div
        [ A.class "elm-reader-details" ]
        displayedExprs

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
