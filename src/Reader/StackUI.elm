module Reader.StackUI exposing (StackUI, fromTrace, handleExprClick, handleExprHover, handleExprUnHover, viewStackUI)

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
            , sourceId : SourceMap.FrameId
            , topY : Int
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
                                            , { line = start.line
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


fromTrace : SourceMap -> TraceData.Frame -> StackUI
fromTrace srcMap frameTrace =
    let
        openFrame = OpenFrame.fromTrace frameTrace

        ( stackFrames, sourceFrameIds ) =
            case frameTrace of
                TraceData.Instrumented data ->
                    let
                        ( liveExprs, deadExprs ) = exprsFromTrace srcMap data
                    in
                    ( FrameDict.fromList
                        [ ( data.runtimeId
                          , { topY = 1
                            , sourceId = data.sourceId
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
                                in
                                ( frame.runtimeId, { topY = 1, sourceId = frame.sourceId, exprs = liveExprs, deadExprs = deadExprs } )
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
    { stackTree = StackTree.singleton openFrame
    , renderedFrames = RenderedFrameMap.ensure srcMap sourceFrameIds RenderedFrameMap.empty
    , stackFrames = stackFrames
    , previewedExpr = Nothing
    }



frameLineSpacing : Int
frameLineSpacing = 2


frameLinePadding : Float
frameLinePadding = 0.5


openChildFrame : SourceMap -> StackUI -> TraceData.FrameId -> SourceMap.ExprId -> TraceData.FrameThunk -> StackUI
openChildFrame srcMap stackUI parentFrameId exprId frameThunk =
    let
        evaluated =
            case frameThunk of
                TraceData.Thunk runtimeId thunk ->
                    thunk ()

                TraceData.Evaluated frame ->
                    frame

        openFrame = OpenFrame.fromTrace evaluated

        ensureChildFrameData stackFrames =
            case OpenFrame.getData openFrame of
                Just data ->
                    if FrameDict.member data.runtimeId stackFrames then
                        stackFrames
                    else
                        -- Initialize the frame data
                        case FrameDict.get parentFrameId stackFrames of
                            Just parentFrame ->
                                case RenderedFrameMap.get parentFrame.sourceId stackUI.renderedFrames of
                                    Just { lines } ->
                                        let
                                            ( liveExprs, deadExprs ) = exprsFromTrace srcMap data
                                        in
                                        FrameDict.set
                                            data.runtimeId
                                            { topY = parentFrame.topY + lines + frameLineSpacing
                                            , sourceId = data.sourceId
                                            , exprs = liveExprs
                                            , deadExprs = deadExprs
                                            }
                                            stackFrames

                                    Nothing ->
                                        stackFrames -- unreachable so long as renderedFrames is properly maintained

                            Nothing ->
                                stackFrames -- unreachable so long as stackFrames is properly maintained

                Nothing ->
                    stackFrames -- unreachable (so long as openFrame is a valid OpenFrame)
    in
    { stackUI
        | stackTree = StackTree.openChildFrame parentFrameId openFrame stackUI.stackTree
        , stackFrames =
            let
                setExprChildFrameCache expr = { expr | childFrame = Just (TraceData.Evaluated evaluated) }
            in
            stackUI.stackFrames
                |> FrameDict.update
                    parentFrameId
                    (\parentFrameData ->
                        let newExprs = ExprDict.update exprId setExprChildFrameCache parentFrameData.exprs in
                        { parentFrameData | exprs = newExprs })
                |> ensureChildFrameData
        , renderedFrames =
            case OpenFrame.frameIdsOf openFrame of
                Just ( sourceId, _ ) ->
                    RenderedFrameMap.ensure srcMap [ sourceId ] stackUI.renderedFrames

                Nothing ->
                    stackUI.renderedFrames -- unreachable
        }


handleExprClick : StackUI -> SourceMap -> TraceData.FrameId -> SourceMap.ExprId -> StackUI
handleExprClick stackUI srcMap frameRuntimeId exprId =
    let
        expr =
            FrameDict.get frameRuntimeId stackUI.stackFrames
            |> Maybe.map .exprs
            |> Maybe.andThen (ExprDict.get exprId)
    in
    case expr |> Maybe.andThen .childFrame of
        Just childFrame ->
            openChildFrame srcMap stackUI frameRuntimeId exprId childFrame

        Nothing ->
            stackUI


handleExprHover : StackUI -> TraceData.FrameId -> SourceMap.ExprId -> StackUI
handleExprHover stackUI frame expr =
    let
        exprValue =
            FrameDict.get frame stackUI.stackFrames
            |> Maybe.map .exprs
            |> Maybe.andThen (ExprDict.get expr)
    in
    { stackUI | previewedExpr = Maybe.map (\_ -> (frame, expr)) exprValue }


handleExprUnHover : StackUI -> StackUI
handleExprUnHover stackUI =
    { stackUI | previewedExpr = Nothing }


-- VIEW

previewedExprCSSRule = """ {
  border-radius: 3px;
  background-color: rgb(230, 230, 200);
}
"""

deadExprsCSSRule = """ {
  color: #bbb;
  background-color: none !important;
}
"""

functionCallExprsCSSRule = """ {
    cursor: pointer;
}
"""

openFunctionCallExprsCSSRule = """ {
  border-radius: 3px;
  background-color: rgb(255,255,250);
  box-shadow: 0px 0px 1px 1px #ddc;
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

        functionCallExprs =
            StackTree.map
                (\openFrame ->
                    case OpenFrame.frameIdsOf openFrame of
                        Just ( _, runtimeId ) ->
                            case FrameDict.get runtimeId stackUI.stackFrames of
                                Just { exprs } ->
                                    ExprDict.toList exprs
                                    |> List.filterMap
                                        (\(exprId, expr) ->
                                            Maybe.map (\_ -> ( runtimeId, exprId )) expr.childFrame
                                        )

                                Nothing ->
                                    []
                        Nothing ->
                            []
                )
                stackUI.stackTree
            |> List.concatMap (\x -> x)


        displayedDeadExprs =
            StackTree.map
                (\openFrame ->
                    case OpenFrame.frameIdsOf openFrame of
                        Just ( _, runtimeId ) ->
                            case FrameDict.get runtimeId stackUI.stackFrames of
                                Just { deadExprs } ->
                                    Just ( runtimeId, deadExprs )

                                Nothing ->
                                    Nothing

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


onMouseOver =
    E.on "mouseover" <|
        JD.andThen Elm.Kernel.Reader.mouseEventToMessage JD.value


onClick =
    E.on "click" <|
        JD.andThen Elm.Kernel.Reader.mouseEventToMessage JD.value


viewStackUI : StackUI -> Html Msg
viewStackUI stackUI =
    -- TODO: expression view
    Html.div
        []
        [ Html.div
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
    case OpenFrame.frameIdsOf openFrame of
        Just ( sourceId, ((TraceData.FrameId numRuntimeId _) as runtimeId) ) ->
            let
                frameSource = RenderedFrameMap.get sourceId stackUI.renderedFrames

                frameExprData = FrameDict.get runtimeId stackUI.stackFrames
            in
            case ( frameSource, frameExprData ) of
                ( Just { html, lines }, Just { topY } ) ->
                    Html.div
                        [ A.class "elm-reader-frame"
                        , A.class ("elm-reader-frame-" ++ String.fromInt numRuntimeId)
                        , A.style "top" (String.fromFloat (toFloat topY - frameLinePadding) ++ "em")
                        , A.style "height" (String.fromInt lines ++ "em")
                        ]
                        [ html ]

                _ ->
                    Html.div
                        [ A.class "elm-reader-frame"
                        , A.class ("elm-reader-frame-" ++ String.fromInt numRuntimeId)
                        ]
                        [ Html.text "Frame failed to render!" ]

        Nothing ->
            Html.div [ A.class "elm-reader-frame" ] [ Html.text "Failed to render frame!" ]
