module Reader
    exposing
        ( Mode(..)
        , Model
        , Msg
        , parseConfig
        , update
        , updateExec
        , view
        )

{-| Reader.


# Kernel util

@docs parseConfig, updateExec


# Reader application

@docs Model, Mode, Msg, update, view

-}

import Debug exposing (toString, log)
import Reader.Hooks
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as JD
import Reader.Dict as Dict exposing (Dict)
import Reader.Flex as Flex
import Reader.FrameUI as FrameUI
import Reader.Msg as Msg
import Reader.SelectedFrameTree as SelectedFrameTree exposing (SelectedFrameTree)
import Reader.SourceMap as SourceMap exposing (SourceMap)
import Reader.TraceData as TraceData exposing (TraceData(..))
import Reader.TraceData.Value as Value exposing (Value)
import Elm.Kernel.Reader


-- KERNEL UTIL

parseSourceMap : JD.Value -> Result JD.Error SourceMap
parseSourceMap = JD.decodeValue SourceMap.decode


{-| parseConfig is used by Reader.js to initialize the model from
the JSON containing source map and tracing data.
-}
parseConfig : Mode -> SourceMap -> JD.Value -> Model
parseConfig mode srcMap data =
    let
        decode =
            JD.map (\traces -> { sources = srcMap, traces = traces })
                (JD.field "traces" TraceData.decode)
    in
    case JD.decodeValue decode data of
        Err e ->
            ProgramDataError e

        Ok { sources, traces } ->
            let
                selectedFrames =
                    case traces of
                        TraceData (frameTrace :: _) ->
                            Just (SelectedFrameTree.fromTrace frameTrace)

                        TraceData [] ->
                            Nothing
                tracesOutline =
                    let
                        (TraceData topLevelFrames) = traces
                        instrumentedFrameTraces =
                            topLevelFrames
                                |> List.filterMap
                                    (\f ->
                                        case f of
                                            TraceData.InstrumentedFrame frameData ->
                                                Just frameData

                                            TraceData.NonInstrumentedFrame _ _ ->
                                                Nothing

                                            TraceData.FrameThunk _ _ ->
                                                -- FIXME
                                                Debug.log "got FrameThunk (in Reader.elm:tracesOutline)!" Nothing
                                    )
                    in
                    { topLevelInstrumented = instrumentedFrameTraces
                    , numNoninstrumented =
                        List.length topLevelFrames - List.length instrumentedFrameTraces
                    }
            in
            ProgramDataReceived
                { sources = sources
                , tracesOutline = tracesOutline
                , hoveredExpr = Nothing
                , selectedFrames = selectedFrames
                , mode = mode
                }

parseFrame : JD.Value -> TraceData.Frame
parseFrame data =
    case JD.decodeValue TraceData.decodeFrame data of
        Err e ->
            Debug.todo ("Reader.parseFrame: failed to decode frame trace: " ++ JD.errorToString e)

        Ok frameTrace ->
            frameTrace


{-| -}
updateExec : (msg -> model -> ( model, a )) -> msg -> model -> ( model, Model )
updateExec =
    Reader.Hooks.updateExec




-- MODEL


{-| Model for Reader application. Considers case where initialization data
is broken.
-}
type Model
    = ProgramDataError JD.Error
    | ProgramDataReceived ModelAfterInit


type alias TracesOutline =
    { numNoninstrumented : Int
    , topLevelInstrumented : List TraceData.InstrumentedFrameData
    }


type alias ModelAfterInit =
    { sources : SourceMap
    , tracesOutline : TracesOutline
    , hoveredExpr : Maybe TraceData.ExprWithContext
    , selectedFrames : Maybe SelectedFrameTree
    , mode : Mode
    }


{-| Mode -- ModeBrowse for browsing through code / examples (using the --reader
option) or ModeDebug for the integrated debugger view.
-}
type Mode
    = ModeBrowse
    | ModeDebug



-- UPDATE


{-| The Msg type for the Reader application.
-}
type alias Msg =
    Msg.Msg


{-| update is the update for the Reader application. Considers
case where initialization data is broken.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        ProgramDataError e ->
            ( ProgramDataError e
            , Cmd.none
            )

        ProgramDataReceived data ->
            ( ProgramDataReceived (updateAfterInit msg data)
            , Cmd.none
            )


updateAfterInit : Msg -> ModelAfterInit -> ModelAfterInit
updateAfterInit msg model =
    case msg of
        Msg.NoOp ->
            model

        Msg.HoverExpr exprData ->
            { model | hoveredExpr = Just exprData }

        Msg.UnHoverExpr ->
            { model | hoveredExpr = Nothing }

        Msg.SelectTopLevelFrame frameTrace ->
            let
                selectedFrameTree =
                    SelectedFrameTree.fromTrace (TraceData.InstrumentedFrame frameTrace)
            in
            { model | selectedFrames = Just selectedFrameTree }

        Msg.OpenChildFrame childFrameId ->
            case model.selectedFrames of
                Nothing ->
                    Debug.log "Unexpected OpenChildFrame msg!" model

                Just topLevelSelectedFrame ->
                    { model |
                        selectedFrames =
                            Maybe.map
                                (SelectedFrameTree.openFrame childFrameId model.sources)
                                model.selectedFrames
                    }



-- VIEW


{-| -}
view : Model -> Html Msg
view generalModel =
    case generalModel of
        ProgramDataReceived model ->
            viewAfterInit model

        ProgramDataError err ->
            div []
                [ pre [] [ text <| JD.errorToString err ]
                ]


viewAfterInit : ModelAfterInit -> Html Msg
viewAfterInit { sources, tracesOutline, hoveredExpr, selectedFrames, mode } =
    let
        outlineSidebar =
            case mode of
                ModeBrowse ->
                    [ viewOutlineSidebar (A.style "flex" "2") tracesOutline ]

                ModeDebug ->
                    []

        heading rest =
            case mode of
                ModeBrowse ->
                    Flex.column
                        [ h1 [] [ text "Elm Reader" ]
                        , rest
                        ]

                ModeDebug ->
                    rest
    in
    heading <|
        Flex.rowWith
            [ A.style "padding-bottom" "40px"
            ]
            (outlineSidebar
                ++ [ viewTraceWindow (A.style "flex" "5") sources hoveredExpr selectedFrames
                   , viewDetailsSidebar [ A.style "flex" "3" ] hoveredExpr
                   ]
            )


viewOutlineSidebar : Attribute Msg -> TracesOutline -> Html Msg
viewOutlineSidebar width {numNoninstrumented, topLevelInstrumented} =
    let
        viewFrameLink frame =
            a
                [ A.href "#"
                , E.onClick (Msg.SelectTopLevelFrame frame)
                ]
                [ text (SourceMap.frameIdToString frame.sourceId) ]
    in
    Flex.columnWith [ width ] <|
        List.map viewFrameLink topLevelInstrumented
            ++ [ text (String.fromInt numNoninstrumented ++ " noninstrumented frames") ]


viewDetailsSidebar : List (Attribute Msg) -> Maybe TraceData.ExprWithContext -> Html Msg
viewDetailsSidebar layout maybeExpr =
    let
        container =
            div layout
    in
    case maybeExpr of
        Nothing ->
            container [ text "No expression selected" ]

        Just { frameSrcId, exprId, expr } ->
            case TraceData.exprValue expr of
                Nothing ->
                    container [ text "Expression has no (recorded) value" ]

                Just val ->
                    container [ text (Value.toString val) ]


viewTraceWindow :
    Attribute Msg
    -> SourceMap
    -> Maybe TraceData.ExprWithContext
    -> Maybe SelectedFrameTree
    -> Html Msg
viewTraceWindow width sources hoveredExpr maybeTrace =
    let
        container =
            div [ width ]
    in
    case maybeTrace of
        Nothing ->
            container [ text (Elm.Kernel.Reader.log "in Nothing branch" "Select a frame to view on the left") ]

        Just selFrame ->
            let
                childTraces =
                    selFrame
                        |> SelectedFrameTree.getOpenFrames
                        |> List.map (FrameUI.view sources hoveredExpr)
            in
            container childTraces
