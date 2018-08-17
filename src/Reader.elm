module Reader
    exposing
        ( Mode(..)
        , Model
        , Msg
        , markInstrumented
        , parseConfig
        , recordCall
        , recordExpr
        , recordFrame
        , seq
        , update
        , updateExec
        , view
        )

{-| Reader.


# Hooks

@docs recordExpr, recordCall, recordFrame, markInstrumented, seq


# Kernel util

@docs parseConfig, updateExec


# Reader application

@docs Model, Mode, Msg, update, view

-}

import Debug exposing (toString)
import Elm.Kernel.Reader
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


-- HOOKS


{-| -}
recordExpr : Int -> a -> a
recordExpr =
    Elm.Kernel.Reader.recordExpr


{-| -}
recordCall : Int -> (a -> b) -> (() -> c) -> c
recordCall =
    Elm.Kernel.Reader.recordCall


{-| -}
recordFrame : String -> (() -> a) -> a
recordFrame =
    Elm.Kernel.Reader.recordFrame


{-| -}
markInstrumented : (a -> b) -> (a -> b)
markInstrumented =
    Elm.Kernel.Reader.markInstrumented


{-| -}
seq : a -> b -> b
seq =
    Elm.Kernel.Reader.seq



-- KERNEL UTIL


{-| parseConfig is used by Reader.js to initialize the model from
the JSON containing source map and tracing data.
-}
parseConfig : Mode -> String -> Model
parseConfig mode data =
    let
        decode =
            JD.map2 (\src traces -> { sources = src, traces = traces })
                (JD.field "source_map" SourceMap.decode)
                (JD.field "traces" TraceData.decode)
    in
    case JD.decodeString decode data of
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
            in
            ProgramDataReceived
                { sources = sources
                , traces = traces
                , hoveredExpr = Nothing
                , selectedFrames = selectedFrames
                , mode = mode
                }


{-| -}
updateExec : (msg -> model -> ( model, a )) -> msg -> model -> ( model, Model )
updateExec =
    Elm.Kernel.Reader.updateExec




-- MODEL


{-| Model for Reader application. Considers case where initialization data
is broken.
-}
type Model
    = ProgramDataError JD.Error
    | ProgramDataReceived ModelAfterInit


type alias ModelAfterInit =
    { sources : SourceMap
    , traces : TraceData
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
            let
                newData =
                    updateAfterInit msg data
            in
            ( ProgramDataReceived newData
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
                    { model | selectedFrames = Maybe.map (SelectedFrameTree.openFrame childFrameId) model.selectedFrames }



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
viewAfterInit { sources, traces, hoveredExpr, selectedFrames, mode } =
    let
        outlineSidebar =
            case mode of
                ModeBrowse ->
                    [ viewOutlineSidebar (A.style "flex" "2") traces ]

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


viewOutlineSidebar : Attribute Msg -> TraceData -> Html Msg
viewOutlineSidebar width (TraceData frames) =
    let
        instrumented =
            frames
                |> List.filterMap
                    (\f ->
                        case f of
                            TraceData.InstrumentedFrame data ->
                                Just data

                            TraceData.NonInstrumentedFrame _ _ ->
                                Nothing
                    )

        numNonInstrumented =
            List.length frames - List.length instrumented

        viewFrameLink frame =
            a
                [ A.href "#"
                , E.onClick (Msg.SelectTopLevelFrame frame)
                ]
                [ text (SourceMap.frameIdToString frame.sourceId) ]
    in
    Flex.columnWith [ width ] <|
        List.map viewFrameLink instrumented
            ++ [ text (String.fromInt numNonInstrumented ++ " noninstrumented frames")
               ]


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
            case expr.value of
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
            container [ text "Select a frame to view on the left" ]

        Just selFrame ->
            let
                childTraces =
                    selFrame
                        |> SelectedFrameTree.getOpenFrames
                        |> List.map (FrameUI.view sources hoveredExpr)
            in
            container childTraces
