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
import Reader.StackUI as StackUI exposing (StackUI)
import Reader.Msg as Msg
import Reader.SourceMap as SourceMap exposing (SourceMap)
import Reader.SourceMap.Ids as SourceMapIds
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
                stackUI =
                    case traces of
                        TraceData (frameTrace :: _) ->
                            StackUI.fromTrace sources frameTrace

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
                                            TraceData.Instrumented frameData ->
                                                Just frameData

                                            TraceData.NonInstrumented _ _ ->
                                                Nothing
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
                , stackUI = stackUI
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
    , stackUI : Maybe StackUI
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

map1_3 : (a -> b -> c -> d) -> (Maybe a -> b -> c -> Maybe d)
map1_3 f maybeA b c =
    case maybeA of
        Just a ->
            Just (f a b c)

        Nothing ->
            Nothing

map1_4 : (a -> b -> c -> d -> e) -> (Maybe a -> b -> c -> d -> Maybe e)
map1_4 f maybeA b c d =
    case maybeA of
        Just a ->
            Just (f a b c d)

        Nothing ->
            Nothing

updateAfterInit : Msg -> ModelAfterInit -> ModelAfterInit
updateAfterInit msg model =
    case msg of
        Msg.NoOp ->
            model

        Msg.HoverExpr frameId exprId ->
            { model | stackUI = (map1_3 StackUI.handleExprHover) model.stackUI frameId exprId }

        Msg.UnHoverExpr ->
            { model | stackUI = Maybe.map StackUI.handleExprUnHover model.stackUI }

        Msg.ClickExpr frameId exprId ->
            { model | stackUI = (map1_4 StackUI.handleExprClick) model.stackUI model.sources frameId exprId }

        Msg.OpenChildFrame parentFrameId childFrame ->
            { model | stackUI = (map1_4 StackUI.handleOpenChildFrame) model.stackUI model.sources parentFrameId childFrame }

        Msg.SelectTopLevelFrame frameTrace ->
            { model | stackUI = StackUI.fromTrace model.sources (TraceData.Instrumented frameTrace) }



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
viewAfterInit { sources, tracesOutline, stackUI, mode } =
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
        Html.div
            []
            (outlineSidebar ++
                case stackUI of
                    Just stackUI_ ->
                        [ StackUI.viewStackUI stackUI_ ]

                    Nothing ->
                        [])


viewOutlineSidebar : Attribute Msg -> TracesOutline -> Html Msg
viewOutlineSidebar width {numNoninstrumented, topLevelInstrumented} =
    let
        viewFrameLink frame =
            a
                [ A.href "#"
                , E.onClick (Msg.SelectTopLevelFrame frame)
                ]
                [ text (SourceMapIds.frameIdToString frame.sourceId) ]
    in
    Flex.columnWith [ width ] <|
        List.map viewFrameLink topLevelInstrumented
            ++ [ text (String.fromInt numNoninstrumented ++ " noninstrumented frames") ]
