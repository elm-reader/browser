module Reader.FrameUI
    exposing
        ( FrameUI
        , view
        , fromTrace
        )

{-| Reader.FrameUI manages the UI of a single stack frame.
-}


import Html as H exposing (Html)
import Html.Attributes as A
import Reader.Dict as Dict exposing (Dict)
import Reader.FrameUI.Instrumented as Instrumented
import Reader.Msg as Msg exposing (Msg)
import Reader.SourceMap as SourceMap exposing (SourceMap)
import Reader.TraceData as TraceData exposing (TraceData)


-- MODEL


type alias FrameUI =
    { frame : TraceData.Frame
    , openedChild : Maybe TraceData.FrameId
    }


fromTrace : TraceData.Frame -> FrameUI
fromTrace trace =
   FrameUI trace Nothing


-- VIEW


view :
   SourceMap
   -> Maybe TraceData.ExprWithContext
   -> FrameUI
   -> Html Msg
view srcMap hoveredExpr frameUI =
    case frameUI.frame of
        (TraceData.NonInstrumentedFrame runtimeId childFrames) as frame ->
            frameContainerElem
                [ H.text ("Non-instrumented frame (" ++ TraceData.frameIdToString runtimeId ++ ").")
                , viewNonInstrumented srcMap childFrames hoveredExpr
                ]

        TraceData.InstrumentedFrame tracedFrame ->
            let
                maybeFrame =
                    Dict.lookup tracedFrame.sourceId srcMap.frames

                maybeRegionSource =
                    maybeFrame
                        |> Maybe.map .region
                        |> Maybe.andThen (\region -> SourceMap.lookupRegionSource region srcMap.sources)
            in
            case Maybe.map2 Tuple.pair maybeFrame maybeRegionSource of
                Just ( frame, src ) ->
                    let
                        content =
                            case Instrumented.viewFrameTrace srcMap hoveredExpr frameUI.openedChild tracedFrame of
                                Err e ->
                                    H.div []
                                        [ H.p [ A.style "font-weight" "bold" ] [ H.text "Got error:" ]
                                        , fmtErr e
                                        , H.p [ A.style "font-weight" "bold" ] [ H.text "Parsing this frame's trace:" ]
                                        , H.pre [] [ H.text src ]
                                        ]

                                Ok frameTraceHtml ->
                                    frameTraceHtml

                        headerMsg =
                            H.p []
                                [ H.text "Instrumented frame (sourceId: "
                                , H.code [] [ H.text (SourceMap.frameIdToString tracedFrame.sourceId) ]
                                , H.text ", runtimeId: "
                                , H.code [] [ H.text (TraceData.frameIdToString tracedFrame.runtimeId) ]
                                , H.text ")"
                                ]
                    in
                    frameContainerElem <|
                        [ headerMsg
                        , content
                        ]

                Nothing ->
                    let
                        errMsg =
                            "failed to find frame region! id: "
                                ++ Debug.toString tracedFrame.sourceId
                                ++ "\nsrcMap: "
                                ++ Debug.toString srcMap
                    in
                    frameContainerElem [ H.pre [] [ H.text errMsg ] ]


fmtErr : String -> Html msg
fmtErr msg =
    H.pre [ A.style "white-space" "pre-wrap" ] [ H.text msg ]


viewNonInstrumented : SourceMap -> List TraceData.Frame -> Maybe TraceData.ExprWithContext -> Html Msg
viewNonInstrumented srcMap childFrames hoveredExprId =
    let
        intoListItem elem =
            H.li [] [ elem ]

        children =
            List.map
                (fromTrace >> view srcMap hoveredExprId >> intoListItem)
                childFrames
    in
    if children == [] then
        H.text "No child frames"
    else
        H.div []
            [ H.text "Child frames:"
            , H.ul [] children
            ]


frameContainerElem : List (Html msg) -> Html msg
frameContainerElem items =
    H.div
        [ A.style "border-left" "1px solid green"
        , A.style "padding-left" "3px"
        , A.style "margin-bottom" "6px"
        ]
        items
