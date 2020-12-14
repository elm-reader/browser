module Reader.StackUI.OpenFrame
    exposing
        ( OpenFrame(..)
        , fromTrace
        , sourceFrameIdOf
        , runtimeFrameIdOf
        , frameIdsOf
        , childFrameOf
        )


import Array exposing (Array)

import Reader.TraceData as TraceData
import Reader.SourceMap as SourceMap exposing (SourceMap)
import Reader.SourceMap.ExprDict as ExprDict exposing (ExprDict)


-- TODO: remove TraceData.InstrumentedFrameData's from
-- the OpenFrame type; replace with minimal used information,
-- i.e. frame runtime IDs and source IDs
type OpenFrame
    = Instrumented TraceData.InstrumentedFrameData
    | NonInstrumented
        TraceData.FrameId
        Int
        (Array TraceData.InstrumentedFrameData) -- child frames (in order of execution)


childFrameOf : OpenFrame -> Maybe TraceData.Frame
childFrameOf openFrame =
    case openFrame of
        Instrumented data ->
            Nothing

        NonInstrumented _ idx subframes ->
            Array.get idx subframes
            |> Maybe.map TraceData.Instrumented


sourceFrameIdOf : OpenFrame -> Maybe SourceMap.FrameId
sourceFrameIdOf openFrame =
    case openFrame of
        Instrumented { sourceId } ->
            Just sourceId

        NonInstrumented _ _ _ ->
            Nothing

runtimeFrameIdOf : OpenFrame -> TraceData.FrameId
runtimeFrameIdOf openFrame =
    case openFrame of
        Instrumented { runtimeId } ->
            runtimeId

        NonInstrumented id _ _ ->
            id

frameIdsOf : OpenFrame -> ( Maybe SourceMap.FrameId, TraceData.FrameId )
frameIdsOf openFrame =
    case openFrame of
        Instrumented { sourceId, runtimeId } ->
            ( Just sourceId, runtimeId )

        NonInstrumented id _ frames ->
            ( Nothing, id )


fromTrace : TraceData.Frame -> OpenFrame
fromTrace frame =
    case frame of
        TraceData.Instrumented data ->
            Instrumented data

        TraceData.NonInstrumented stackFrameId subframes ->
            NonInstrumented stackFrameId 0 (Array.fromList subframes)
