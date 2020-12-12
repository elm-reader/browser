module Reader.StackUI.OpenFrame
    exposing
        ( OpenFrame(..)
        , fromTrace
        , frameIdsOf
        , getData
        )


import Array exposing (Array)

import Reader.TraceData as TraceData
import Reader.SourceMap as SourceMap exposing (SourceMap)
import Reader.SourceMap.ExprDict as ExprDict exposing (ExprDict)


type OpenFrame
    = Instrumented TraceData.InstrumentedFrameData
    | NonInstrumented
        TraceData.FrameId
        Int -- index of currently open frame in the following array
        (Array TraceData.InstrumentedFrameData) -- child frames (in order of execution)


{-| `frameIdsOf` should never return `Nothing` on valid input, i.e. as long
as the `OpenFrame` is not `NonInstrumented _ n arr` where `n < 0` or
`n >= Array.length arr`, the function `frameIdsOf` returns `Just ...` -}
frameIdsOf : OpenFrame -> Maybe ( SourceMap.FrameId, TraceData.FrameId )
frameIdsOf openFrame =
    case openFrame of
        Instrumented { sourceId, runtimeId } ->
            Just ( sourceId, runtimeId )

        NonInstrumented _ idx frames ->
            Array.get idx frames
            |> Maybe.map (\{ sourceId, runtimeId } -> ( sourceId, runtimeId ))


getData : OpenFrame -> Maybe TraceData.InstrumentedFrameData
getData openFrame =
    case openFrame of
        Instrumented data ->
            Just data

        NonInstrumented _ i datas ->
            Array.get i datas


exprFromChildFrame : TraceData.FrameId -> OpenFrame -> Maybe SourceMap.ExprId
exprFromChildFrame childFrameId openFrame =
    case getData openFrame of
        Just data ->
            ExprDict.find
                (\expr ->
                    case TraceData.exprChildFrame expr of
                        Just childFrame ->
                            TraceData.frameIdsEqual childFrameId (TraceData.frameIdOfThunk childFrame)

                        Nothing ->
                            False)
                data.exprs

        Nothing ->
            Nothing -- unreachable if openFrame is valid


fromTrace : TraceData.Frame -> OpenFrame
fromTrace frame =
    case frame of
        TraceData.Instrumented data ->
            Instrumented data

        TraceData.NonInstrumented stackFrameId subframes ->
            NonInstrumented stackFrameId 0 (Array.fromList subframes)
