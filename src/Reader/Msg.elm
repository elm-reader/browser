module Reader.Msg exposing (Msg(..))

import Reader.TraceData as TraceData exposing (TraceData)
import Reader.SourceMap as SourceMap exposing (SourceMap)


type Msg
    = NoOp
    | SelectTopLevelFrame TraceData.InstrumentedFrameData
    | HoverExpr TraceData.ExprWithContext
    | UnHoverExpr
    | OpenChildFrame TraceData.FrameId
