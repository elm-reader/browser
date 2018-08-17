module Reader.Msg exposing (Msg(..))

import Reader.TraceData as TraceData exposing (TraceData)


type Msg
    = NoOp
    | SelectTopLevelFrame TraceData.InstrumentedFrameData
    | HoverExpr TraceData.ExprWithContext
    | UnHoverExpr
    | OpenChildFrame TraceData.FrameId
