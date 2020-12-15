module Reader.Msg exposing (Msg(..))

import Reader.TraceData as TraceData exposing (TraceData)
import Reader.SourceMap as SourceMap exposing (SourceMap)

import Debugger.Expando as Expando


type Msg
    = NoOp
    | SelectTopLevelFrame TraceData.InstrumentedFrameData
    | OpenExpr TraceData.FrameId SourceMap.ExprId
    | PinExpr TraceData.FrameId SourceMap.ExprId
    | HoverExpr TraceData.FrameId SourceMap.ExprId
    | UnHoverExpr
    | OpenChildFrame TraceData.FrameId TraceData.InstrumentedFrameData
    | ExprUIMsg TraceData.FrameId SourceMap.ExprId Expando.Msg
