module Reader.TraceData
    exposing
        ( Expr
        , ExprWithContext
        , Frame(..)
        , FrameId(..)
        , InstrumentedFrameData
        , TraceData(..)
        , childFrames
        , decode
        , decodeFrame
        , exprChildFrame
        , exprValue
        , frameIdOf
        , frameIdToString
        , frameIdsEqual
        , isAncestorOf
        , isFrameInstrumented
        )

import Debug
import Json.Decode as JD
import Reader.Dict as Dict exposing (Dict)
import Reader.SourceMap as SourceMap exposing (SourceMap)
import Reader.TraceData.Value as Value exposing (Value)
import Tuple

import Elm.Kernel.Coerce


-- TRACE DATA


type TraceData
    = TraceData (List Frame)


decode : JD.Decoder TraceData
decode =
    JD.map TraceData <| JD.list decodeFrame



-- FRAME TRACES


type Frame
    = InstrumentedFrame InstrumentedFrameData
    | NonInstrumentedFrame FrameId (List Frame)
    | FrameThunk FrameId (() -> Frame)


type FrameId
    = -- uid: the unique ID of this runtime frame
      -- id path: the IDs of this frame's ancestors, from the root to this frame
      FrameId Int (List Int)


decodeFrameId : JD.Decoder FrameId
decodeFrameId =
    JD.map2 FrameId
        (JD.field "uid" JD.int)
        (JD.field "id_path" (JD.list JD.int))


frameIdToString : FrameId -> String
frameIdToString (FrameId uid ancestors) =
    "Frame "
        ++ String.fromInt uid
        ++ " (ancestors: ["
        ++ String.join ", " (List.map String.fromInt ancestors)
        ++ "])"


type alias InstrumentedFrameData =
    { sourceId : SourceMap.FrameId
    , runtimeId : FrameId
    , exprs : Dict SourceMap.ExprId Expr
    }


isFrameInstrumented : Frame -> Bool
isFrameInstrumented frame =
    case frame of
        InstrumentedFrame _ ->
            True

        NonInstrumentedFrame _ _ ->
            False

        FrameThunk _ _ ->
            False -- FIXME


frameIdOf : Frame -> FrameId
frameIdOf frame =
    case frame of
        NonInstrumentedFrame id _ ->
            id

        InstrumentedFrame { runtimeId } ->
            runtimeId

        FrameThunk id _ ->
            id



frameIdsEqual : FrameId -> FrameId -> Bool
frameIdsEqual (FrameId uid1 _) (FrameId uid2 _) =
    uid1 == uid2


{-| `isAncestorOf a b` answers whether b is an ancestor of a.

    List.filter (isAncestorOf child) frames

-}
isAncestorOf : FrameId -> FrameId -> Bool
isAncestorOf (FrameId _ ancestors) (FrameId possibleAncestor _) =
    List.member possibleAncestor ancestors


decodeFrame : JD.Decoder Frame
decodeFrame =
    let
        decFrame =
            JD.lazy (\() -> decodeFrame)

        decExpr =
            JD.lazy (\() -> decodeExpr)

        decodeInstrumented =
            JD.map3 (\sid rid exprs -> InstrumentedFrame (InstrumentedFrameData sid rid exprs))
                (JD.field "source_map_id" SourceMap.decodeFrameId)
                (JD.field "runtime_id" decodeFrameId)
                (JD.field "exprs" <| Dict.decode ( "id", SourceMap.decodeExprId ) ( "expr", decExpr ))

        decodeNonInstrumented =
            JD.map2 NonInstrumentedFrame
                (JD.field "runtime_id" decodeFrameId)
                (JD.field "child_frames" <| JD.list decFrame)

        decodeThunk =
            JD.map2 (\_ a -> a) (JD.field "is_thunk" JD.bool) <|
                (JD.map2 Tuple.pair JD.value (JD.field "runtime_id" decodeFrameId)
                    |> JD.map Elm.Kernel.Coerce.decodeFrameThunk)
    in
    JD.oneOf [ decodeNonInstrumented, decodeInstrumented, decodeThunk ]


childFrames : Frame -> List Frame
childFrames frameTrace =
    case frameTrace of
        InstrumentedFrame { exprs } ->
            Dict.values exprs
                |> List.map exprChildFrame
                |> List.filterMap identity

        NonInstrumentedFrame _ children ->
            children

        FrameThunk _ _ ->
            Debug.log "WARNING: childFrames got a FrameThunk" []



-- EXPRESSION TRACES


{-| -}
type Expr =
    -- childFrame is the frame that returned the value of this expression
    Expr { value : Maybe Value , childFrame : Maybe Frame }


exprValue : Expr -> Maybe Value
exprValue (Expr {value}) = value


exprChildFrame : Expr -> Maybe Frame
exprChildFrame (Expr {childFrame}) = childFrame


decodeExpr : JD.Decoder Expr
decodeExpr =
    JD.map2 (\val frame -> Expr {value = val, childFrame = frame})
        (JD.field "val" <| JD.nullable Value.decode)
        (JD.field "child_frame" <| JD.nullable decodeFrame)


type alias ExprWithContext =
    { frameSrcId : SourceMap.FrameId
    , stackFrameId : FrameId
    , exprId : SourceMap.ExprId
    , expr : Expr
    }
