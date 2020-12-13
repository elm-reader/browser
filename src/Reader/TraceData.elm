module Reader.TraceData
    exposing
        ( Expr
        , Frame(..)
        , FrameThunk(..)
        , FrameId(..)
        , parentFrameId
        , InstrumentedFrameData
        , TraceData(..)
        , decode
        , decodeFrame
        , evaluate
        , exprChildFrame
        , exprValue
        , frameIdOf
        , frameIdOfThunk
        , frameIdToString
        , frameIdsEqual
        , isAncestorOf
        )

import Debug
import Json.Decode as JD
import Reader.Dict as Dict exposing (Dict)
import Reader.SourceMap as SourceMap exposing (SourceMap)
import Reader.SourceMap.Ids as SourceMapIds
import Reader.SourceMap.ExprDict as ExprDict exposing (ExprDict)
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


type FrameThunk = Thunk FrameId (() -> Frame) | Evaluated Frame


type Frame
    = Instrumented InstrumentedFrameData
    | NonInstrumented FrameId (List InstrumentedFrameData)


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

parentFrameId : FrameId -> Maybe FrameId
parentFrameId (FrameId me ancestors) =
    case ancestors of
        [] ->
            Nothing

        parent :: rest ->
            Just (FrameId parent rest)


type alias InstrumentedFrameData =
    { sourceId : SourceMap.FrameId
    , runtimeId : FrameId
    , exprs : ExprDict Expr
    }


frameIdOf : Frame -> FrameId
frameIdOf frame =
    case frame of
        NonInstrumented id _ ->
            id

        Instrumented { runtimeId } ->
            runtimeId


frameIdsEqual : FrameId -> FrameId -> Bool
frameIdsEqual (FrameId uid1 _) (FrameId uid2 _) =
    uid1 == uid2


frameIdOfThunk : FrameThunk -> FrameId
frameIdOfThunk frame =
    case frame of
        Evaluated f ->
            frameIdOf f

        Thunk id _ ->
            id


evaluate : FrameThunk -> Frame
evaluate frameThunk =
    case frameThunk of
        Thunk runtimeId thunk ->
            thunk ()

        Evaluated frame ->
            frame

{-| `isAncestorOf a b` answers whether b is an ancestor of a.

    List.filter (isAncestorOf child) frames

-}
isAncestorOf : FrameId -> FrameId -> Bool
isAncestorOf (FrameId _ ancestors) (FrameId possibleAncestor _) =
    List.member possibleAncestor ancestors


decodeFrame : JD.Decoder Frame
decodeFrame =
    let
        decExpr =
            JD.lazy (\() -> decodeExpr)

        decodeExprDict =
            Dict.decode ( "id", SourceMapIds.decodeExprId ) ( "expr", decExpr )
            |> JD.map Dict.toList
            |> JD.map ExprDict.fromList

        decodeInstrumentedFrameData =
            JD.map3 (\sid rid exprs -> InstrumentedFrameData sid rid exprs)
                (JD.field "source_map_id" SourceMapIds.decodeFrameId)
                (JD.field "runtime_id" decodeFrameId)
                (JD.field "exprs" decodeExprDict)

        decodeInstrumented =
            JD.map Instrumented decodeInstrumentedFrameData

        decodeNonInstrumented =
            JD.map2 NonInstrumented
                (JD.field "runtime_id" decodeFrameId)
                (JD.field "child_frames" <| JD.list decodeInstrumentedFrameData)
    in
    JD.oneOf [ decodeNonInstrumented, decodeInstrumented ]


decodeThunk : JD.Decoder FrameThunk
decodeThunk =
    let
        evaluated =
            JD.map Evaluated decodeFrame

        unevaluated =
            JD.map2 (\_ a -> a) (JD.field "is_thunk" JD.bool) <|
                (JD.map2 Tuple.pair JD.value (JD.field "runtime_id" decodeFrameId)
                    |> JD.map Elm.Kernel.Coerce.decodeFrameThunk)
    in
    JD.oneOf [ evaluated, unevaluated ]



-- EXPRESSION TRACES


{-| -}
type Expr =
    -- childFrame is the frame that returned the value of this expression
    Expr { value : Maybe Value , childFrame : Maybe FrameThunk }


exprValue : Expr -> Maybe Value
exprValue (Expr {value}) = value


exprChildFrame : Expr -> Maybe FrameThunk
exprChildFrame (Expr {childFrame}) = childFrame


decodeExpr : JD.Decoder Expr
decodeExpr =
    JD.map2 (\val frame -> Expr {value = val, childFrame = frame})
        (JD.field "val" <| JD.nullable Value.decode)
        (JD.field "child_frame" <| JD.nullable decodeThunk)
