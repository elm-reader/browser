module Reader.TraceData.FrameDict exposing (FrameDict, empty, get, set, member, update, fromList)

import Reader.TraceData exposing (FrameId(..))
import Dict exposing (Dict)


type FrameDict v = FrameDict (Dict Int v)


empty : FrameDict a
empty = FrameDict Dict.empty


member : FrameId -> FrameDict a -> Bool
member (FrameId uniq _) (FrameDict dict) =
    Dict.member uniq dict


fromList : List ( FrameId, a ) -> FrameDict a
fromList lst =
    lst |> List.map (\((FrameId uniq _), val) -> (uniq, val)) |> Dict.fromList |> FrameDict


get : FrameId -> FrameDict a -> Maybe a
get (FrameId uniq _) (FrameDict dict) =
    Dict.get uniq dict


set : FrameId -> a -> FrameDict a -> FrameDict a
set (FrameId uniq _) val (FrameDict dict) =
    FrameDict (Dict.insert uniq val dict)


update : FrameId -> (a -> a) -> FrameDict a -> FrameDict a
update (FrameId uniq _) updater (FrameDict dict) =
    FrameDict (Dict.update uniq (Maybe.map updater) dict)
