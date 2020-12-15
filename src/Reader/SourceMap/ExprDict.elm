module Reader.SourceMap.ExprDict exposing (ExprDict, empty, get, set, update, find, fromList, toList, unionMany, keys, size, map)

import Dict exposing (Dict)
import Reader.SourceMap.Ids exposing (ExprId(..))


type ExprDict v = ExprDict (Dict Int v)


size : ExprDict v -> Int
size (ExprDict dict) = Dict.size dict


map : (ExprId -> a -> b) -> ExprDict a -> ExprDict b
map func (ExprDict dict) =
    ExprDict <| Dict.map (ExprId >> func) dict


fromList : List ( ExprId, a ) -> ExprDict a
fromList lst =
    lst |> List.map (\((ExprId i), val) -> (i, val)) |> Dict.fromList |> ExprDict


toList : ExprDict a -> List ( ExprId, a )
toList (ExprDict dict) =
    Dict.toList dict |> List.map (\(i, val) -> (ExprId i, val))


empty : ExprDict a
empty = ExprDict Dict.empty


get : ExprId -> ExprDict a -> Maybe a
get (ExprId i) (ExprDict dict) =
    Dict.get i dict


set : ExprId -> a -> ExprDict a -> ExprDict a
set (ExprId i) a (ExprDict dict) =
    ExprDict (Dict.insert i a dict)


update : ExprId -> (a -> a) -> ExprDict a -> ExprDict a
update (ExprId i) updater (ExprDict dict) =
    ExprDict (Dict.update i (Maybe.map updater) dict)


find : (a -> Bool) -> ExprDict a -> Maybe ExprId
find condition (ExprDict dict) =
    Dict.foldl
        (\key val accum ->
            case accum of
                Just _ ->
                    accum

                Nothing ->
                    if condition val then
                        Just (ExprId key)
                    else
                        Nothing
        )
        Nothing
        dict


unionMany : List (ExprDict v) -> ExprDict v
unionMany dicts =
    ExprDict <|
        List.foldr
            (\(ExprDict dict) union -> Dict.union dict union)
            Dict.empty
            dicts

keys : ExprDict v -> List ExprId
keys (ExprDict dict) =
    Dict.keys dict |> List.map ExprId
