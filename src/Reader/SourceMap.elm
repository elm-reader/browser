module Reader.SourceMap
    exposing
        ( ExprId
        , FrameId
        , Frame
        , getFrame
        , getFrameHeight
        , Position
        , Region
        , SourceMap
        , comparePositions
        , decode
        , decodeRegion
        , exprsEndingAt
        , exprsStartingAt
        , lookupRegionSource
        )

import Json.Decode as JD
import Reader.Dict
import Dict exposing (Dict)
import Reader.SourceMap.ExprDict as ExprDict exposing (ExprDict)
import Reader.SourceMap.Ids as Ids


-- SOURCE MAPS


type alias SourceMap =
    { frames : Reader.Dict.Dict Ids.FrameId Frame
    , sources : Reader.Dict.Dict Ids.ModuleId String
    }


emptySourceMap =
    SourceMap Reader.Dict.empty Reader.Dict.empty


decode : JD.Decoder SourceMap
decode =
    JD.map2 SourceMap
        (JD.field "frames" <| Reader.Dict.decode ( "id", Ids.decodeFrameId ) ( "frame", decodeFrame ))
        (JD.field "sources" <| Reader.Dict.decode ( "module", Ids.decodeModuleId ) ( "source", JD.string ))


getFrame : Ids.FrameId -> SourceMap -> Maybe Frame
getFrame frameId { frames } =
    Reader.Dict.get frameId frames


getFrameHeight : Ids.FrameId -> SourceMap -> Maybe Int
getFrameHeight id sourceMap =
    getFrame id sourceMap
        |> Maybe.map (\{ region } -> region.end.line - region.start.line + 1)



-- FRAME


type alias Frame =
    { region : Region
    , exprRegions : ExprDict (List Region)
    , exprNames : Reader.Dict.Dict Ids.ExprId ( Ids.ModuleId, String )
    }


decodeExprRegions : JD.Decoder (ExprDict (List Region))
decodeExprRegions =
    let
        decodeEntry =
            JD.map2 Tuple.pair
                (JD.field "id" (JD.map Ids.ExprId JD.int))
                (JD.field "regions" <| JD.list decodeRegion)
    in
    JD.list decodeEntry
        |> JD.map ExprDict.fromList


decodeFrame : JD.Decoder Frame
decodeFrame =
    let
        decodeExprName =
            JD.map2 Tuple.pair
                (JD.field "module" Ids.decodeModuleId)
                (JD.field "name" JD.string)
    in
    JD.map3 Frame
        (JD.field "region" decodeRegion)
        (JD.field "expr_regions" decodeExprRegions)
        (JD.field "expr_names" <| Reader.Dict.decode ( "id", Ids.decodeExprId ) ( "qualified_name", decodeExprName ))



-- REGION


type alias Region =
    { mod : Ids.ModuleId
    , start : Position
    , end : Position
    }


decodeRegion : JD.Decoder Region
decodeRegion =
    JD.map3 Region
        (JD.field "module" Ids.decodeModuleId)
        (JD.field "start" decodePosition)
        (JD.field "end" decodePosition)


type alias Position =
    { line : Int
    , col : Int
    }


decodePosition : JD.Decoder Position
decodePosition =
    JD.map2 Position
        (JD.field "line" JD.int)
        (JD.field "column" JD.int)


comparePositions : Position -> Position -> Order
comparePositions p1 p2 =
    if p1.line == p2.line then
        compare p1.col p2.col
    else
        compare p1.line p2.line



-- POSITION/REGION LOOKUP UTILS


isPositionInRegion : Position -> Region -> Bool
isPositionInRegion pos { start, end } =
    let
        between left right x =
            x > left && x < right
    in
    (pos.line |> between start.line end.line)
        || (pos.line == start.line && pos.col >= start.col)
        || (pos.line == end.line && pos.col < end.col)


lookupRegionSource : Region -> SourceMap -> Maybe String
lookupRegionSource { mod, start, end } sourceMap =
    let
        -- nthLine returns the character position at which the nth line starts
        nthLine n str =
            if str == "" then
                Nothing
            else if n <= 1 then
                Just 0
            else
                let
                    newlines =
                        String.indices "\n" str

                    lineBreakPos =
                        List.head <| List.drop (n - 2) newlines
                in
                Maybe.map ((+) 1) lineBreakPos

        modSource =
            Reader.Dict.get mod sourceMap.sources

        startPos =
            modSource
                |> Maybe.andThen (nthLine start.line)
                |> Maybe.map ((+) (start.col - 1))

        endPos =
            modSource
                |> Maybe.andThen (nthLine end.line)
                |> Maybe.map ((+) (end.col - 1))
    in
    case ( startPos, endPos ) of
        ( Just startIndex, Just endIndex ) ->
            modSource
                |> Maybe.map (String.slice startIndex endIndex)

        ( _, _ ) ->
            Nothing


{-| exprsStartingAt returns a list of the IDs of expressions with a region
starting at `pos`, in decreasing order of the length of the associated region.
-}
exprsStartingAt : Position -> ExprDict (List Region) -> List Ids.ExprId
exprsStartingAt pos exprRegions =
    exprsWithARegionFulfilling (\region -> region.start == pos) exprRegions
        |> List.sortWith
            (\( _, r1 ) ( _, r2 ) -> comparePositions r2.end r1.end)
        |> List.map (\( exprId, _ ) -> exprId)


{-| exprsEndingAt returns a list of the IDs of expressions with a region
ending at `pos`, in increasing order of the length of the associated region.
-}
exprsEndingAt : Position -> ExprDict (List Region) -> List Ids.ExprId
exprsEndingAt pos exprRegions =
    exprsWithARegionFulfilling (\region -> region.end == pos) exprRegions
        |> List.sortWith
            (\( _, r1 ) ( _, r2 ) -> comparePositions r2.start r1.start)
        |> List.map (\( exprId, _ ) -> exprId)


{-| Helper function. See exprsStartingAt and exprsEndingAt
-}
exprsWithARegionFulfilling : (Region -> Bool) -> ExprDict (List Region) -> List ( Ids.ExprId, Region )
exprsWithARegionFulfilling condition exprRegions =
    exprRegions
        |> ExprDict.toList
        |> List.filterMap
            (\( exprId, regions ) ->
                case List.filter condition regions of
                    [ region ] ->
                        Just ( exprId, region )

                    firstRegion :: _ ->
                        Debug.log
                            ("exprsWithARegionFulfilling: got multiple overlapping regions for expr " ++ Debug.toString exprId)
                            (Just ( exprId, firstRegion ))

                    [] ->
                        Nothing
            )


-- IDS

type alias ExprId =
    Ids.ExprId


type alias FrameId =
    Ids.FrameId
