module Reader.StackUI.RenderFrame exposing (renderFrame)

import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as JD
import Reader.Msg as Msg exposing (Msg)
import Reader.SourceMap as SourceMap exposing (SourceMap)
import Reader.SourceMap.ExprDict as SrcMapExprDict
import Reader.SourceMap.Ids exposing (exprIdToInt)
import Reader.TraceData as TraceData exposing (TraceData)
import Array exposing (Array)


renderFrame : SourceMap -> SourceMap.FrameId -> Result String (Html Msg)
renderFrame srcMap frameId =
    frameToTokens srcMap frameId
    |> Result.andThen viewFrameTokens


type Token
    = TokenExprStart SourceMap.ExprId -- '<span title="[expr value]">'
    | TokenExprEnd SourceMap.ExprId -- '</span>'
    | TokenText String -- "sourceText"


frameToTokens :
    SourceMap
    -> SourceMap.FrameId
    -> Result String (Array Token)
frameToTokens srcMap frameId =
    case SourceMap.getFrame frameId srcMap of
        Nothing ->
            Err "could not find frameId in srcMap.frames"

        Just { region, exprRegions } ->
            case SourceMap.lookupRegionSource region srcMap of
                Nothing ->
                    Err "could not find frame.region in srcMap.sources"

                Just src ->
                    Ok (frameSrcToTokens region.start src exprRegions)

thenAppend : Array a -> Array a -> Array a
thenAppend addendum base =
    Array.append base addendum

thenMaybePush : Maybe a -> Array a -> Array a
thenMaybePush addendum base =
    case addendum of
        Just elem ->
            Array.push elem base

        Nothing ->
            base

tokenGenerationIterator exprRegions src =
    \char { position, idx, tokenStart, tokenStream } ->
        let
            nextPos =
                if char == '\n' then
                    { line = position.line + 1
                    , col = 1
                    }
                else
                    { line = position.line
                    , col = position.col + 1
                    }

            starts =
                SourceMap.exprsStartingAt position exprRegions
                    |> List.map TokenExprStart

            ends =
                SourceMap.exprsEndingAt position exprRegions
                    |> List.map TokenExprEnd

            (priorToken, nextTokenStart) =
                if not (List.isEmpty starts && List.isEmpty ends) then
                    (Just <| TokenText <| String.slice tokenStart idx src, idx)
                else
                    (Nothing, tokenStart)
        in
        { position = nextPos
        , idx = idx + 1
        , tokenStart = nextTokenStart
        , tokenStream =
            tokenStream
                |> thenMaybePush priorToken
                |> thenAppend (Array.fromList ends)
                |> thenAppend (Array.fromList starts)
        }


frameSrcToTokens :
    SourceMap.Position
    -> String
    -> SrcMapExprDict.ExprDict (List SourceMap.Region)
    -> Array Token
frameSrcToTokens initialPos src exprRegions =
    let
        { tokenStream } =
            String.foldl (tokenGenerationIterator exprRegions src)
                { position = initialPos, idx = 0, tokenStart = 0, tokenStream = Array.empty }
                src
                -- Run through tokenGenerationIterator once more to capture the last token; 'X' is arbitrary
                |> tokenGenerationIterator exprRegions src 'X'
    in
    tokenStream


joinArray : String -> Array String -> String
joinArray sep entries =
    Maybe.withDefault
        ""
        (Array.foldl
            (\entry result ->
                case result of
                    Just accum ->
                        Just (accum ++ sep ++ entry)

                    Nothing ->
                        Just entry
            )
            Nothing
            entries
        )

viewFrameTokens : Array Token -> Result String (Html Msg)
viewFrameTokens tokens =
    Result.map (Html.pre []) (viewFrameTokensHelp 0 tokens)

viewFrameTokensHelp :
    Int -- idx, index into tokens from which to read
    -> Array Token -- tokens
    -> Result String (List (Html Msg))
viewFrameTokensHelp idx tokens =
    case Array.get idx tokens of
        Just (TokenText txt) ->
            viewFrameTokensHelp (idx + 1) tokens
                |> Result.map (\restHtml -> Html.text txt :: restHtml)

        Just (TokenExprStart exprId) ->
            case takeExprTokensToHtml exprId (idx + 1) tokens of
                Ok ( htmlItems, idxAfterExpr ) ->
                    viewFrameTokensHelp idxAfterExpr tokens
                        |> Result.map (\restHtml -> htmlItems ++ restHtml)

                Err e ->
                    Err (e ++ "\n All tokens were:" ++ Debug.toString tokens)

        Just (TokenExprEnd _) ->
            Err ("unexpected TokenExprEnd [\n  " ++ (joinArray ",\n  " <| Array.map Debug.toString tokens))

        Nothing ->
            Ok []


{-| takeExprTokensToHtml parses a token stream to Html, processing up to the end of
`context.currentExpr` (i.e. until the token `TokenExprEnd context.currentExpr`),
and returns a list of Html items in that expression as well as the list of unconsumed tokens.
-}
takeExprTokensToHtml :
    SourceMap.ExprId -- currentExpr
    -> Int -- idx, index into tokens from which to read
    -> Array Token -- tokens
    -> Result String ( List (Html Msg), Int )
takeExprTokensToHtml currentExpr idx tokens =
    case Array.get idx tokens of
        Nothing ->
            Err "unexpected end of stream"

        Just (TokenText txt) ->
            takeExprTokensToHtml currentExpr (idx + 1) tokens
                |> Result.map
                    (\( restHtmlInContext, idxAfterExpr ) ->
                        ( Html.text txt :: restHtmlInContext
                        , idxAfterExpr
                        )
                    )

        Just (TokenExprStart hereExprId) ->
            takeExprTokensToHtml hereExprId (idx + 1) tokens
                |> Result.andThen
                    (\( htmlItemsHere, idxAfterHereExpr ) ->
                        takeExprTokensToHtml currentExpr idxAfterHereExpr tokens
                            |> Result.map
                                (\( htmlItemsAfterHere, idxAfterContext ) ->
                                    ( exprElem hereExprId htmlItemsHere
                                        :: htmlItemsAfterHere
                                    , idxAfterContext
                                    )
                                )
                    )

        Just (TokenExprEnd closingExprId) ->
            if closingExprId == currentExpr then
                Ok ( [], idx + 1 )
            else
                Err
                    ("Unexpected TokenExprEnd "
                        ++ Debug.toString closingExprId
                        ++ " at index "
                        ++ String.fromInt idx
                        ++ " in tokens array: "
                        ++ Debug.toString tokens
                    )



-- EXPRESSION RENDERING


exprElem : SourceMap.ExprId -> List (Html Msg) -> Html Msg
exprElem exprId contents =
    Html.span
        [ A.class ("elm-reader-expr-" ++ String.fromInt (exprIdToInt exprId)), A.class "elm-reader-expr" ]
        contents

