module Reader.FrameUI.Instrumented exposing (viewFrameTrace, frameToTokens)

{-| Reader.FrameUI.View.Instrumented handles the creation of the clickable,
highlightable text of a frame.
-}

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as JD
import Reader.Dict as Dict exposing (Dict)
import Reader.Msg as Msg exposing (Msg)
import Reader.SourceMap as SourceMap exposing (SourceMap)
import Reader.TraceData as TraceData exposing (TraceData)
import Reader.TraceData.Value as Value exposing (Value)
import Reader.FrameUI.Token as Token exposing (Token)
import Array
import Array exposing (Array)


viewFrameTrace :
    SourceMap
    -> Maybe TraceData.ExprWithContext
    -> Maybe TraceData.FrameId
    -> Maybe (List Token)
    -> TraceData.InstrumentedFrameData
    -> Result String (List Token, Html Msg)
viewFrameTrace srcMap hoveredExpr openChildFrameId cachedTokens tracedFrame =
    let
        tokensResult =
            case cachedTokens of
                Just ts ->
                    Ok ts

                Nothing ->
                    (frameToTokens srcMap tracedFrame.sourceId)
    in
    case tokensResult of
        Ok tokens ->
            tokens
                |> viewFrameTokens tracedFrame hoveredExpr openChildFrameId
                |> Result.map (H.pre [ A.style "margin-top" "3px" ])
                |> Result.map (\html -> (tokens, html))

        Err e ->
            Err e


frameToTokens :
    SourceMap
    -> SourceMap.FrameId
    -> Result String (List Token)
frameToTokens srcMap frameId =
    case Dict.lookup frameId srcMap.frames of
        Nothing ->
            Err "could not find frameId in srcMap.frames"

        Just { region, exprRegions } ->
            case SourceMap.lookupRegionSource region srcMap.sources of
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
                    |> List.map Token.ExprStart

            ends =
                SourceMap.exprsEndingAt position exprRegions
                    |> List.map Token.ExprEnd

            (priorToken, nextTokenStart) =
                if not (List.isEmpty starts && List.isEmpty ends) then
                    (Just <| Token.Text <| String.slice tokenStart idx src, idx)
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
    -> Dict SourceMap.ExprId (List SourceMap.Region)
    -> List Token
frameSrcToTokens initialPos src exprRegions =
    let
        { tokenStream } =
            String.foldl (tokenGenerationIterator exprRegions src)
                { position = initialPos, idx = 0, tokenStart = 0, tokenStream = Array.empty }
                src
                -- Run through tokenGenerationIterator once more to capture the last token; 'X' is arbitrary
                |> tokenGenerationIterator exprRegions src 'X'
    in
    Array.toList tokenStream


viewFrameTokens :
    TraceData.InstrumentedFrameData
    -> Maybe TraceData.ExprWithContext
    -> Maybe TraceData.FrameId
    -> List Token
    -> Result String (List (Html Msg))
viewFrameTokens frameTrace hoveredExpr openChildFrameId tokens =
    case tokens of
        (Token.Text txt) :: rest ->
            viewFrameTokens frameTrace hoveredExpr openChildFrameId rest
                |> Result.map (\restHtml -> H.text txt :: restHtml)

        (Token.ExprStart exprId) :: rest ->
            let
                exprRenderingContext =
                    { frameTrace = frameTrace
                    , currentExpr = exprId
                    , hoveredExpr = hoveredExpr
                    , openChildFrameId = openChildFrameId
                    }
            in
            case takeExprTokensToHtml exprRenderingContext rest of
                Ok ( htmlItems, tokensAfterExpr ) ->
                    viewFrameTokens frameTrace hoveredExpr openChildFrameId tokensAfterExpr
                        |> Result.map (\restHtml -> htmlItems ++ restHtml)

                Err e ->
                    Err (e ++ "\n All tokens were:" ++ Debug.toString tokens)

        (Token.ExprEnd exprId) :: rest ->
            Err ("unexpected Token.ExprEnd [\n  " ++ (String.join ",\n  " <| List.map Debug.toString tokens))

        [] ->
            Ok []


type alias ExprRenderingContext =
    { hoveredExpr : Maybe TraceData.ExprWithContext
    , currentExpr : SourceMap.ExprId
    , frameTrace : TraceData.InstrumentedFrameData
    , openChildFrameId : Maybe TraceData.FrameId
    }

isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False

{-| takeExprTokensToHtml parses a token stream to Html, processing up to the end of
`context.currentExpr` (i.e. until the token `Token.ExprEnd context.currentExpr`),
and returns a list of Html items in that expression as well as the list of unconsumed tokens.
-}
takeExprTokensToHtml :
    ExprRenderingContext
    -> List Token
    -> Result String ( List (Html Msg), List Token )
takeExprTokensToHtml context tokens =
    case tokens of
        [] ->
            Err "unexpected end of stream"

        (Token.Text txt) :: restTokens ->
            takeExprTokensToHtml context restTokens
                |> Result.map
                    (\( restHtmlInContext, tokensAfterContext ) ->
                        ( H.text txt :: restHtmlInContext
                        , tokensAfterContext
                        )
                    )

        (Token.ExprStart hereExprId) :: restTokens ->
            let
                ( maybeExprData, exprTitle ) =
                    case Dict.lookup hereExprId context.frameTrace.exprs of
                        Nothing ->
                            ( Nothing
                            , "No value for this expression (e.g. because it is a unexecuted branch)"
                            )

                        Just expr ->
                            case TraceData.exprValue expr of
                                Just val ->
                                    ( Just
                                        { frameSrcId = context.frameTrace.sourceId
                                        , stackFrameId = context.frameTrace.runtimeId
                                        , exprId = hereExprId
                                        , expr = expr
                                        }
                                    , Value.toString val
                                    )

                                Nothing ->
                                    ( Nothing
                                    , "<no value associated with this expr in trace frame. exprID is " ++ Debug.toString hereExprId ++ " expr is " ++ Debug.toString expr ++ ">"
                                    )

                styleClasses =
                    styleClassesFor context maybeExprData

                exprOptions =
                    { title = exprTitle
                    , maybeExprData = maybeExprData
                    , styleClasses = styleClasses
                    , contents = [] -- to be filled in below
                    }
            in
            takeExprTokensToHtml { context | currentExpr = hereExprId } restTokens
                |> Result.andThen
                    (\( htmlItemsHere, tokensAfterHereExpr ) ->
                        takeExprTokensToHtml context tokensAfterHereExpr
                            |> Result.map
                                (\( htmlItemsAfterHere, tokensAfterContext ) ->
                                    ( exprElem { exprOptions | contents = htmlItemsHere }
                                        :: htmlItemsAfterHere
                                    , tokensAfterContext
                                    )
                                )
                    )

        (Token.ExprEnd closingExprId) :: restTokens ->
            if closingExprId == context.currentExpr then
                Ok ( [], restTokens )
            else
                Err
                    ("Unexpected Token.ExprEnd: "
                        ++ Debug.toString closingExprId
                        ++ ", restTokens: "
                        ++ Debug.toString restTokens
                    )


-- EXPRESSION RENDERING: highlighting and event handling

type alias ExprOptions =
    { title : String
    , maybeExprData : Maybe TraceData.ExprWithContext
    , styleClasses : StyleClasses
    , contents : List (Html Msg)
    }


type alias StyleClasses = List String

styleClassesFor : ExprRenderingContext -> Maybe TraceData.ExprWithContext -> StyleClasses
styleClassesFor context maybeExprData =
    case maybeExprData of
        Nothing ->
            [ "elm-reader-untraced" ]

        Just exprData ->
            let
                isHovered =
                    case context.hoveredExpr of
                        Nothing ->
                            False

                        Just hovered ->
                            TraceData.frameIdsEqual hovered.stackFrameId context.frameTrace.runtimeId
                                -- FIXME: necessary? && hovered.frameSrcId == context.frameTrace.sourceId
                                && hovered.exprId == exprData.exprId
            in
            case TraceData.exprChildFrame exprData.expr of
                Nothing ->
                    [ "elm-reader-expr" ]
                        ++ (if isHovered then [ "elm-reader-expr--hovered" ] else [])

                Just childFrame ->
                    let
                        isOpened =
                            context.openChildFrameId == Just (TraceData.frameIdOf childFrame)
                    in
                    [ "elm-reader-expr", "elm-reader-call" ]
                        ++ (if isHovered then ["elm-reader-expr--hovered"] else [])
                        ++ (if isOpened then ["elm-reader-call--opened"] else [])


exprElem : ExprOptions -> Html Msg
exprElem { title, maybeExprData, styleClasses, contents } =
    let
        handlers =
            case maybeExprData of
                Nothing ->
                    []

                Just exprData ->
                    [ handleMouseOut, handleMouseOver exprData ]
                        ++ handleClick exprData
    in
    H.span
        (handlers ++ List.map A.class styleClasses)
        contents

handleMouseOver exprData =
    E.stopPropagationOn "mouseover"
        (JD.succeed ( Msg.HoverExpr exprData, True ))

handleMouseOut =
    E.stopPropagationOn "mouseout"
        (JD.succeed ( Msg.UnHoverExpr, True ))

handleClick exprData =
    case TraceData.exprChildFrame exprData.expr of
        Nothing ->
            [ E.stopPropagationOn "click" (JD.succeed ( Msg.NoOp, True )) ]

        Just childFrame ->
            [ E.stopPropagationOn
                "click"
                (JD.succeed ( Msg.OpenChildFrame (TraceData.frameIdOf childFrame), True ))
            ]
