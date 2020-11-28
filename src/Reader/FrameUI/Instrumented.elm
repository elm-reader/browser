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
                    Debug.log "CALCULATING TOKENS in VIEW" (frameToTokens srcMap tracedFrame.sourceId)
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

frameSrcToTokens :
    SourceMap.Position
    -> String
    -> Dict SourceMap.ExprId (List SourceMap.Region)
    -> List Token
frameSrcToTokens initialPos src exprRegions =
    let
        (_, association) =
            String.foldl
                (\char (position, accum) ->
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
                            SourceMap.exprsEndingAt nextPos exprRegions
                                |> List.map Token.ExprEnd
                    in
                    ( nextPos
                    , accum
                        |> thenAppend (Array.fromList starts)
                        |> Array.push (Token.Character char)
                        |> thenAppend (Array.fromList ends)
                    )
                )
                ( initialPos, Array.empty )
                src
    in
    Array.toList association


viewFrameTokens :
    TraceData.InstrumentedFrameData
    -> Maybe TraceData.ExprWithContext
    -> Maybe TraceData.FrameId
    -> List Token
    -> Result String (List (Html Msg))
viewFrameTokens frameTrace hoveredExpr openChildFrameId tokens =
    case tokens of
        (Token.Character ch) :: rest ->
            viewFrameTokens frameTrace hoveredExpr openChildFrameId rest
                |> Result.map (\restHtml -> H.text (String.fromChar ch) :: restHtml)

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

        (Token.Character ch) :: restTokens ->
            takeExprTokensToHtml context restTokens
                |> Result.map
                    (\( restHtmlInContext, tokensAfterContext ) ->
                        ( H.text (String.fromChar ch) :: restHtmlInContext
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

                highlights =
                    highlightsFor context maybeExprData

                exprOptions =
                    { title = exprTitle
                    , maybeExprData = maybeExprData
                    , highlights = highlights
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
    , highlights : Highlights
    , contents : List (Html Msg)
    }


type Highlight
    = Hovered
    -- for expressions with no recorded trace value:
    | Untraced
    -- for expressions whose child frames are open in the UI:
    | OpenedCall

type alias Highlights = List Highlight

highlightsFor : ExprRenderingContext -> Maybe TraceData.ExprWithContext -> Highlights
highlightsFor context maybeExprData =
    case maybeExprData of
        Nothing ->
            [ Untraced ]

        Just exprData ->
            highlightsForOpenedCall context (TraceData.exprChildFrame exprData.expr)
                ++ highlightsForHovered context exprData.exprId

highlightsForOpenedCall : ExprRenderingContext -> Maybe TraceData.Frame -> Highlights
highlightsForOpenedCall {openChildFrameId} maybeChildFrame =
    case (openChildFrameId, maybeChildFrame) of
        (Just idOfOpened, Just childFrame) ->
            if idOfOpened == TraceData.frameIdOf childFrame then
                [ OpenedCall ]
            else
                []

        _ ->
            []


highlightsForHovered : ExprRenderingContext -> SourceMap.ExprId -> Highlights
highlightsForHovered context hereExprId =
    case context.hoveredExpr of
        Just hovered ->
            let
                isHovered =
                    (hovered.frameSrcId == context.frameTrace.sourceId)
                        && (hovered.exprId == hereExprId)
                        && (hovered.stackFrameId == context.frameTrace.runtimeId)
            in
            if isHovered then
                [ Hovered ]
            else
                []

        Nothing ->
            []


baseStyle =
    [ A.style "border-radius" "3px"
    , A.style "padding" "1px"
    , A.style "margin" "1px"
    ]


highlightStyles hs =
    let
        border =
            [ A.style "border" "1px solid #51d59d"
            , A.style "margin" "0px !important"
            ]
    in
    case hs of
        Hovered ->
            [ A.style "background-color" "rgb(230, 230, 200)" ]

        Untraced ->
            [ A.style "text-decoration" "line-through"
            ]

        OpenedCall ->
            [ A.style "border" ""
            ]
                ++ border


exprElem : ExprOptions -> Html Msg
exprElem { title, maybeExprData, highlights, contents } =
    let
        handlers =
            case maybeExprData of
                Nothing ->
                    [ E.stopPropagationOn "mouseover" (JD.succeed ( Msg.UnHoverExpr, True ))
                    , E.stopPropagationOn "click" (JD.succeed ( Msg.NoOp, True ))
                    ]

                Just exprData ->
                    handleMouseOut
                        :: handleMouseOver exprData
                        :: handleClick exprData
    in
    H.span
        (List.concatMap highlightStyles highlights
            ++ handlers
            ++ baseStyle
            ++ [ A.title title ]
        )
        contents

handleMouseOver exprData =
    E.stopPropagationOn "mouseover"
        (JD.succeed ( Msg.HoverExpr exprData, True ))

handleMouseOut =
    E.stopPropagationOn "mouseout"
        (JD.succeed ( Msg.UnHoverExpr, True ))

handleClick exprData =
    let
        ( msg, cursor ) =
            case TraceData.exprChildFrame exprData.expr of
                Nothing ->
                    ( Msg.NoOp, "default" )

                Just childFrame ->
                    ( Msg.OpenChildFrame (TraceData.frameIdOf childFrame)
                    , "pointer"
                    )
    in
    [ E.stopPropagationOn "click" (JD.succeed ( msg, True ))
    , A.style "cursor" cursor
    ]
