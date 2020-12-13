module Reader.StackUI.RenderedFrameMap
    exposing
        ( RenderedFrameMap
        , ensure
        , ensureOne
        , renderFrame
        , empty
        , get
        )

import Html exposing (Html)

import Reader.Msg exposing (Msg)
import Reader.SourceMap as SourceMap exposing (SourceMap)
import Reader.SourceMap.FrameDict as SrcMapFrameDict

import Reader.StackUI.RenderFrame as RenderFrame


type RenderedFrameMap = RenderedFrameMap (SrcMapFrameDict.FrameDict { lines : Int, html : Html Msg })


empty : RenderedFrameMap
empty = RenderedFrameMap SrcMapFrameDict.empty


get : SourceMap.FrameId -> RenderedFrameMap -> Maybe { lines : Int, html : Html Msg }
get frameId (RenderedFrameMap map) =
    SrcMapFrameDict.get frameId map


ensure : SourceMap -> List SourceMap.FrameId -> RenderedFrameMap -> RenderedFrameMap
ensure srcMap frames (RenderedFrameMap renderedFrameMap) =
    RenderedFrameMap
        (List.foldl
            (\sourceId renders ->
                if SrcMapFrameDict.member sourceId renders then
                    renders
                else
                    -- It is important not to invoke renderFrame unless sourceId is actually not set (it's expensive)
                    SrcMapFrameDict.set sourceId (renderFrame srcMap sourceId) renders
                )
            renderedFrameMap
            frames)


ensureOne : SourceMap -> SourceMap.FrameId -> RenderedFrameMap -> ( RenderedFrameMap, { lines : Int, html : Html Msg } )
ensureOne srcMap sourceId ((RenderedFrameMap renders) as renderedFrameMap) =
    case SrcMapFrameDict.get sourceId renders of
        Just render ->
            ( renderedFrameMap, render )

        Nothing ->
            let
                render =
                    renderFrame srcMap sourceId
            in
            ( RenderedFrameMap (SrcMapFrameDict.set sourceId render renders), render )

renderFrame : SourceMap -> SourceMap.FrameId -> { lines : Int, html : Html Msg }
renderFrame srcMap id =
    case ( RenderFrame.renderFrame srcMap id, SourceMap.getFrameHeight id srcMap ) of
        ( Err message, maybeLines ) ->
            { lines = Maybe.withDefault 1 maybeLines, html = Html.div [] [ Html.text message ]}

        ( _, Nothing ) ->
            { lines = 1, html = Html.div [] [ Html.text "Failed to get rendered frame's height!" ]}

        ( Ok html, Just lines ) ->
            { lines = lines, html = html }
