module Reader.SelectedFrameTree
    exposing
        ( SelectedFrameTree
        , frameIdOf
        , fromTrace
        , getOpenFrames
        , openFrame
        )

import Reader.SourceMap as SourceMap exposing (SourceMap)
import Reader.TraceData as TraceData exposing (TraceData)
import Reader.FrameUI as FrameUI exposing (FrameUI)


type SelectedFrameTree = SelectedFrameTree FrameUI (List SelectedFrameTree)


fromTrace : TraceData.Frame -> SelectedFrameTree
fromTrace frame =
    SelectedFrameTree
        (FrameUI.fromTrace frame)
        (List.map fromTrace (TraceData.childFrames frame))


frameIdOf : SelectedFrameTree -> TraceData.FrameId
frameIdOf (SelectedFrameTree {frame} _) =
    TraceData.frameIdOf frame


openFrame : TraceData.FrameId -> SelectedFrameTree -> SelectedFrameTree
openFrame childFrameId =
    let
        isTarget (SelectedFrameTree otherFrameUI _) =
            TraceData.frameIdsEqual
                childFrameId
                (TraceData.frameIdOf otherFrameUI.frame)

        -- TODO: this can be optimized to avoid linear lookup time of isAncestorOf by
        -- taking the tail of the ancestor list after each iteration.
        isAncestor =
            frameIdOf >> TraceData.isAncestorOf childFrameId

        openFrameIn ((SelectedFrameTree frameUI children) as selTree) =
            if isTarget selTree then
                selTree
            else
                let
                    combine sf ( newChild, restChildren ) =
                        if isTarget sf || isAncestor sf then
                            ( Just (frameIdOf sf)
                            , openFrameIn sf :: restChildren
                            )
                        else
                            ( newChild, sf :: restChildren )

                    ( maybeNewOpenChild, newChildren ) =
                        children
                            |> List.foldr combine ( Nothing, [] )
                in
                SelectedFrameTree
                    { frameUI | openedChild = maybeNewOpenChild }
                    newChildren
    in
    openFrameIn


getOpenFrames :
    SelectedFrameTree
    -> List FrameUI
getOpenFrames (SelectedFrameTree thisFrameUI children) =
    case thisFrameUI.openedChild of
        Nothing ->
            [ thisFrameUI ]

        Just childId ->
            let
                isTarget =
                    TraceData.frameIdsEqual childId
            in
            case children |> List.filter (frameIdOf >> isTarget) of
                [ openChild ] ->
                    thisFrameUI :: getOpenFrames openChild

                [] ->
                    Debug.log
                        "ERROR: did not find open child ID in child frames"
                        [ thisFrameUI ]

                (anOpenChild :: _ :: _) as duplicates ->
                    let
                        _ =
                            Debug.log
                                "ERROR: found duplicate child frames with same ID"
                                (List.map frameIdOf duplicates)
                    in
                    thisFrameUI
                        :: getOpenFrames anOpenChild
