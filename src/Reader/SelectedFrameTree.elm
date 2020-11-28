module Reader.SelectedFrameTree
    exposing
        ( SelectedFrameTree
        , frameIdOf
        , fromTrace
        , getOpenFrames
        , openFrame
        )

import Elm.Kernel.Reader
import Reader.SourceMap as SourceMap exposing (SourceMap)
import Reader.TraceData as TraceData exposing (TraceData)
import Reader.FrameUI as FrameUI exposing (FrameUI)
import Reader.FrameUI.Instrumented as Instrumented

type SelectedFrameTree
    = Thunk TraceData.FrameId (() -> TraceData.Frame)
    | Concrete Evaluated


type Evaluated = Evaluated FrameUI (List SelectedFrameTree)


evaluate : SelectedFrameTree -> Evaluated
evaluate sft =
    case sft of
        Thunk _ thunk ->
            fromEvaluatedTrace (thunk ())

        Concrete frame ->
            frame

fromTrace : TraceData.Frame -> SelectedFrameTree
fromTrace frame =
    case frame of
        TraceData.FrameThunk id thunk ->
            Thunk id thunk

        _ ->
            Concrete (fromEvaluatedTrace frame)

fromEvaluatedTrace : TraceData.Frame -> Evaluated
fromEvaluatedTrace frame =
    Evaluated
        (FrameUI.fromTrace frame)
        (List.map fromTrace (TraceData.childFrames frame))

frameIdOf : SelectedFrameTree -> TraceData.FrameId
frameIdOf sft =
    case sft of
        Thunk id _ ->
            id

        Concrete (Evaluated frameUI _) ->
            TraceData.frameIdOf frameUI.frame


evaluateWithTokens : SourceMap -> SelectedFrameTree -> Evaluated
evaluateWithTokens srcMap sft =
    let
        (Evaluated frameUI children) =
            evaluate sft

        newFrameUI =
            { frameUI
                | tokens =
                    case frameUI.frame of
                        TraceData.InstrumentedFrame {sourceId} ->
                            Result.toMaybe (Instrumented.frameToTokens srcMap sourceId)

                        _ ->
                            Nothing
            }
    in
    Evaluated newFrameUI children


openFrame : TraceData.FrameId -> SourceMap -> SelectedFrameTree -> SelectedFrameTree
openFrame childFrameId sources =
    let
        isTarget otherSelTree =
            TraceData.frameIdsEqual
                childFrameId
                (frameIdOf otherSelTree)

        -- TODO: this can be optimized to avoid linear lookup time of isAncestorOf by
        -- taking the tail of the ancestor list after each iteration.
        isAncestor =
            frameIdOf >> TraceData.isAncestorOf childFrameId

        openFrameIn selTree =
            if isTarget selTree then
                Concrete (evaluateWithTokens sources selTree)
            else
                let
                    combine sf ( newChild, restChildren ) =
                        if isTarget sf || isAncestor sf then
                            ( Just (frameIdOf sf)
                            , openFrameIn sf :: restChildren
                            )
                        else
                            ( newChild, sf :: restChildren )

                    (Evaluated frameUI children) = evaluateWithTokens sources selTree

                    ( maybeNewOpenChild, newChildren ) =
                        List.foldr combine ( Nothing, [] ) children
                in
                Concrete
                    (Evaluated
                        { frameUI
                            | openedChild = maybeNewOpenChild
                        }
                        newChildren
                    )
    in
    openFrameIn

isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False

getOpenFrames :
    SelectedFrameTree
    -> List FrameUI
getOpenFrames selTree =
    case selTree of
        Thunk  _ _ ->
            []

        Concrete (Evaluated thisFrameUI children) ->
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
