module Reader.StackUI.StackTree
    exposing
        ( StackTree
        , singleton
        , map
        , openChildFrame
        )


import Array exposing (Array)

import Reader.TraceData as TraceData
import Reader.StackUI.OpenFrame as OpenFrame exposing (OpenFrame)


type StackTree
    = StackTreeI
        { first : TraceData.InstrumentedFrameData
        , openChild : Maybe StackTree
        , previouslyOpenedChildren : List StackTree
        }
    | StackTreeN
        { first : ( TraceData.FrameId, Array TraceData.InstrumentedFrameData )
        , openChildIndex : Int -- index into first[1] at which openChild's frame is found, used for UI input field
        , openChild : StackTree
        , previouslyOpenedChildren : List StackTree
        }


{- Returns `Just` for valid OpenFrames -}
singleton : OpenFrame -> Maybe StackTree
singleton openFrame =
    case openFrame of
        OpenFrame.Instrumented data ->
            Just <|
                StackTreeI { first = data, openChild = Nothing, previouslyOpenedChildren = [] }

        OpenFrame.NonInstrumented id idx subframes ->
            case Array.get idx subframes of
                Nothing ->
                    Nothing

                Just data ->
                    Just <|
                        StackTreeN
                            { first = ( id, subframes )
                            , openChildIndex = idx
                            , openChild =
                                StackTreeI { first = data, openChild = Nothing, previouslyOpenedChildren = [] }
                            , previouslyOpenedChildren = []
                            }


toOpenFrame : StackTree -> OpenFrame
toOpenFrame stackTree =
    case stackTree of
        StackTreeI { first } ->
            OpenFrame.Instrumented first

        StackTreeN { first, openChildIndex } ->
            let
                ( id, subframes ) = first
            in
            OpenFrame.NonInstrumented id openChildIndex subframes


frameIdOf : StackTree -> TraceData.FrameId
frameIdOf stackTree =
    case stackTree of
        StackTreeI { first } ->
            first.runtimeId

        StackTreeN { first } ->
            let
                ( id, _ ) = first
            in
            id


{-| `map` applies `func` to each *open frame* in the stack tree,
i.e. to each frame in the currently open stack,
and returns the results in a List. -}
map : (OpenFrame -> a) -> StackTree -> List a
map func stackTree =
    func (toOpenFrame stackTree) ::
        case getOpenChild stackTree of
            Nothing ->
                []
            Just st ->
                func (toOpenFrame st) :: map func st


openChildFrame : TraceData.FrameId -> OpenFrame -> StackTree -> StackTree
openChildFrame parentRuntimeId childFrame stackTree =
    let
        childFrameRuntimeId =
            OpenFrame.runtimeFrameIdOf childFrame
    in
    case singleton childFrame of
        Just childSingleton ->
            openChildFrameHelp
                parentRuntimeId
                childSingleton
                (\st ->
                    TraceData.frameIdsEqual childFrameRuntimeId (frameIdOf st)
                )
                stackTree

        Nothing ->
            stackTree -- invariant violation in childFrame


withThisOpenChild : StackTree -> StackTree -> List StackTree -> StackTree
withThisOpenChild stackTree newOpenChild newChildren =
    case stackTree of
        StackTreeI st ->
            StackTreeI { first = st.first, openChild = Just newOpenChild, previouslyOpenedChildren = newChildren }

        StackTreeN st ->
            let
                ( id, subframes ) = st.first
                newIdx =
                    indexOf
                        (\frame ->
                            TraceData.frameIdsEqual (frameIdOf newOpenChild) frame.runtimeId
                        )
                        subframes
                        |> Maybe.withDefault st.openChildIndex -- for `indexOf` to fail violates invariants
            in
            StackTreeN
                { first = ( id, subframes )
                , openChildIndex = newIdx
                , openChild = newOpenChild
                , previouslyOpenedChildren = newChildren
                }


mapOpenChild : StackTree -> (StackTree -> StackTree) -> StackTree
mapOpenChild stackTree func =
    -- NOTE: `func` must leave the `first` field intact, as
    -- this map function doesn't change `previouslyOpenChildren`
    -- or `openChildIndex`.
    case stackTree of
        StackTreeI st ->
            StackTreeI { st | openChild = Maybe.map func st.openChild }

        StackTreeN st ->
            StackTreeN { st | openChild = func st.openChild }


getOpenChild : StackTree -> Maybe StackTree
getOpenChild stackTree =
    case stackTree of
        StackTreeI st ->
            st.openChild

        StackTreeN st ->
            Just st.openChild


getPreviouslyOpenedChildren : StackTree -> List StackTree
getPreviouslyOpenedChildren stackTree =
    case stackTree of
        StackTreeI st ->
            st.previouslyOpenedChildren

        StackTreeN st ->
            st.previouslyOpenedChildren


openChildFrameHelp : TraceData.FrameId -> StackTree -> (StackTree -> Bool) -> StackTree -> StackTree
openChildFrameHelp parentRuntimeId childSingleton isChildFrame stackTree =
    if TraceData.frameIdsEqual (frameIdOf stackTree) parentRuntimeId then
        let
            childAlreadyOpen =
                getOpenChild stackTree |> Maybe.map isChildFrame |> Maybe.withDefault False
        in
        if childAlreadyOpen then
            stackTree
        else
            -- Check previouslyOpenedChildren for a subtree
            case List.partition isChildFrame (getPreviouslyOpenedChildren stackTree) of
                ( [ theChild ] , rest ) ->
                    withThisOpenChild
                        stackTree
                        theChild
                        (pushIfJust (getOpenChild stackTree) rest)

                ( [], rest ) ->
                    -- If it wasn't previously open, open it
                    withThisOpenChild
                        stackTree
                        childSingleton
                        (pushIfJust (getOpenChild stackTree) rest)

                _ ->
                    stackTree -- should be unreachable (there should be at most child satisfying isChildFrame)
    else
        -- As parentRuntimeId is not this frame, proceed down the stack
        mapOpenChild stackTree (openChildFrameHelp parentRuntimeId childSingleton isChildFrame)


pushIfJust : Maybe v -> List v -> List v
pushIfJust maybeV vs =
    case maybeV of
        Just v ->
            v :: vs

        Nothing ->
            vs

indexOf : (v -> Bool) -> Array v -> Maybe Int
indexOf condition arr =
    Array.indexedMap (\i elem -> (i, condition elem)) arr
    |> Array.filter (\(i, cond) -> cond)
    |> Array.map (\(i, _) -> i)
    |> Array.get 0
