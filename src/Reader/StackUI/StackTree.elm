module Reader.StackUI.StackTree
    exposing
        ( StackTree
        , singleton
        , map
        , openChildFrame
        )

import Reader.TraceData as TraceData
import Reader.StackUI.OpenFrame as OpenFrame exposing (OpenFrame)


type StackTree =
    StackTree
        { first : OpenFrame
        , openChild : Maybe StackTree
        , previouslyOpenedChildren : List StackTree
        -- probably unnecessary , height : Int -- units of line-height from the first line at the top of the first frame
        }


singleton : OpenFrame -> StackTree
singleton openFrame =
    StackTree { first = openFrame, openChild = Nothing, previouslyOpenedChildren = [] }


{-| `map` applies `func` to each *open frame* in the stack tree, i.e.
to each frame in the currently open stack, and returns the results in a
List. -}
map : (OpenFrame -> a) -> StackTree -> List a
map func (StackTree { first, openChild }) =
    func first ::
        case openChild of
            Nothing -> []
            Just stackTree -> map func stackTree


openChildFrame : TraceData.FrameId -> OpenFrame -> StackTree -> StackTree
openChildFrame parentRuntimeId childFrame stackTree =
    case OpenFrame.frameIdsOf childFrame of
        Just ( _, childFrameRuntimeId ) ->
            openChildFrameHelp
                parentRuntimeId
                childFrame
                (\(StackTree { first }) ->
                    case OpenFrame.frameIdsOf first of
                        Just ( _, id ) ->
                            TraceData.frameIdsEqual id childFrameRuntimeId

                        Nothing ->
                            False
                )
                stackTree

        Nothing ->
            stackTree -- unreachable if childFrame is valid


openChildFrameHelp : TraceData.FrameId -> OpenFrame -> (StackTree -> Bool) -> StackTree -> StackTree
openChildFrameHelp parentRuntimeId childFrame isChildFrame (StackTree stackTree) =
    case OpenFrame.frameIdsOf stackTree.first of
        Just ( _, frameRuntimeId ) ->
            if TraceData.frameIdsEqual frameRuntimeId parentRuntimeId then
                -- Check previouslyOpenedChildren for a subtree
                case List.partition isChildFrame stackTree.previouslyOpenedChildren of
                    ( [ theChild ] , rest ) ->
                        StackTree
                            { first = stackTree.first
                            , openChild = Just theChild
                            , previouslyOpenedChildren = pushIfJust stackTree.openChild stackTree.previouslyOpenedChildren
                            }

                    ( [], _ ) ->
                        -- If it wasn't previously open, open it
                        StackTree
                            { stackTree
                                | openChild = Just (singleton childFrame)
                                , previouslyOpenedChildren =
                                    pushIfJust stackTree.openChild stackTree.previouslyOpenedChildren
                                }

                    _ ->
                        StackTree stackTree -- should be unreachable (there should be only one child satisfying isChildFrame)
            else
                -- As parentRuntimeId is not this frame, proceed down the stack
                StackTree
                    { stackTree
                        | openChild =
                            Maybe.map (openChildFrameHelp parentRuntimeId childFrame isChildFrame) stackTree.openChild
                        }

        Nothing ->
            StackTree stackTree -- unreachable if stackTree.first is a valid OpenFrame


pushIfJust : Maybe v -> List v -> List v
pushIfJust maybeV vs =
    case maybeV of
        Just v ->
            v :: vs

        Nothing ->
            vs
