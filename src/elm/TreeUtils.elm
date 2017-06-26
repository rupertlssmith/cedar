module TreeUtils
    exposing
        ( updateTree
        , (&>)
        , foldToLeft
        , foldToRight
        , foldToNext
        , foldToPrevious
        , foldToUp
        )

import MultiwayTree as Tree exposing (Tree(..))
import MultiwayTreeZipper as Zipper exposing (Zipper)


{-| Applies an update function to the node under the zipper, then
 walks back up to the root of the tree, to produce a new tree with
 the update applied to the designated node.
-}
updateTree : (a -> a) -> Zipper a -> Maybe (Tree a)
updateTree update zipper =
    Just zipper
        &> Zipper.updateDatum update
        &> Zipper.goToRoot
        &> (\( tree, _ ) -> Just tree)


(&>) : Maybe a -> (a -> Maybe b) -> Maybe b
(&>) =
    flip Maybe.andThen


fold : (Zipper a -> Maybe (Zipper a)) -> (Zipper a -> b -> b) -> b -> Zipper a -> b
fold navop func accum zipper =
    case navop zipper of
        Nothing ->
            func zipper accum

        Just nextZipper ->
            fold navop func (func zipper accum) nextZipper


foldToLeft : (Zipper a -> b -> b) -> b -> Zipper a -> b
foldToLeft =
    fold Zipper.goLeft


foldToRight : (Zipper a -> b -> b) -> b -> Zipper a -> b
foldToRight =
    fold Zipper.goRight


foldToNext : (Zipper a -> b -> b) -> b -> Zipper a -> b
foldToNext =
    fold Zipper.goToNext


foldToPrevious : (Zipper a -> b -> b) -> b -> Zipper a -> b
foldToPrevious =
    fold Zipper.goToPrevious


foldToUp : (Zipper a -> b -> b) -> b -> Zipper a -> b
foldToUp =
    fold Zipper.goUp
