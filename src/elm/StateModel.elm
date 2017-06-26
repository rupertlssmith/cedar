module StateModel
    exposing
        ( boolToMaybe
        , (>&&>)
        , (>||>)
        , (>##>)
        , defaultTransition
        , mapWhenCompose
        )

import Maybe exposing (andThen)
import Maybe.Extra exposing (orElse, unwrap)


boolToMaybe : (a -> Bool) -> a -> Maybe a
boolToMaybe filter val =
    if filter val then
        Just val
    else
        Nothing


{-|
 Both functions must succeed to get a result. If either fails with Nothing,
 then the result will be Nothing
-}
(>&&>) : (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
(>&&>) fst snd val =
    fst val |> andThen snd


{-|
 The resutl will be the second function, iff the first function produces Nothing.
-}
(>||>) : (a -> Maybe b) -> (a -> Maybe b) -> a -> Maybe b
(>||>) fst snd val =
    fst val |> orElse (snd val)


{-|
 The functions are applied one after the other.
 If the first function produces Nothing the original input will be given to the second function.
 If the first function produces Just someValue, that value will be given to the second function.
 If the second function fails with Nothing, the output will be whatever the first function output.
 This skips over operations that fail, passing the operand down the chain to try it with successive
 functions.
-}
(>##>) : (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
(>##>) fst snd val =
    unwrap (snd val) (\fstOut -> (unwrap (Just fstOut) (Just) (snd fstOut))) (fst val)


defaultTransition state trans =
    trans state |> Maybe.withDefault state


mapWhenCompose : (c -> d -> Maybe a) -> (a -> d -> Maybe b) -> c -> d -> Maybe b
mapWhenCompose mapWhen1 mapWhen2 func state =
    mapWhen1 func state
        |> andThen ((flip mapWhen2) state)
