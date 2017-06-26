module ScrollPort exposing (Scroll, Move)

{-|
Describes the type of a scroll port observer.

@docs Scroll, Move
-}

import Scroll exposing (Move)


{-|
Describes the fields that a scroll event must produce.
-}
type alias Move =
    Scroll.Move


{-|
Describes the type of a scroll observer.
-}
type alias Scroll =
    Sub Move
