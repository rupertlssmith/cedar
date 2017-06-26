port module ScrollPort exposing (scroll, Move)

import Scroll exposing (Move)


type alias Move =
    Scroll.Move


port scroll : (Move -> msg) -> Sub msg
