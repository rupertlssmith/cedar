module ResizeObserver exposing (ResizeEvent, Resize)

{-|
Defines the type of a resize observer.

@docs ResizeEvent, Resize
-}


{-|
Describes the fields that a resize event must produce.
-}
type alias ResizeEvent =
    { id : String
    , width : Float
    , height : Float
    }


{-|
Describes the type of a resize observer.
-}
type alias Resize =
    Sub ResizeEvent
