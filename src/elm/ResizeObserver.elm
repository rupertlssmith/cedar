port module ResizeObserver
    exposing
        ( ResizeEvent
        , resize
        )


type alias ResizeEvent =
    { id : String
    , width : Float
    , height : Float
    }


resize : (ResizeEvent -> msg) -> Sub msg
resize =
    mutation


port mutation : (ResizeEvent -> msg) -> Sub msg
