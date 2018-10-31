module DOMUtils exposing (DOMState, domMetricsOn)

import DOM
import Html exposing (Attribute)
import Html.Events
import Json.Decode as Json exposing (at, field)


type alias Metrics =
    { rect : DOM.Rectangle
    , x : Float
    , y : Float
    }


type alias DOMState =
    { rect : DOM.Rectangle
    , clientX : Maybe Float
    , clientY : Maybe Float
    , touchX : Maybe Float
    , touchY : Maybe Float
    , type_ : String
    }


computeMetrics : DOMState -> Maybe Metrics
computeMetrics g =
    let
        rect =
            g.rect

        set x y =
            ( x - rect.left, y - rect.top ) |> Just
    in
    (case ( g.clientX, g.clientY, g.touchX, g.touchY ) of
        ( Just 0.0, Just 0.0, _, _ ) ->
            ( rect.width / 2.0, rect.height / 2.0 ) |> Just

        ( Just x, Just y, _, _ ) ->
            set x y

        ( _, _, Just x, Just y ) ->
            set x y

        _ ->
            Nothing
    )
        |> Maybe.map (\( x, y ) -> Metrics rect x y)


domMetricsOn : (DOMState -> m) -> String -> Attribute m
domMetricsOn f name =
    Html.Events.on name (Json.map f geometryDecoder)


geometryDecoder : Json.Decoder DOMState
geometryDecoder =
    Json.map6 DOMState
        (field "currentTarget" DOM.boundingClientRect)
        (Json.maybe (field "clientX" Json.float))
        (Json.maybe (field "clientY" Json.float))
        (Json.maybe (at [ "touches", "0", "clientX" ] Json.float))
        (Json.maybe (at [ "touches", "0", "clientY" ] Json.float))
        (field "type" Json.string)
