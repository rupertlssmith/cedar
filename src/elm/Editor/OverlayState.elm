module Editor.OverlayState
    exposing
        ( OverlayState(..)
        , Position
          -- Convenience re-exports from StateMachine
        , State
        , Allowed
        , untag
          -- Constructors
        , hidden
          -- Map
        , mapPosition
        , mapValue
        , mapControlBar
          -- State transitions
        , toAwareWithPosition
        , toActiveWithControlBarAndValue
        , toActiveWithValue
        , toInactive
        )

import Animation
import DOM exposing (Rectangle)
import Editor.ControlBar as ControlBar
import StateMachine exposing (State(..), Allowed, map)


untag : State tag value -> value
untag =
    StateMachine.untag


type alias State p m =
    StateMachine.State p m


type alias Allowed =
    StateMachine.Allowed


type alias Position =
    { rect : Rectangle
    , yOffset : Float
    , overlayStyle : Animation.State
    }


{-| Note that the Hidden state is effectively a reset on the state machine,
and is allowed from any state, so it is not marked explcitly here.
-}
type OverlayState
    = Hidden (State { aware : Allowed } {})
    | Aware (State { active : Allowed } { position : Position })
    | Active (State { inactive : Allowed } { position : Position, controlBar : ControlBar.Model, value : String })
    | Inactive (State { active : Allowed } { position : Position, controlBar : ControlBar.Model })



-- State constructors.


hidden : OverlayState
hidden =
    State {} |> Hidden


aware : Position -> OverlayState
aware position =
    State { position = position } |> Aware


active : Position -> ControlBar.Model -> String -> OverlayState
active position controlBar value =
    State { position = position, controlBar = controlBar, value = value } |> Active


inactive : Position -> ControlBar.Model -> OverlayState
inactive position controlBar =
    State { position = position, controlBar = controlBar } |> Inactive



-- Map functions


mapPosition : (Position -> Position) -> OverlayState -> OverlayState
mapPosition func overlayState =
    let
        mapField func =
            \model -> { model | position = func model.position }
    in
        case overlayState of
            Aware state ->
                map (mapField func) state |> Aware

            Active state ->
                map (mapField func) state |> Active

            Inactive state ->
                map (mapField func) state |> Inactive

            _ ->
                overlayState


mapControlBar : (ControlBar.Model -> ControlBar.Model) -> OverlayState -> OverlayState
mapControlBar func overlayState =
    let
        mapField func =
            \model -> { model | controlBar = func model.controlBar }
    in
        case overlayState of
            Active state ->
                map (mapField func) state |> Active

            Inactive state ->
                map (mapField func) state |> Inactive

            _ ->
                overlayState


mapValue : (String -> String) -> OverlayState -> OverlayState
mapValue func overlayState =
    let
        mapField func =
            \model -> { model | value = func model.value }
    in
        case overlayState of
            Active state ->
                map (mapField func) state |> Active

            _ ->
                overlayState



-- State transition functions that can be applied only to states that are permitted
-- to make a transition.


toAwareWithPosition : Position -> State { a | aware : Allowed } m -> OverlayState
toAwareWithPosition position _ =
    aware position


toActiveWithControlBarAndValue : ControlBar.Model -> String -> State { a | active : Allowed } { m | position : Position } -> OverlayState
toActiveWithControlBarAndValue controlBar value (State model) =
    active model.position controlBar value


toActiveWithValue : String -> State { a | active : Allowed } { m | position : Position, controlBar : ControlBar.Model } -> OverlayState
toActiveWithValue value (State model) =
    active model.position model.controlBar value


toInactive : State { a | inactive : Allowed } { m | position : Position, controlBar : ControlBar.Model } -> OverlayState
toInactive (State model) =
    inactive model.position model.controlBar
