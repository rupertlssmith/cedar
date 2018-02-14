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


mapPosition :
    (Position -> Position)
    -> State p { m | position : Position }
    -> State p { m | position : Position }
mapPosition func state =
    let
        mapField func =
            \model -> { model | position = func model.position }
    in
        map (mapField func) state


mapControlBar :
    (ControlBar.Model -> ControlBar.Model)
    -> State p { m | controlBar : ControlBar.Model }
    -> State p { m | controlBar : ControlBar.Model }
mapControlBar func state =
    let
        mapField func =
            \model -> { model | controlBar = func model.controlBar }
    in
        map (mapField func) state


mapValue :
    (String -> String)
    -> State p { m | value : String }
    -> State p { m | value : String }
mapValue func state =
    let
        mapField func =
            \model -> { model | value = func model.value }
    in
        map (mapField func) state



-- State transition functions that can be applied only to states that are permitted
-- to make a transition.


toAwareWithPosition : Position -> State { a | aware : Allowed } m -> OverlayState
toAwareWithPosition position _ =
    aware position


toActiveWithControlBarAndValue : ControlBar.Model -> String -> State { a | aware : Allowed } { m | pos : Position } -> OverlayState
toActiveWithControlBarAndValue controlBar value (State model) =
    active model.pos controlBar value


toActiveWithValue : String -> State { a | active : Allowed } { m | pos : Position, controlBar : ControlBar.Model } -> OverlayState
toActiveWithValue value (State model) =
    active model.pos model.controlBar value


toInactive : State { a | inactive : Allowed } { m | pos : Position, controlBar : ControlBar.Model } -> OverlayState
toInactive (State model) =
    inactive model.pos model.controlBar
