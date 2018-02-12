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
        , toActiveWithControllerAndValue
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
    | Aware (State { active : Allowed } { pos : Position })
    | Active (State { inactive : Allowed } { pos : Position, controller : ControlBar.Model, val : String })
    | Inactive (State { active : Allowed } { pos : Position, controller : ControlBar.Model })



-- State constructors.


hidden : OverlayState
hidden =
    State {} |> Hidden


aware : Position -> OverlayState
aware position =
    State { pos = position } |> Aware


active : Position -> ControlBar.Model -> String -> OverlayState
active position controller value =
    State { pos = position, controller = controller, val = value } |> Active


inactive : Position -> ControlBar.Model -> OverlayState
inactive position controller =
    State { pos = position, controller = controller } |> Inactive



-- Map functions


mapPos : (a -> b) -> ({ m | pos : a } -> { m | pos : b })
mapPos func =
    \model -> { model | pos = func model.pos }


mapController : (a -> b) -> ({ m | controller : a } -> { m | controller : b })
mapController func =
    \model -> { model | controller = func model.controller }


mapVal : (a -> b) -> ({ m | val : a } -> { m | val : b })
mapVal func =
    \model -> { model | val = func model.val }


mapPosition :
    (Position -> Position)
    -> State p { m | pos : Position }
    -> State p { m | pos : Position }
mapPosition func state =
    map (mapPos func) state


mapControlBar :
    (ControlBar.Model -> ControlBar.Model)
    -> State p { m | controller : ControlBar.Model }
    -> State p { m | controller : ControlBar.Model }
mapControlBar func state =
    map (mapController func) state


mapValue :
    (String -> String)
    -> State p { m | val : String }
    -> State p { m | val : String }
mapValue func state =
    map (mapVal func) state



-- State transition functions that can be applied only to states that are permitted
-- to make a transition.


toAwareWithPosition : Position -> State { a | aware : Allowed } m -> OverlayState
toAwareWithPosition position _ =
    aware position


toActiveWithControllerAndValue : ControlBar.Model -> String -> State { a | aware : Allowed } { m | pos : Position } -> OverlayState
toActiveWithControllerAndValue controller value (State model) =
    active model.pos controller value


toActiveWithValue : String -> State { a | active : Allowed } { m | pos : Position, controller : ControlBar.Model } -> OverlayState
toActiveWithValue value (State model) =
    active model.pos model.controller value


toInactive : State { a | inactive : Allowed } { m | pos : Position, controller : ControlBar.Model } -> OverlayState
toInactive (State model) =
    inactive model.pos model.controller
