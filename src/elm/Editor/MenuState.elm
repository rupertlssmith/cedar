module Editor.MenuState
    exposing
        ( MenuState(..)
        , Menu
          -- Convenience re-exports from StateMachine
        , State
        , Allowed
        , untag
          -- Constructors
        , disabled
          -- Map
        , mapSlideButtonStyle
        , mapMenu
          -- State transitions
        , toAvailableWithMenu
        , toAvailable
        , toOpen
        )

import Animation
import Editor.ControlBar as ControlBar
import Editor.ContentTree as ContentTree
import StateMachine exposing (State(..), Allowed, map)


untag : State tag value -> value
untag =
    StateMachine.untag


type alias State p m =
    StateMachine.State p m


type alias Allowed =
    StateMachine.Allowed


type alias Menu =
    { menuStyle : Animation.State
    , controlBar : ControlBar.Model
    , contentTree : ContentTree.Model
    }


type MenuState
    = Disabled (State { available : Allowed } { anim : Animation.State })
    | Available (State { open : Allowed } { anim : Animation.State, controls : Menu })
    | Open (State { available : Allowed } { anim : Animation.State, controls : Menu })



-- State constructors.


disabled : Animation.State -> MenuState
disabled slideButtonStyle =
    State { anim = slideButtonStyle } |> Disabled


available : Animation.State -> Menu -> MenuState
available slideButtonStyle menu =
    State { anim = slideButtonStyle, controls = menu } |> Available


open : Animation.State -> Menu -> MenuState
open slideButtonStyle menu =
    State { anim = slideButtonStyle, controls = menu } |> Open



-- Map functions


mapAnim : (a -> b) -> ({ m | anim : a } -> { m | anim : b })
mapAnim func =
    \model -> { model | anim = func model.anim }


mapControls : (a -> b) -> ({ m | controls : a } -> { m | controls : b })
mapControls func =
    \model -> { model | controls = func model.controls }


mapSlideButtonStyle :
    (Animation.State -> Animation.State)
    -> State p { m | anim : Animation.State }
    -> State p { m | anim : Animation.State }
mapSlideButtonStyle func state =
    map (mapAnim func) state


mapMenu :
    (Menu -> Menu)
    -> State p { m | controls : Menu }
    -> State p { m | controls : Menu }
mapMenu func state =
    map (mapControls func) state



-- State transition functions that can be applied only to states that are permitted
-- to make a transition.


toAvailableWithMenu : Menu -> State { a | available : Allowed } { m | anim : Animation.State } -> MenuState
toAvailableWithMenu menu (State model) =
    available model.anim menu


toAvailable : State { a | available : Allowed } { m | anim : Animation.State, controls : Menu } -> MenuState
toAvailable (State model) =
    available model.anim model.controls


toOpen : State { a | open : Allowed } { m | anim : Animation.State, controls : Menu } -> MenuState
toOpen (State model) =
    available model.anim model.controls
