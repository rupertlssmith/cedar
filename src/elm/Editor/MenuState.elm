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
    = Disabled (State { available : Allowed } { slideButtonStyle : Animation.State })
    | Available (State { open : Allowed } { slideButtonStyle : Animation.State, controls : Menu })
    | Open (State { available : Allowed } { slideButtonStyle : Animation.State, controls : Menu })



-- State constructors.


disabled : Animation.State -> MenuState
disabled slideButtonStyle =
    State { slideButtonStyle = slideButtonStyle } |> Disabled


available : Animation.State -> Menu -> MenuState
available slideButtonStyle menu =
    State { slideButtonStyle = slideButtonStyle, controls = menu } |> Available


open : Animation.State -> Menu -> MenuState
open slideButtonStyle menu =
    State { slideButtonStyle = slideButtonStyle, controls = menu } |> Open



-- Map functions


mapSlideButtonStyle : (Animation.State -> Animation.State) -> MenuState -> MenuState
mapSlideButtonStyle func menuState =
    let
        mapField func =
            \model -> { model | slideButtonStyle = func model.slideButtonStyle }
    in
        case menuState of
            Disabled state ->
                map (mapField func) state |> Disabled

            Available state ->
                map (mapField func) state |> Available

            Open state ->
                map (mapField func) state |> Open


mapMenu : (Menu -> Menu) -> MenuState -> MenuState
mapMenu func menuState =
    let
        mapField func =
            \model -> { model | controls = func model.controls }
    in
        case menuState of
            Available state ->
                map (mapField func) state |> Available

            Open state ->
                map (mapField func) state |> Open

            _ ->
                menuState



-- State transition functions that can be applied only to states that are permitted
-- to make a transition.


toAvailableWithMenu : Menu -> State { a | available : Allowed } { m | slideButtonStyle : Animation.State } -> MenuState
toAvailableWithMenu menu (State model) =
    available model.slideButtonStyle menu


toAvailable : State { a | available : Allowed } { m | slideButtonStyle : Animation.State, controls : Menu } -> MenuState
toAvailable (State model) =
    available model.slideButtonStyle model.controls


toOpen : State { a | open : Allowed } { m | slideButtonStyle : Animation.State, controls : Menu } -> MenuState
toOpen (State model) =
    available model.slideButtonStyle model.controls
