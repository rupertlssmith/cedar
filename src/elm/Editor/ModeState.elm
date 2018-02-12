module Editor.ModeState
    exposing
        ( ModeState(..)
          -- Convenience re-exports from StateMachine
        , State
        , Allowed
        , untag
          -- Constructors
        , loading
          -- Map
        , mapContent
        , mapSelectedModel
        , mapEditorStyle
          -- State transitions
        , toExplore
        , toMarkdown
        , toPreview
        , toWysiwyg
        )

import Animation
import DOM exposing (Rectangle)
import Editor.ControlBar as ControlBar
import Editor.Overlay as Overlay
import Model exposing (Content(..))
import MultiwayTreeZipper as Zipper exposing (Zipper)
import StateMachine exposing (State(..), Allowed, map)


untag : State tag value -> value
untag =
    StateMachine.untag


type alias State p m =
    StateMachine.State p m


type alias Allowed =
    StateMachine.Allowed


type alias SelectedModel =
    { selectedContent : Zipper Content
    , overlay : Overlay.Model
    , editedValue : Maybe String
    }


type ModeState
    = Loading (State { explore : Allowed } {})
    | Explore (State { markdown : Allowed } { contentItem : Content })
    | Markdown (State { preview : Allowed, wysiwyg : Allowed } { contentItem : Content, selected : SelectedModel })
    | Preview (State { markdown : Allowed, wysiwyg : Allowed } { contentItem : Content, selected : SelectedModel })
    | Wysiwyg (State { markdown : Allowed, preview : Allowed } { contentItem : Content, selected : SelectedModel, anim : Animation.State })



-- State constructors.


loading : ModeState
loading =
    State {} |> Loading


explore : Content -> ModeState
explore content =
    State { contentItem = content } |> Explore


markdown : Content -> SelectedModel -> ModeState
markdown content selected =
    State { contentItem = content, selected = selected } |> Markdown


preview : Content -> SelectedModel -> ModeState
preview content selected =
    State { contentItem = content, selected = selected } |> Preview


wysiwyg : Content -> SelectedModel -> Animation.State -> ModeState
wysiwyg content selected style =
    State { contentItem = content, selected = selected, anim = style } |> Wysiwyg



-- Map functions


mapContentItem : (a -> b) -> ({ m | contentItem : a } -> { m | contentItem : b })
mapContentItem func =
    \model -> { model | contentItem = func model.contentItem }


mapSelected : (a -> b) -> ({ m | selected : a } -> { m | selected : b })
mapSelected func =
    \model -> { model | selected = func model.selected }


mapAnim : (a -> b) -> ({ m | anim : a } -> { m | anim : b })
mapAnim func =
    \model -> { model | anim = func model.anim }


mapContent :
    (Content -> Content)
    -> State p { m | contentItem : Content }
    -> State p { m | contentItem : Content }
mapContent func state =
    map (mapContentItem func) state


mapSelectedModel :
    (selectedModel -> selectedModel)
    -> State p { m | selected : selectedModel }
    -> State p { m | selected : selectedModel }
mapSelectedModel func state =
    map (mapSelected func) state


mapEditorStyle :
    (Animation.State -> Animation.State)
    -> State p { m | anim : Animation.State }
    -> State p { m | anim : Animation.State }
mapEditorStyle func state =
    map (mapAnim func) state



-- State transition functions that can be applied only to states that are permitted
-- to make a transition.


toExplore : Content -> State { a | explore : Allowed } m -> ModeState
toExplore content _ =
    explore content


toMarkdown : SelectedModel -> State { a | markdown : Allowed } { m | contentItem : Content } -> ModeState
toMarkdown selected (State model) =
    markdown model.contentItem selected


toPreview : State { a | preview : Allowed } { m | contentItem : Content, selected : SelectedModel } -> ModeState
toPreview (State model) =
    preview model.contentItem model.selected


toWysiwyg : Animation.State -> State { a | wysiwyg : Allowed } { m | contentItem : Content, selected : SelectedModel } -> ModeState
toWysiwyg anim (State model) =
    wysiwyg model.contentItem model.selected anim
