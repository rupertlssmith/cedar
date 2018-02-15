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
        , mapInlineEditorStyle
          -- State transitions
        , toExploreWithContent
        , toExplore
        , toMarkdown
        , toMarkdownWithSelectedModel
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
    | Markdown (State { preview : Allowed, wysiwyg : Allowed, explore : Allowed } { contentItem : Content, selected : SelectedModel })
    | Preview (State { markdown : Allowed, wysiwyg : Allowed, explore : Allowed } { contentItem : Content, selected : SelectedModel })
    | Wysiwyg (State { markdown : Allowed, preview : Allowed, explore : Allowed } { contentItem : Content, selected : SelectedModel, inlineEditorStyle : Animation.State })



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
    State { contentItem = content, selected = selected, inlineEditorStyle = style } |> Wysiwyg



-- Map functions


mapContent : (Content -> Content) -> ModeState -> ModeState
mapContent func modeState =
    let
        mapField func =
            \model -> { model | contentItem = func model.contentItem }
    in
        case modeState of
            Explore state ->
                map (mapField func) state |> Explore

            Markdown state ->
                map (mapField func) state |> Markdown

            Preview state ->
                map (mapField func) state |> Preview

            Wysiwyg state ->
                map (mapField func) state |> Wysiwyg

            _ ->
                modeState


mapSelectedModel : (SelectedModel -> SelectedModel) -> ModeState -> ModeState
mapSelectedModel func modeState =
    let
        mapField func =
            \model -> { model | selected = func model.selected }
    in
        case modeState of
            Markdown state ->
                map (mapField func) state |> Markdown

            Preview state ->
                map (mapField func) state |> Preview

            Wysiwyg state ->
                map (mapField func) state |> Wysiwyg

            _ ->
                modeState


mapInlineEditorStyle : (Animation.State -> Animation.State) -> ModeState -> ModeState
mapInlineEditorStyle func modeState =
    let
        mapField func =
            \model -> { model | inlineEditorStyle = func model.inlineEditorStyle }
    in
        case modeState of
            Wysiwyg state ->
                map (mapField func) state |> Wysiwyg

            _ ->
                modeState



-- State transition functions that can be applied only to states that are permitted
-- to make a transition.


toExploreWithContent : Content -> State { a | explore : Allowed } m -> ModeState
toExploreWithContent content _ =
    explore content


toExplore : State { a | explore : Allowed } { m | contentItem : Content } -> ModeState
toExplore (State model) =
    explore model.contentItem


toMarkdownWithSelectedModel : SelectedModel -> State { a | markdown : Allowed } { m | contentItem : Content } -> ModeState
toMarkdownWithSelectedModel selected (State model) =
    markdown model.contentItem selected


toMarkdown : State { a | markdown : Allowed } { m | contentItem : Content, selected : SelectedModel } -> ModeState
toMarkdown (State model) =
    markdown model.contentItem model.selected


toPreview : State { a | preview : Allowed } { m | contentItem : Content, selected : SelectedModel } -> ModeState
toPreview (State model) =
    preview model.contentItem model.selected


toWysiwyg : Animation.State -> State { a | wysiwyg : Allowed } { m | contentItem : Content, selected : SelectedModel } -> ModeState
toWysiwyg inlineEditorStyle (State model) =
    wysiwyg model.contentItem model.selected inlineEditorStyle
