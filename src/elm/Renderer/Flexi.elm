module Renderer.Flexi exposing (Layout, Template, LinkBuilder, Editor, view)

{-| Renderer.Flexi produces HTML from templates. The templates are designed such that
they can be rendered on the client side when working interactively with content
or on the server side when they are rendering content statically.

@docs Layout, Template, LinkBuilder, Editor, view

-}

import Dict exposing (Dict)
import Html exposing (Html, node, text)
import Model exposing (Content(Content), ContentModel, ContentType(ContentType))
import MultiwayTree as Tree exposing (Tree(..))
import MultiwayTreeZipper as Zipper exposing (Zipper)
import Renderer.ContentAsTree exposing (contentContainerToTree)


{-| Defines the type of a link builder. This takes a string containing a slug for a
piece of content, and renders it as an HTML href linking to that content.
-}
type alias LinkBuilder msg =
    String -> Html.Attribute msg


{-| Defines the type of a template. A template takes a link builder, an editor and
some content and produces Html.
-}
type alias Template msg =
    LinkBuilder msg -> Editor msg -> Zipper Content -> Html msg


{-| Defines the type of a layout. A layout is a higher level template; it takes a
template as input and produces a template as output.
-}
type alias Layout msg =
    Template msg -> Template msg


{-| Defines the type of a content editor. A content editor can render an interactive
editor for content, or it can simply render the content statically.
-}
type alias Editor msg =
    Zipper Content -> Html msg


{-| Renders content with the layouts, template, link builders and editor all as
parameters.
-}
view : Dict String (Layout msg) -> Dict String (Template msg) -> LinkBuilder msg -> Editor msg -> Model.Content -> Html msg
view layouts templates linker editor (Content content) =
    case content.contentType of
        Nothing ->
            Html.div [] [ Html.text "The content to render has no contentType." ]

        Just (ContentType contentType) ->
            let
                layout =
                    Dict.get contentType.layout layouts

                template =
                    Dict.get contentType.template templates
            in
                case ( layout, template ) of
                    ( Nothing, _ ) ->
                        Html.div [] [ Html.text "Failed to find a matching layout." ]

                    ( _, Nothing ) ->
                        Html.div [] [ Html.text "Failed to find a matching template." ]

                    ( Just layout, Just template ) ->
                        ( (contentContainerToTree identity (Content content)), [] )
                            |> layout template linker editor
