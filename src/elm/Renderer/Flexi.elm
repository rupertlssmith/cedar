module Renderer.Flexi exposing (Layout, Template, LinkBuilder, Editor, view)

import Dict exposing (Dict)
import Html exposing (Html, node, text)
import Model exposing (Content(Content), ContentModel, ContentType(ContentType))
import MultiwayTree as Tree exposing (Tree(..))
import MultiwayTreeZipper as Zipper exposing (Zipper)
import Renderer.ContentAsTree exposing (contentContainerToTree)


type alias Layout msg =
    Template msg -> Template msg


type alias Template msg =
    LinkBuilder msg -> Editor msg -> Zipper Content -> Html msg


type alias LinkBuilder msg =
    String -> Html.Attribute msg


type alias Editor msg =
    Zipper Content -> Html msg


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
