module Template.Article exposing (template)

import Html exposing (Html, div)
import Html.Attributes exposing (attribute, class)
import Model exposing (Content(Content))
import Renderer.Flexi exposing (Template, Editor)
import MultiwayTreeZipper as Zipper exposing (Zipper)


template : Template msg
template linker editor model =
    let
        (Content content) =
            Zipper.datum model

        id =
            Maybe.withDefault "" content.id
    in
        div [ class "mdl-layout__content" ]
            [ div
                [ attribute "data-content" id
                , attribute "data-content-path" "model.markdown"
                , class "layout-fixed-width"
                ]
                [ div [ class "layout-spacer" ] []
                , article editor model
                ]
            ]


article : Editor msg -> Zipper Content -> Html msg
article editor article =
    editor article
