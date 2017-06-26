module Layout.Standard exposing (layout)

import Html exposing (Html, node, text, div, button, a, nav, body)
import Html.Attributes exposing (attribute, class, href, id)
import Model exposing (Content)
import Renderer.Flexi exposing (Template, Layout, LinkBuilder, Editor)
import MultiwayTreeZipper as Zipper exposing (Zipper)


layout : Layout msg
layout template linker editor content =
    pageBody template linker editor content


pageBody : Template msg -> LinkBuilder msg -> Editor msg -> Zipper Content -> Html msg
pageBody template linker editor model =
    div [ class "mdl-layout mdl-js-layout mdl-layout--fixed-header" ]
        [ topHeader linker, template linker editor model, footer, footerScripts ]


topHeader : LinkBuilder msg -> Html msg
topHeader linker =
    div [ class "mdl-layout__header mdl-layout__header--waterfall" ]
        [ div [ class "mdl-layout__header-row" ]
            [ a [ linker "overview", id "thesett-logo" ]
                []
            , div [ class "mdl-layout-spacer" ]
                []
            , nav [ class "mdl-navigation" ]
                [ a [ class "mdl-navigation__link mdl-typography--text-uppercase", href "/style-lab" ]
                    [ text "Style Lab" ]
                , a [ class "mdl-navigation__link mdl-typography--text-uppercase", linker "about" ]
                    [ text "What is this?" ]
                ]
            , div [ class "mdl-layout-spacer" ]
                []
            ]
        ]


footer : Html msg
footer =
    node "footer" [ class "thesett-footer mdl-mega-footer" ] []


footerScripts : Html msg
footerScripts =
    node "script" [ attribute "src" "https://storage.googleapis.com/code.getmdl.io/1.2.0/material.min.js" ] []
