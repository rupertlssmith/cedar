module Layout.ServerWrapper exposing (wrapper)

import Html exposing (Html, node, text, body)
import Html.Attributes exposing (attribute)
import Renderer.Flexi exposing (Layout)


wrapper : String -> Layout msg -> Layout msg
wrapper applicationContextRoot layout template linker editor content =
    node "html"
        [ attribute "lang" "en" ]
        [ head applicationContextRoot
        , body []
            [ layout template linker editor content ]
        ]


head : String -> Html msg
head applicationContextRoot =
    node "head"
        []
        [ node "meta"
            [ attribute "charset" "utf-8" ]
            []
        , node
            "meta"
            [ attribute "http-equiv" "X-UA-Compatible"
            , attribute "content" "IE=edge"
            ]
            []
        , node
            "script"
            [ attribute "type" "application/ld+json" ]
            [ text
                ("{\"@context\": \"http://schema.org\","
                    ++ "\"@type\": \"WebSite\","
                    ++ "\"name\": \"thesett.com\","
                    ++ "\"alternateName\": \"thesett.com\","
                    ++ "\"url\": \"https://www.thesett.com\""
                    ++ "}"
                )
            ]
        , node "title"
            []
            [ text "The Sett" ]
        , node "meta"
            [ attribute "name" "description"
            , attribute "content" ""
            ]
            []
        , node "meta"
            [ attribute "name" "viewport"
            , attribute "content" "width=device-width, initial-scale=1.0, minimum-scale=1.0"
            ]
            []
        , node "link"
            [ attribute "rel" "shortcut icon"
            , attribute "href" "/favicon.ico"
            ]
            []
        , node "link"
            [ attribute "rel" "stylesheet"
            , attribute "type" "text/css"
            , attribute "href" "https://fonts.googleapis.com/css?family=Roboto:400,300,500|Roboto+Mono|Roboto+Condensed:400,700&subset=latin,latin-ext"
            ]
            []
        , node "link"
            [ attribute "rel" "stylesheet"
            , attribute "href" "https://fonts.googleapis.com/icon?family=Material+Icons"
            ]
            []
        , node "link"
            [ attribute "rel" "stylesheet"
            , attribute "href" "https://code.getmdl.io/1.2.0/material.green-indigo.min.css"
            ]
            []
        , node "link"
            [ attribute "rel" "stylesheet"
            , attribute "href" <| applicationContextRoot ++ "thesett-laf/styles/main.css"
            ]
            []
        ]
