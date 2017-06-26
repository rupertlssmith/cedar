port module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (attribute, class, name, href, id)
import Markdown
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder, field)
import Json.Decode.Extra exposing ((|:), withDefault)
import ServerSide.Static exposing (StringProgram, htmlToStringProgram)
import Model
import Renderer.Flexi exposing (LinkBuilder, Editor)
import ModelUtils exposing (asMarkdown)
import Templates exposing (ssLayouts, templates)
import MultiwayTreeZipper as Zipper exposing (Zipper)
import Config exposing (Config, configDecoder)


type alias Model =
    { config : Config
    , content : Model.Content
    }


main : StringProgram
main =
    htmlToStringProgram { init = init }


init : Value -> Html Never
init json =
    let
        result =
            Decode.decodeValue modelDecoder json
    in
        case result of
            Ok model ->
                let
                    linker : LinkBuilder msg
                    linker slug =
                        href <| model.config.applicationContextRoot ++ "content/" ++ slug
                in
                    Html.map never <| Renderer.Flexi.view (ssLayouts model.config) templates linker renderer model.content

            Err _ ->
                Html.div [] [ Html.text <| "Failed to decode the content model: " ++ (toString json) ]


renderer : Editor msg
renderer zipper =
    let
        (Model.Content content) =
            Zipper.datum zipper
    in
        content |> .model |> asMarkdown |> Markdown.toHtml []


modelDecoder : Decoder Model
modelDecoder =
    (Decode.succeed
        (\config content ->
            { config = config
            , content = content
            }
        )
    )
        |: field "config" Config.configDecoder
        |: field "content" Model.contentDecoder
