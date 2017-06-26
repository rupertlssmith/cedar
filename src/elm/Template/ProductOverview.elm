module Template.ProductOverview exposing (template)

import Html exposing (Html, div, h4, span, text, a)
import Html.Attributes exposing (attribute, class, name)
import Model exposing (Content(Content), ContentModel(..), Panel(..))
import Renderer.Flexi exposing (Template, Editor)
import MultiwayTreeZipper as Zipper exposing (Zipper)
import TreeUtils exposing (foldToRight, foldToNext)
import Maybe.Extra


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
                , class "layout-fixed-width"
                ]
                (List.concat
                    [ [ div [ class "layout-spacer" ] [] ]
                    , (Zipper.goToChild 0 model |> productCards editor)
                    , (Zipper.goToChild 0 model |> features editor)
                    ]
                )
            ]


productCards : Editor msg -> Maybe (Zipper Content) -> List (Html msg)
productCards editor maybeContainer =
    case maybeContainer of
        Nothing ->
            []

        Just container ->
            [ div [ class "thesett-card-container mdl-grid" ]
                (foldToRight (\zipper -> \accum -> (productCard editor zipper) :: accum)
                    []
                    container
                )
            ]


productCard : Editor msg -> Zipper Content -> Html msg
productCard editor zipper =
    let
        (Content product) =
            Zipper.datum zipper
    in
        case product.model of
            PanelAsContentModel (Panel panel) ->
                div [ class "mdl-cell mdl-cell--6-col mdl-cell--4-col-tablet mdl-cell--4-col-phone mdl-card mdl-shadow--3dp" ]
                    [ div [ class "mdl-card__title" ]
                        [ h4
                            [ class "mdl-card__title-text"
                            , attribute "data-content-id" <| Maybe.withDefault "" product.id
                            , attribute "data-content-path" "title"
                            ]
                            [ text panel.title ]
                        ]
                    , div [ class "mdl-card__supporting-text" ]
                        [ span
                            [ class "mdl-typography--font-light mdl-typography--subhead"
                            , attribute "data-content-id" <| Maybe.withDefault "" product.id
                            , attribute "data-content-path" "markdown"
                            ]
                            [ editor zipper ]
                        ]
                    ]

            _ ->
                div [] []


features : Editor msg -> Maybe (Zipper Content) -> List (Html msg)
features editor maybeContainer =
    let
        featuresOfProduct =
            foldToRight (\zipper -> \accum -> (feature editor zipper) :: accum) []
    in
        case maybeContainer of
            Nothing ->
                []

            Just container ->
                List.concat
                    (foldToRight
                        (\zipper ->
                            \accum ->
                                (Maybe.Extra.unwrap [] featuresOfProduct (Zipper.goToChild 0 zipper)) :: accum
                        )
                        []
                        container
                    )


feature : Editor msg -> Zipper Content -> Html msg
feature editor zipper =
    let
        (Content feature) =
            Zipper.datum zipper
    in
        case feature.model of
            PanelAsContentModel (Panel panel) ->
                div
                    [ class "row"
                    , attribute "data-content-id" <| Maybe.withDefault "" feature.id
                    , attribute "data-content-path" "markdown"
                    ]
                    [ h4
                        [ attribute "data-content-id" <| Maybe.withDefault "" feature.id
                        , attribute "data-content-path" "title"
                        ]
                        [ text panel.title ]
                    , editor zipper
                    ]

            _ ->
                div [] []
