module Editor.ContentTree exposing
    ( Model
    , Msg
    , OutMsg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Animation exposing (Interpolation, Property, State, percent, px)
import AnimationUtil exposing (animateStyle)
import Ease
import Editor.ControlBar as ControlBar
import Html exposing (Html, div, i, li, span, text, ul)
import Html.Attributes exposing (class, href)
import Html.Events as Events
import Maybe.Extra exposing (unwrap)
import Model exposing (Content(..), Relationship(..))
import MultiwayTree as Tree exposing (Tree(..))
import MultiwayTreeZipper as Zipper exposing (Zipper)
import Renderer.ContentAsTree exposing (contentRelationshipsToTree)
import Time exposing (Time, second)
import TreeUtils exposing ((&>), updateTree)



-- Data model


type Msg
    = ControlBarUpdate String ControlBar.Msg
    | MouseOverNode (Zipper ContentNode)
    | MouseOutTree
    | ToggleOpen (Zipper ContentNode)
    | SelectLocation (Zipper ContentNode) String
    | ControlBar ControlBar.OutMsg


type OutMsg
    = Navigate String


type alias Model =
    { contentTree : ContentTree
    , hovered : Maybe (Zipper ContentNode)
    , selected : Maybe (Zipper ContentNode)
    }


type alias ContentTree =
    Tree ContentNode


type alias ContentNode =
    { id : String
    , slug : String
    , open : Bool
    , controlBar : ControlBar.Model
    }



-- Initialization


init : Content -> Maybe Content -> Model
init content selectedContent =
    { contentTree = contentToTree content selectedContent
    , hovered = Nothing
    , selected = Nothing
    }


makeNode : Maybe Content -> Content -> ContentNode
makeNode selectedContent (Content content) =
    let
        selected =
            unwrap False (\(Content sel) -> sel.id == content.id) selectedContent

        id =
            Maybe.withDefault "" content.id

        controlBar =
            ControlBar.initWithAnimation id
                controlBarHiddenStyle
                controlBarShownStyle
                controlBarShowEasing
                controlBarHideEasing
                selected
                [ ( "add", "control-icon control-icon__add" ) ]
    in
    { id = id
    , slug = Maybe.withDefault "" content.slug
    , open = True
    , controlBar = controlBar
    }


contentToTree : Content -> Maybe Content -> ContentTree
contentToTree content selectedContent =
    contentRelationshipsToTree (makeNode selectedContent) content



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (List.map
            (\( id, model ) -> Sub.map (ControlBarUpdate id) <| ControlBar.subscriptions model)
            (collectControlBars model.contentTree)
        )


collectControlBars : ContentTree -> List ( String, ControlBar.Model )
collectControlBars tree =
    Tree.foldl (\node -> \accum -> ( node.id, node.controlBar ) :: accum) [] tree



-- Animation Styles


controlBarHiddenStyle : List Property
controlBarHiddenStyle =
    [ Animation.left (px 24.0)
    ]


controlBarShownStyle : List Property
controlBarShownStyle =
    [ Animation.left (px -6.0)
    ]


controlBarShowEasing : Interpolation
controlBarShowEasing =
    Animation.easing
        { duration = 8.0e-2 * second
        , ease = Ease.inOut Ease.inQuad Ease.outBack
        }


controlBarHideEasing : Interpolation
controlBarHideEasing =
    Animation.easing
        { duration = 1.2e-1 * second
        , ease = Ease.inOut Ease.inBack Ease.outQuad
        }



-- State updates


update : Msg -> Model -> ( Model, Maybe OutMsg )
update action model =
    case action of
        ControlBarUpdate id msg ->
            let
                controlBarZipper =
                    Zipper.goTo (\node -> id == node.id)
                        ( model.contentTree, [] )
            in
            case controlBarZipper of
                Nothing ->
                    ( model, Nothing )

                Just zipper ->
                    let
                        node =
                            Zipper.datum zipper

                        newControlBar =
                            ControlBar.update msg node.controlBar

                        newRootZipper =
                            Zipper.updateDatum (\node -> { node | controlBar = newControlBar }) zipper
                                &> Zipper.goToRoot
                    in
                    case newRootZipper of
                        Nothing ->
                            ( model, Nothing )

                        Just ( newContentTree, _ ) ->
                            ( { model | contentTree = newContentTree }, Nothing )

        MouseOverNode zipper ->
            ( { model | hovered = Just zipper }, Nothing )

        MouseOutTree ->
            ( { model | hovered = Nothing }, Nothing )

        ToggleOpen zipper ->
            case toggleOpenState zipper of
                Nothing ->
                    ( model, Nothing )

                Just newTree ->
                    ( { model | contentTree = newTree }, Nothing )

        SelectLocation zipper location ->
            let
                node =
                    Zipper.datum zipper

                newTree =
                    updateTree (\node -> { node | controlBar = ControlBar.show node.controlBar }) zipper
            in
            case newTree of
                Nothing ->
                    ( model, Nothing )

                Just newTree ->
                    ( { model | contentTree = newTree |> hideAllControlBarsExcept node.id, selected = Just zipper }
                    , Just <| Navigate location
                    )

        ControlBar outMsg ->
            ( model, Nothing )


toggleOpenState : Zipper ContentNode -> Maybe ContentTree
toggleOpenState =
    updateTree (\node -> { node | open = not node.open })


hideAllControlBarsExcept : String -> ContentTree -> ContentTree
hideAllControlBarsExcept id tree =
    Tree.map
        (\node ->
            { node
                | controlBar =
                    if node.id == id then
                        node.controlBar

                    else
                        ControlBar.hide node.controlBar
            }
        )
        tree



-- View


view : Maybe Content -> Model -> Html Msg
view selectedContent model =
    div
        [ class "tree-container"
        , Events.onMouseLeave MouseOutTree
        ]
        [ ul [ class "tree-wholerow__ul" ]
            [ tree selectedContent
                model.hovered
                ( model.contentTree, [] )
            ]
        ]


hoverClass : Maybe String -> String -> String
hoverClass hovered nodeId =
    case hovered of
        Nothing ->
            ""

        Just id ->
            if nodeId == id then
                " tree-wholerow__hovered"

            else
                ""


selectedClass : Maybe Content -> String -> String
selectedClass selectedContent nodeId =
    case selectedContent of
        Nothing ->
            ""

        Just (Content content) ->
            if Just nodeId == content.id then
                " tree-wholerow__selected"

            else
                ""


wholeRowDiv : Maybe Content -> Maybe (Zipper ContentNode) -> Zipper ContentNode -> Html Msg
wholeRowDiv selectedContent hovered zipper =
    let
        node =
            Zipper.datum zipper

        hoveredId =
            Maybe.map Zipper.datum hovered
                |> Maybe.map .id
    in
    div
        [ class <|
            "tree-wholerow"
                ++ hoverClass hoveredId node.id
                ++ selectedClass selectedContent node.id
        , Events.onMouseOver <| MouseOverNode zipper
        , Events.onClick <| SelectLocation zipper node.slug
        ]
        [ Html.map ControlBar <| ControlBar.view "tree-wholerow__controlbar" node.controlBar ]


expandIcon : Zipper ContentNode -> Html Msg
expandIcon zipper =
    let
        node =
            Zipper.datum zipper
    in
    if node.open then
        i
            [ class "control-icon control-icon__open"
            , Events.onMouseOver <| MouseOverNode zipper
            , Events.onClick <| ToggleOpen zipper
            ]
            []

    else
        i
            [ class "control-icon control-icon__closed"
            , Events.onMouseOver <| MouseOverNode zipper
            , Events.onClick <| ToggleOpen zipper
            ]
            []


itemHtml : Zipper ContentNode -> Html Msg
itemHtml zipper =
    let
        node =
            Zipper.datum zipper
    in
    span
        [ class "tree-anchor"
        , href <| "#" ++ node.slug
        , Events.onMouseOver <| MouseOverNode zipper
        , Events.onClick <| SelectLocation zipper node.slug
        ]
        [ text node.slug ]


tree : Maybe Content -> Maybe (Zipper ContentNode) -> Zipper ContentNode -> Html Msg
tree selectedContent hovered zipper =
    let
        innerTree (( Tree node children, _ ) as zipper) =
            case ( children, node.open ) of
                ( [], _ ) ->
                    li []
                        [ wholeRowDiv selectedContent hovered zipper
                        , i
                            [ class "control-icon"
                            , Events.onMouseOver <| MouseOverNode zipper
                            , Events.onClick <| SelectLocation zipper node.slug
                            ]
                            []
                        , itemHtml zipper
                        ]

                ( children, False ) ->
                    li []
                        [ wholeRowDiv selectedContent hovered zipper
                        , expandIcon zipper
                        , itemHtml zipper
                        ]

                ( children, _ ) ->
                    li []
                        [ wholeRowDiv selectedContent hovered zipper
                        , expandIcon zipper
                        , itemHtml zipper
                        , ul [ class "tree-wholerow__ul" ]
                            (List.filterMap identity
                                (List.indexedMap
                                    (\index ->
                                        \child ->
                                            Just zipper
                                                &> Zipper.goToChild index
                                                &> (\zipper -> Just (innerTree zipper))
                                    )
                                    children
                                )
                            )
                        ]
    in
    innerTree zipper
