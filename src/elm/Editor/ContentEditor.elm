module Editor.ContentEditor
    exposing
        ( Msg
        , Model
        , view
        , update
        , subscriptions
        , init
        , delta2url
        , location2messages
        )

import Animation exposing (px, turn, Property, Interpolation, State)
import AnimationUtil exposing (animateStyle)
import Auth
import Color
import Config exposing (Config)
import Content.Service
import Content.ServiceExtra as CSE
import Dict exposing (Dict)
import DOM exposing (Rectangle)
import DOMUtils exposing (DOMState, domMetricsOn)
import Ease
import Editor.ContentTree as ContentTree
import Editor.ControlBar as ControlBar
import Editor.Overlay as Overlay
import Function exposing (swirll)
import Html.Attributes exposing (class, id, href, src)
import Html.Events as Events
import Html exposing (Html, text, div, button, img)
import Html.Keyed
import Markdown
import Maybe.Extra exposing (isJust)
import Editor.MenuState as MenuState
    exposing
        ( MenuState(..)
        , Menu
        , disabled
        , mapSlideButtonStyle
        , mapMenu
        , toAvailableWithMenu
        , toAvailable
        , toOpen
        )
import Editor.ModeState as ModeState
    exposing
        ( ModeState(..)
        , SelectedModel
        , loading
        , mapContent
        , mapSelectedModel
        , mapInlineEditorStyle
        , toExploreWithContent
        , toExplore
        , toMarkdownWithSelectedModel
        , toMarkdown
        , toPreview
        , toWysiwyg
        )
import Model exposing (Content(..), ContentModel(..))
import ModelUtils exposing (asMarkdown, asUUID, withMarkdown)
import MultiwayTree as Tree exposing (Tree(Tree))
import MultiwayTreeZipper as Zipper exposing (Zipper)
import Navigation
import Optional exposing (optional, required, when)
import Renderer.ContentAsTree exposing (containerTreeToContent)
import Renderer.Flexi exposing (Editor, LinkBuilder, Layout, Template)
import ResizeObserver exposing (ResizeEvent)
import RouteUrl as Routing
import ScrollPort exposing (Scroll, Move)
import Task.Extra exposing (message)
import Time exposing (second, Time)
import TreeUtils exposing (updateTree)
import Update3
import Utils exposing (error)


type Msg
    = Animate Animation.Msg
    | ControlBarUpdate ControlBar.Msg
    | CSEApi CSE.Msg
    | ContentServiceApi Content.Service.Msg
    | ContentTreeMsg ContentTree.Msg
    | OverlayMsg Overlay.Msg
    | Init
    | SelectLocation String
    | MouseOverContent (Zipper Content) DOMState
    | MouseOutContent
    | Resize ResizeEvent
    | BodyScroll Move
    | ToggleMenu
    | ControlBar ControlBar.OutMsg
    | LogOut
    | Error Auth.Msg



-- The complete content editor state.


type alias Model =
    { mode : ModeState
    , menu : MenuState
    , yOffset : Float
    , config : Config
    , userId : String
    }


contentZipperToModel : Zipper Content -> ContentModel
contentZipperToModel zipper =
    let
        (Model.Content content) =
            Zipper.datum zipper
    in
        content.model



-- Initialization


init : Config -> String -> ( Model, Cmd Msg )
init config userId =
    ( { mode = loading
      , menu = disabled <| Animation.style slideButtonClosedStyle
      , yOffset = 0.0
      , config = config
      , userId = userId
      }
    , message Init
    )



-- Subscriptions


subscriptions : ResizeObserver.Resize -> ScrollPort.Scroll -> Model -> Sub Msg
subscriptions resize scroll model =
    let
        menuSubs =
            case model.menu of
                Disabled state ->
                    let
                        untagged =
                            MenuState.untag state
                    in
                        Animation.subscription Animate [ untagged.slideButtonStyle ]

                Available state ->
                    let
                        untagged =
                            MenuState.untag state
                    in
                        Sub.batch
                            [ Animation.subscription Animate
                                [ untagged.slideButtonStyle
                                , untagged.controls.menuStyle
                                ]
                            , ContentTree.subscriptions untagged.controls.contentTree
                                |> Sub.map ContentTreeMsg
                            ]

                Open state ->
                    let
                        untagged =
                            MenuState.untag state
                    in
                        Sub.batch
                            [ Animation.subscription Animate
                                [ untagged.slideButtonStyle
                                , untagged.controls.menuStyle
                                ]
                            , ContentTree.subscriptions untagged.controls.contentTree
                                |> Sub.map ContentTreeMsg
                            ]

        modeSubs =
            case model.mode of
                Markdown state ->
                    let
                        untagged =
                            ModeState.untag state
                    in
                        Overlay.subscriptions untagged.selected.overlay
                            |> Sub.map OverlayMsg

                Preview state ->
                    let
                        untagged =
                            ModeState.untag state
                    in
                        Overlay.subscriptions untagged.selected.overlay
                            |> Sub.map OverlayMsg

                Wysiwyg state ->
                    let
                        untagged =
                            ModeState.untag state
                    in
                        Sub.batch
                            [ Overlay.subscriptions untagged.selected.overlay
                                |> Sub.map OverlayMsg
                            , Animation.subscription Animate
                                [ untagged.inlineEditorStyle ]
                            ]

                _ ->
                    Sub.none
    in
        Sub.batch
            [ menuSubs
            , modeSubs
            , resize |> Sub.map (\event -> Resize event)
            , scroll |> Sub.map (\event -> BodyScroll event)
            ]



-- Working with the content service.


cseCallbacks : CSE.Callbacks Model Msg
cseCallbacks =
    let
        default =
            CSE.callbacks
    in
        { default
            | retrieveWithContainerBySlug = contentLoaded
            , retrieveTree = treeFetched
            , error = error Error
        }


contentLoaded : Content -> Model -> ( Model, Cmd Msg )
contentLoaded content model =
    case model.mode of
        Loading state ->
            ( { model | mode = toExploreWithContent content state }, Cmd.none )

        Explore state ->
            ( { model | mode = mapContent (always content) <| Explore state }, Cmd.none )

        Markdown state ->
            ( { model | mode = toExploreWithContent content state }, Cmd.none )

        Preview state ->
            ( { model | mode = toExploreWithContent content state }, Cmd.none )

        Wysiwyg state ->
            ( { model | mode = toExploreWithContent content state }, Cmd.none )


treeFetched : Content -> Model -> ( Model, Cmd Msg )
treeFetched content model =
    let
        maybeContent =
            case model.mode of
                Explore state ->
                    (ModeState.untag state).contentItem |> Just

                Markdown state ->
                    (ModeState.untag state).contentItem |> Just

                Preview state ->
                    (ModeState.untag state).contentItem |> Just

                Wysiwyg state ->
                    (ModeState.untag state).contentItem |> Just

                _ ->
                    Nothing
    in
        case model.menu of
            Disabled state ->
                ( { model
                    | menu =
                        toAvailableWithMenu
                            { menuStyle = Animation.style menuClosedStyle
                            , controlBar = ControlBar.init "slideInMenu" [ ( "collapseall", "control-icon control-icon__collapse-all" ) ]
                            , contentTree = ContentTree.init content maybeContent
                            }
                            state
                  }
                , Cmd.none
                )

            _ ->
                ( model, Cmd.none )


csCallbacks : Content.Service.Callbacks Model Msg
csCallbacks =
    let
        default =
            Content.Service.callbacks
    in
        { default
            | update = contentLoaded
            , error = error Error
        }



-- Navigation


editorPrefix : String
editorPrefix =
    "#/slug/"


urlOf : Model -> Maybe String
urlOf model =
    let
        slugToUrl (Content content) =
            editorPrefix ++ (Maybe.withDefault "" content.slug)
    in
        case model.mode of
            Explore state ->
                (ModeState.untag state).contentItem |> slugToUrl |> Just

            Markdown state ->
                (ModeState.untag state).contentItem |> slugToUrl |> Just

            Preview state ->
                (ModeState.untag state).contentItem |> slugToUrl |> Just

            Wysiwyg state ->
                (ModeState.untag state).contentItem |> slugToUrl |> Just

            _ ->
                Nothing


delta2url : Model -> Model -> Maybe Routing.UrlChange
delta2url prevModel newModel =
    let
        maybeUrl =
            urlOf newModel
    in
        case maybeUrl of
            Just url ->
                { entry = Routing.NewEntry
                , url = url
                }
                    |> Just

            Nothing ->
                Nothing


location2messages : Navigation.Location -> List Msg
location2messages location =
    [ String.dropLeft (String.length editorPrefix) location.hash |> SelectLocation ]



-- Model updates


debugFilter : Msg -> Msg
debugFilter msg =
    case msg of
        -- Animate _ ->
        --     msg
        --
        -- ControlBarUpdate _ ->
        --     msg
        --
        -- CSEApi _ ->
        --     msg
        --
        -- ContentTreeMsg _ ->
        --     msg
        --
        -- OverlayMsg _ ->
        --     msg
        --
        -- MouseOverContent _ _ ->
        --     msg
        _ ->
            -- Debug.log "contentEditor" msg
            msg


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Auth.Msg )
update action model =
    case (debugFilter action) of
        Animate msg ->
            ( updateAnimate msg model, Cmd.none, Cmd.none )

        CSEApi msg ->
            updateCSEApi msg model
                |> Update3.addOutMsg Cmd.none

        ContentServiceApi msg ->
            updateContentServiceApi msg model
                |> Update3.addOutMsg Cmd.none

        OverlayMsg msg ->
            updateOverlayMsg msg model
                |> Update3.addOutMsg Cmd.none

        Init ->
            updateInit model
                |> Update3.addOutMsg Cmd.none

        SelectLocation location ->
            updateSelectLocation location model
                |> Update3.addOutMsg Cmd.none

        MouseOverContent zipper domState ->
            ( updateMouseOverContent zipper domState model, Cmd.none, Cmd.none )

        MouseOutContent ->
            ( updateMouseOutContent model, Cmd.none, Cmd.none )

        Resize size ->
            ( updateResize size model, Cmd.none, Cmd.none )

        BodyScroll move ->
            ( updateBodyScroll move model, Cmd.none, Cmd.none )

        ContentTreeMsg msg ->
            updateContentTreeMsg msg model
                |> Update3.addOutMsg Cmd.none

        ControlBarUpdate msg ->
            ( updateControlBarUpdate msg model, Cmd.none, Cmd.none )

        ToggleMenu ->
            ( updateToggleMenu model, Cmd.none, Cmd.none )

        ControlBar outMsg ->
            updateControlBar outMsg model
                |> Update3.addOutMsg Cmd.none

        LogOut ->
            --( model, Auth.logout )
            ( model, Cmd.none, Auth.logout )

        Error _ ->
            ( model, Cmd.none, Cmd.none )


{-| Update animations when animated styles are available.
-}
updateAnimate : Animation.Msg -> Model -> Model
updateAnimate msg model =
    let
        updateMenuStyle menu =
            { menu | menuStyle = Animation.update msg menu.menuStyle }

        updateSlideButtonStyle slideButtonStyle =
            Animation.update msg slideButtonStyle

        updateInlineEditorStyle inlineEditorStyle =
            Animation.update msg inlineEditorStyle
    in
        { model
            | menu =
                mapSlideButtonStyle updateSlideButtonStyle model.menu
                    |> mapMenu updateMenuStyle
            , mode = mapInlineEditorStyle updateInlineEditorStyle model.mode
        }


{-| Forward on the content service messages to it.
-}
updateCSEApi : CSE.Msg -> Model -> ( Model, Cmd Msg )
updateCSEApi cseMsg model =
    (CSE.update cseCallbacks cseMsg model)


{-| Forward on the content service messages to it.
-}
updateContentServiceApi : Content.Service.Msg -> Model -> ( Model, Cmd Msg )
updateContentServiceApi msg model =
    (Content.Service.update csCallbacks msg model)


updateModeWithOverlay : String -> Float -> Overlay.OutMsg -> ModeState -> ( ModeState, Cmd Msg )
updateModeWithOverlay apiRoot yOffset outmsg mode =
    case outmsg of
        Overlay.Closed ->
            case mode of
                Markdown state ->
                    ( toExplore state, Cmd.none )

                Preview state ->
                    ( toExplore state, Cmd.none )

                Wysiwyg state ->
                    ( toExplore state, Cmd.none )

                _ ->
                    ( mode, Cmd.none )

        Overlay.SelectMode "markdown" ->
            let
                activateOverlay selected =
                    { selected
                        | overlay =
                            Overlay.makeActive
                                yOffset
                                (selected.editedValue |> Maybe.withDefault (contentZipperToModel selected.selectedContent |> asMarkdown))
                                selected.overlay
                    }
            in
                case mode of
                    Preview state ->
                        ( mapSelectedModel activateOverlay <| toMarkdown state
                        , Cmd.none
                        )

                    Markdown state ->
                        ( mapSelectedModel activateOverlay <| Markdown state
                        , Cmd.none
                        )

                    _ ->
                        ( mode, Cmd.none )

        Overlay.SelectMode "preview" ->
            case mode of
                Markdown state ->
                    ( mapSelectedModel (\selected -> { selected | overlay = Overlay.makeInactive selected.overlay }) <|
                        toPreview state
                    , Cmd.none
                    )

                _ ->
                    ( mode, Cmd.none )

        Overlay.ContentValue value ->
            ( mapSelectedModel (\selected -> { selected | editedValue = Just value }) mode, Cmd.none )

        Overlay.SelectMode "save" ->
            let
                contentFromEdit : String -> Zipper Content -> Content
                contentFromEdit value selectedContent =
                    let
                        (( tree, _ ) as zipper) =
                            selectedContent

                        newTree =
                            TreeUtils.updateTree
                                (\(Content content) ->
                                    Content
                                        { content
                                            | model =
                                                (withMarkdown (contentZipperToModel zipper) value)
                                        }
                                )
                                zipper
                                |> Maybe.withDefault tree
                    in
                        containerTreeToContent newTree

                saveCmd : Content -> Cmd Msg
                saveCmd (Content content) =
                    let
                        id =
                            Maybe.withDefault "" content.id
                    in
                        Content.Service.invokeUpdate apiRoot ContentServiceApi id (Content content)

                saveEdits state =
                    let
                        untagged =
                            ModeState.untag state
                    in
                        case untagged.selected.editedValue of
                            Nothing ->
                                ( mode, Cmd.none )

                            Just value ->
                                let
                                    newContent =
                                        contentFromEdit value untagged.selected.selectedContent
                                in
                                    ( mapContent (always newContent) mode
                                    , saveCmd newContent
                                    )
            in
                case mode of
                    Preview state ->
                        saveEdits state

                    Markdown state ->
                        saveEdits state

                    _ ->
                        ( mode, Cmd.none )

        _ ->
            ( mode, Cmd.none )


{-| Forward on the overlay msg to get an update from it.
Respond to any out messages from the overlay that need attention:
If it is Closed, then switch to the Explore mode,
and detach the resize observer from the content model.
If it is a mode selection then switch and animate to the selected mode.
-}
updateOverlayMsg : Overlay.Msg -> Model -> ( Model, Cmd Msg )
updateOverlayMsg msg model =
    let
        updateOverlay state =
            Update3.lift .overlay (\x m -> { m | overlay = x }) OverlayMsg Overlay.update msg (ModeState.untag state).selected
                |> Update3.mapModel (\selected -> mapSelectedModel (always selected) model.mode)
                |> Update3.evalMaybe (updateModeWithOverlay model.config.apiRoot model.yOffset) Cmd.none
                |> Tuple.mapFirst (\mode -> { model | mode = mode })
    in
        case model.mode of
            Markdown state ->
                updateOverlay state

            Preview state ->
                updateOverlay state

            Wysiwyg state ->
                updateOverlay state

            _ ->
                ( model, Cmd.none )


{-| Fetch the content by its slug.
Fetch the content tree.
-}
updateInit : Model -> ( Model, Cmd Msg )
updateInit model =
    ( model
    , Cmd.batch
        [ CSE.invokeRetrieveTree model.config.apiRoot CSEApi
        , CSE.invokeRetrieveWithContainerBySlug model.config.apiRoot CSEApi "overview"
        ]
    )


updateSelectLocation : String -> Model -> ( Model, Cmd Msg )
updateSelectLocation location model =
    ( model, CSE.invokeRetrieveWithContainerBySlug model.config.apiRoot CSEApi location )


{-| When in explore mode,
switch to markdown mode with the content model
and an overlay in the Aware state.
Ask the resize observer to observe the content under the mouse.
-}
updateMouseOverContent : Zipper Content -> DOMState -> Model -> Model
updateMouseOverContent zipper domState model =
    case model.mode of
        Explore state ->
            { model
                | mode =
                    toMarkdownWithSelectedModel
                        { selectedContent = zipper
                        , overlay = Overlay.makeAware domState.rect Overlay.init
                        , editedValue = Nothing
                        }
                        state
            }

        _ ->
            model


updateMouseOutContent : Model -> Model
updateMouseOutContent model =
    case model.mode of
        Markdown state ->
            { model | mode = toExplore state }

        _ ->
            model


{-| When there is a selected content model with an overlay,
forward changes in the content models rendered size to the overlay.
-}
updateResize : ResizeEvent -> Model -> Model
updateResize size model =
    { model
        | mode =
            mapSelectedModel
                (\selected ->
                    let
                        contentModel =
                            contentZipperToModel selected.selectedContent

                        id =
                            "inline__wrapper" ++ asUUID contentModel
                    in
                        { selected
                            | overlay =
                                if id == size.id then
                                    Overlay.resize size selected.overlay
                                else
                                    selected.overlay
                        }
                )
                model.mode
    }


updateBodyScroll : Move -> Model -> Model
updateBodyScroll ( from, to ) model =
    { model
        | yOffset = to
        , mode =
            mapSelectedModel
                (\selected ->
                    { selected | overlay = Overlay.scroll ( from, to ) selected.overlay }
                )
                model.mode
    }



-- ==== Menu updates ==== --


updateContentTreeMsg : ContentTree.Msg -> Model -> ( Model, Cmd Msg )
updateContentTreeMsg msg model =
    let
        navigateCmd (ContentTree.Navigate location) =
            CSE.invokeRetrieveWithContainerBySlug model.config.apiRoot CSEApi location

        contentTreeAndNav state =
            let
                ( newTree, outMsg ) =
                    ContentTree.update msg (MenuState.untag state).controls.contentTree
            in
                ( { model | menu = mapMenu (\menu -> { menu | contentTree = newTree }) model.menu }
                , Maybe.map navigateCmd outMsg
                    |> Maybe.withDefault Cmd.none
                )
    in
        case model.menu of
            Available state ->
                contentTreeAndNav state

            Open state ->
                contentTreeAndNav state

            _ ->
                ( model, Cmd.none )


updateControlBarUpdate : ControlBar.Msg -> Model -> Model
updateControlBarUpdate msg model =
    { model | menu = mapMenu (\menu -> { menu | controlBar = ControlBar.update msg menu.controlBar }) model.menu }


updateToggleMenu : Model -> Model
updateToggleMenu model =
    let
        updateSlideButtonStyle toStyle fromStyle =
            animateStyle slideButtonEasing fromStyle toStyle

        updateMenuStyle style menu =
            { menu | menuStyle = animateStyle menuEasing menu.menuStyle style }
    in
        case model.menu of
            Available state ->
                { model
                    | menu =
                        toOpen state
                            |> mapSlideButtonStyle (updateSlideButtonStyle slideButtonOpenStyle)
                            |> mapMenu (updateMenuStyle menuOpenStyle)
                }

            Open state ->
                { model
                    | menu =
                        toAvailable state
                            |> mapSlideButtonStyle (updateSlideButtonStyle slideButtonClosedStyle)
                            |> mapMenu (updateMenuStyle menuClosedStyle)
                }

            _ ->
                model


updateControlBar : ControlBar.OutMsg -> Model -> ( Model, Cmd Msg )
updateControlBar outMsg model =
    ( model, Cmd.none )



-- Animation Styles


menuClosedStyle : List Property
menuClosedStyle =
    [ Animation.left (px -360.0)
    ]


menuOpenStyle : List Property
menuOpenStyle =
    [ Animation.left (px 0.0)
    ]


menuEasing : Interpolation
menuEasing =
    Animation.easing
        { duration = 5.0e-2 * second
        , ease = Ease.inOut Ease.inQuad Ease.outBack
        }


slideButtonClosedStyle : List Property
slideButtonClosedStyle =
    [ Animation.rotate (turn 0)
    ]


slideButtonOpenStyle : List Property
slideButtonOpenStyle =
    [ Animation.rotate (turn 0.5)
    ]


slideButtonEasing : Interpolation
slideButtonEasing =
    Animation.easing
        { duration = 1.0e-1 * second
        , ease = Ease.inOutQuad
        }


inlineEditorActiveStyle : List Property
inlineEditorActiveStyle =
    [ Animation.opacity 1.0
    ]


inlineEditorInactiveStyle : List Property
inlineEditorInactiveStyle =
    [ Animation.opacity 0.0
    ]



-- View


emptyDiv : Html msg
emptyDiv =
    div [] []


view :
    Dict String (Layout Msg)
    -> Dict String (Template Msg)
    -> Model
    -> Html Msg
view layouts templates model =
    case model.mode of
        Loading state ->
            div [] [ div [] (menuView model Nothing) ]

        Explore state ->
            let
                untagged =
                    ModeState.untag state
            in
                div []
                    [ div []
                        ((contentView layouts templates model untagged.contentItem)
                            :: (menuView model (Just untagged.contentItem))
                        )
                    ]

        Markdown state ->
            let
                untagged =
                    ModeState.untag state
            in
                div []
                    [ Overlay.view untagged.selected.overlay |> Html.map OverlayMsg
                    , div []
                        ((contentView layouts templates model untagged.contentItem)
                            :: (menuView model (Just untagged.contentItem))
                        )
                    ]

        Preview state ->
            let
                untagged =
                    ModeState.untag state
            in
                div []
                    [ Overlay.view untagged.selected.overlay |> Html.map OverlayMsg
                    , div []
                        ((contentView layouts templates model untagged.contentItem)
                            :: (menuView model (Just untagged.contentItem))
                        )
                    ]

        Wysiwyg state ->
            let
                untagged =
                    ModeState.untag state
            in
                div []
                    [ Overlay.view untagged.selected.overlay |> Html.map OverlayMsg
                    , div []
                        ((contentView layouts templates model untagged.contentItem)
                            :: (menuView model (Just untagged.contentItem))
                        )
                    ]


menuView : Model -> Maybe Content -> List (Html Msg)
menuView model maybeContent =
    case model.menu of
        Disabled state ->
            let
                untagged =
                    MenuState.untag state
            in
                [ slideButton untagged ]

        Available state ->
            let
                untagged =
                    MenuState.untag state
            in
                [ slideButton untagged
                , sideNav model untagged maybeContent
                ]

        Open state ->
            let
                untagged =
                    MenuState.untag state
            in
                [ slideButton untagged
                , sideNav model untagged maybeContent
                , clickPlane
                ]


slideButton : { m | slideButtonStyle : Animation.State } -> Html Msg
slideButton slideButton =
    div
        [ class "slide-button"
        , Events.onClick ToggleMenu
        ]
        [ div
            (Animation.render slideButton.slideButtonStyle
                ++ [ class "slide-button__inset" ]
            )
            []
        ]


sideNav : Model -> { m | controls : Menu } -> Maybe Content -> Html Msg
sideNav model menuState maybeContent =
    div
        (Animation.render menuState.controls.menuStyle
            ++ [ class "sidenav" ]
        )
        [ userStatusBar model
        , sideNavControlBar menuState.controls.controlBar
        , contentTree menuState maybeContent
        ]


userStatusBar : Model -> Html Msg
userStatusBar model =
    div [ class "sidenav__userbar control-bar" ]
        [ div [ class "control-bar__row" ]
            [ div [ class "control-bar__left-0" ]
                [ div
                    [ class "sidenav__avatar-cropper" ]
                    [ img [ model.config.avatarApiRoot ++ "avatar/" ++ model.userId ++ "/image" |> src ] []
                    ]
                ]
            , div [ class "control-bar__right-0" ]
                [ button
                    [ class "sidenav__logout-button"
                    , Events.onClick LogOut
                    ]
                    [ text "Log Out" ]
                ]
            ]
        ]


contentTree : { m | controls : Menu } -> Maybe Content -> Html Msg
contentTree menuState maybeContent =
    ContentTree.view maybeContent menuState.controls.contentTree
        |> Html.map (\treeMsg -> ContentTreeMsg treeMsg)


sideNavControlBar : ControlBar.Model -> Html Msg
sideNavControlBar controlBar =
    Html.map ControlBar <| ControlBar.view "sidenav__controlbar" controlBar


clickPlane : Html Msg
clickPlane =
    div
        [ class "click-plane"
        , Events.onClick ToggleMenu
        ]
        []


contentView :
    Dict String (Layout Msg)
    -> Dict String (Template Msg)
    -> Model
    -> Content
    -> Html Msg
contentView layouts templates model content =
    Renderer.Flexi.view layouts
        templates
        (linker model.config.applicationContextRoot)
        (editor model.mode)
        content


linker : String -> LinkBuilder msg
linker applicationContextRoot slug =
    href <| applicationContextRoot ++ "editor/" ++ editorPrefix ++ slug


editor : ModeState -> Editor Msg
editor mode zipper =
    let
        contentModel =
            contentZipperToModel zipper

        contentId =
            asUUID contentModel

        markdownView : ContentModel -> Html Msg
        markdownView contentModel =
            asMarkdown contentModel
                |> Markdown.toHtml Nothing
                |> Html.div
                    [ domMetricsOn (MouseOverContent zipper) "mouseover"
                    ]

        defaultContent =
            markdownView contentModel

        defaultAttributes =
            [ class "editor-inline__wrapper" ]

        htmlContent selected =
            if (asUUID (contentZipperToModel selected.selectedContent) == contentId) then
                case selected.editedValue of
                    Just value ->
                        markdownView <| withMarkdown contentModel value

                    Nothing ->
                        defaultContent
            else
                defaultContent

        attributes selected =
            if (asUUID (contentZipperToModel selected.selectedContent) == contentId) then
                [ class "editor-inline__wrapper"
                , class "watch-resize"
                , id <| "inline__wrapper" ++ asUUID contentModel
                ]
            else
                defaultAttributes
    in
        -- This is where the inline editor must go.
        -- , div
        --     (Animation.render model.inlineEditorStyle
        --         ++ [ class "editor-inline__editor" ]
        --     )
        --     [ content
        --     ]
        case mode of
            Markdown state ->
                let
                    untagged =
                        ModeState.untag state
                in
                    div (attributes untagged.selected) [ htmlContent untagged.selected ]

            Preview state ->
                let
                    untagged =
                        ModeState.untag state
                in
                    div (attributes untagged.selected) [ htmlContent untagged.selected ]

            Wysiwyg state ->
                let
                    untagged =
                        ModeState.untag state
                in
                    div (attributes untagged.selected) [ htmlContent untagged.selected ]

            _ ->
                div defaultAttributes [ defaultContent ]
