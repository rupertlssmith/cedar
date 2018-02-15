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
    -- Sub.batch
    --     (optional
    --         [ Animation.subscription Animate
    --             (optional
    --                 [ mapWhenWithAvailable (\available -> available.menuStyle) model.menu
    --                 , mapWhenWithSlideButton (\slideButton -> slideButton.slideButtonStyle) model.menu
    --                 , mapWhenWithInlineEditor (\editor -> editor.inlineEditorStyle) model.mode
    --                 ]
    --             )
    --             |> required
    --         , mapWhenWithAvailable
    --             (\available ->
    --                 ContentTree.subscriptions available.contentTree
    --                     |> Sub.map ContentTreeMsg
    --             )
    --             model.menu
    --         , mapWhenWithSelectedModel
    --             (\selected ->
    --                 Overlay.subscriptions selected.overlay
    --                     |> Sub.map OverlayMsg
    --             )
    --             model.mode
    --         , resize |> Sub.map (\event -> Resize event) |> required
    --         , scroll |> Sub.map (\event -> BodyScroll event) |> required
    --         ]
    --     )
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
    case (Debug.log "contentLoaded" model.mode) of
        Loading state ->
            ( { model | mode = toExploreWithContent content state }, Cmd.none )

        _ ->
            ( model, Cmd.none )


treeFetched : Content -> Model -> ( Model, Cmd Msg )
treeFetched content model =
    -- let
    --     maybeContent =
    --         mapWhenWithContent (\{ contentItem } -> contentItem) model.mode
    -- in
    --     ( { model
    --         | menu =
    --             disabledToAvailable
    --                 { menuStyle = Animation.style menuClosedStyle
    --                 , controlBar = ControlBar.init "slideInMenu" [ ( "collapseall", "control-icon control-icon__collapse-all" ) ]
    --                 , contentTree = ContentTree.init content maybeContent
    --                 }
    --                 model.menu
    --       }
    --     , Cmd.none
    --     )
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
        case (Debug.log "treeFetched" model.menu) of
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
        -- mapWhenWithContent (\content -> slugToUrl content.contentItem) model.mode
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

        -- changed =
        --     (prevModel.contentItem /= newModel.contentItem)
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
        Animate _ ->
            msg

        ControlBarUpdate _ ->
            msg

        CSEApi _ ->
            msg

        ContentTreeMsg _ ->
            msg

        OverlayMsg _ ->
            msg

        MouseOverContent _ _ ->
            msg

        _ ->
            Debug.log "contentEditor" msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case (debugFilter action) of
        Animate msg ->
            updateAnimate msg model

        CSEApi msg ->
            updateCSEApi msg model

        ContentServiceApi msg ->
            updateContentServiceApi msg model

        OverlayMsg msg ->
            updateOverlayMsg msg model

        Init ->
            updateInit model

        SelectLocation location ->
            updateSelectLocation location model

        MouseOverContent zipper domState ->
            updateMouseOverContent zipper domState model

        MouseOutContent ->
            updateMouseOutContent model

        Resize size ->
            updateResize size model

        BodyScroll move ->
            updateBodyScroll move model

        -- ==== Menu updates ==== --
        ContentTreeMsg msg ->
            updateContentTreeMsg msg model

        ControlBarUpdate msg ->
            updateControlBarUpdate msg model

        ToggleMenu ->
            updateToggleMenu model

        ControlBar outMsg ->
            updateControlBar outMsg model

        LogOut ->
            --( model, Auth.logout )
            ( model, Cmd.none )

        Error _ ->
            ( model, Cmd.none )


{-| Update animations when animated styles are available.
-}
updateAnimate : Animation.Msg -> Model -> ( Model, Cmd Msg )
updateAnimate msg model =
    let
        updateMenuStyle menu =
            { menu | menuStyle = Animation.update msg menu.menuStyle }

        updateSlideButtonStyle slideButtonStyle =
            Animation.update msg slideButtonStyle

        updateInlineEditorStyle inlineEditorStyle =
            Animation.update msg inlineEditorStyle
    in
        ( { model
            | menu =
                mapSlideButtonStyle updateSlideButtonStyle model.menu
                    |> mapMenu updateMenuStyle
            , mode = mapInlineEditorStyle updateInlineEditorStyle model.mode
          }
        , Cmd.none
        )


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


updateModeWithOverlay : Float -> Overlay.OutMsg -> ModeState -> ( ModeState, Cmd Msg )
updateModeWithOverlay yOffset outmsg mode =
    case outmsg of
        Overlay.Closed ->
            --  mapWhenWithContent (\content -> toExplore content mode) mode
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
            -- selectedToMarkdown mode
            --     |> Maybe.andThen
            --         (updateWhenWithSelectedModel
            --             (\selected ->
            --                 { selected
            --                     | overlay =
            --                         Overlay.makeActive
            --                             model.yOffset
            --                             (selected.editedValue |> Maybe.withDefault (contentZipperToModel selected.selectedContent |> asMarkdown))
            --                             selected.overlay
            --                 }
            --             )
            --         )
            case mode of
                Preview state ->
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
                        ( mapSelectedModel activateOverlay <| toMarkdown state
                        , Cmd.none
                        )

                _ ->
                    ( mode, Cmd.none )

        Overlay.SelectMode "preview" ->
            -- selectedToPreview mode
            --     |> Maybe.andThen
            --         (updateWhenWithSelectedModel
            --             (\selected -> { selected | overlay = Overlay.makeInactive selected.overlay })
            --         )
            case mode of
                Markdown state ->
                    ( mapSelectedModel (\selected -> { selected | overlay = Overlay.makeInactive selected.overlay }) <|
                        toPreview state
                    , Cmd.none
                    )

                _ ->
                    ( mode, Cmd.none )

        Overlay.ContentValue value ->
            -- (updateWhenWithSelectedModel
            --     (\selected -> { selected | editedValue = Just value })
            -- )
            --     mode
            ( mapSelectedModel (\selected -> { selected | editedValue = Just value }) mode, Cmd.none )

        Overlay.SelectMode "save" ->
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
    -- let
    --     switchMode : Maybe Overlay.OutMsg -> Mode -> Maybe Mode
    --     switchMode outmsg mode =
    --         case outmsg of
    --             Just Overlay.Closed ->
    --                 mapWhenWithContent (\content -> toExplore content mode) mode
    --
    --             Just (Overlay.SelectMode "markdown") ->
    --                 -- inlineStyle = animateStyle menuEasing model.inlineEditorStyle inlineEditorInactiveStyle
    --                 selectedToMarkdown mode
    --                     |> Maybe.andThen
    --                         (updateWhenWithSelectedModel
    --                             (\selected ->
    --                                 { selected
    --                                     | overlay =
    --                                         Overlay.makeActive
    --                                             model.yOffset
    --                                             (selected.editedValue |> Maybe.withDefault (contentZipperToModel selected.selectedContent |> asMarkdown))
    --                                             selected.overlay
    --                                 }
    --                             )
    --                         )
    --
    --             Just (Overlay.SelectMode "preview") ->
    --                 -- inlineStyle = animateStyle menuEasing model.inlineEditorStyle inlineEditorActiveStyle
    --                 selectedToPreview mode
    --                     |> Maybe.andThen
    --                         (updateWhenWithSelectedModel
    --                             (\selected -> { selected | overlay = Overlay.makeInactive selected.overlay })
    --                         )
    --
    --             Just (Overlay.ContentValue value) ->
    --                 (updateWhenWithSelectedModel
    --                     (\selected -> { selected | editedValue = Just value })
    --                 )
    --                     mode
    --
    --             Just (Overlay.SelectMode "save") ->
    --                 (mapWhenCompose mapWhenWithSelectedModel updateWhenWithContent)
    --                     (\selected ->
    --                         \withContent ->
    --                             case selected.editedValue of
    --                                 Nothing ->
    --                                     withContent
    --
    --                                 Just value ->
    --                                     let
    --                                         (( tree, _ ) as zipper) =
    --                                             selected.selectedContent
    --
    --                                         newTree =
    --                                             TreeUtils.updateTree
    --                                                 (\(Content content) ->
    --                                                     Content
    --                                                         { content
    --                                                             | model =
    --                                                                 (withMarkdown (contentZipperToModel zipper) value)
    --                                                         }
    --                                                 )
    --                                                 zipper
    --                                                 |> Maybe.withDefault tree
    --
    --                                         d =
    --                                             Debug.log "save" newTree
    --                                     in
    --                                         { withContent | contentItem = containerTreeToContent newTree }
    --                     )
    --                     mode
    --
    --             _ ->
    --                 Nothing
    --
    --     commandsForOverlayOutMsg : Maybe Overlay.OutMsg -> Mode -> Cmd Msg
    --     commandsForOverlayOutMsg outmsg mode =
    --         case outmsg of
    --             Just (Overlay.SelectMode "save") ->
    --                 -- When with a selected model,
    --                 -- fold the edited value back into the content model.
    --                 -- fold the content model back into the current content item.
    --                 -- Invoke update to save the content.
    --                 (mapWhenCompose mapWhenWithSelectedModel mapWhenWithContent)
    --                     (\selected ->
    --                         \withContent ->
    --                             case selected.editedValue of
    --                                 Nothing ->
    --                                     Cmd.none
    --
    --                                 Just value ->
    --                                     let
    --                                         (( tree, _ ) as zipper) =
    --                                             selected.selectedContent
    --
    --                                         newTree =
    --                                             TreeUtils.updateTree
    --                                                 (\(Content content) ->
    --                                                     Content
    --                                                         { content
    --                                                             | model =
    --                                                                 (withMarkdown (contentZipperToModel zipper) value)
    --                                                         }
    --                                                 )
    --                                                 zipper
    --                                                 |> Maybe.withDefault tree
    --
    --                                         (Content content) =
    --                                             (containerTreeToContent newTree)
    --
    --                                         id =
    --                                             Maybe.withDefault "" content.id
    --
    --                                         d =
    --                                             Debug.log "save" content
    --                                     in
    --                                         Content.Service.invokeUpdate
    --                                             model.config.apiRoot
    --                                             ContentServiceApi
    --                                             id
    --                                             (containerTreeToContent newTree)
    --                     )
    --                     mode
    --                     |> Maybe.withDefault Cmd.none
    --
    --             _ ->
    --                 Cmd.none
    --
    --     updateModeWithNewOverlay : Overlay.Model -> Mode -> Maybe Mode
    --     updateModeWithNewOverlay overlay mode =
    --         updateWhenWithSelectedModel
    --             (\selected ->
    --                 { selected | overlay = overlay }
    --             )
    --             mode
    --
    --     maybeOverlayUpdate : Mode -> Maybe ( Overlay.Model, Cmd Overlay.Msg, Maybe Overlay.OutMsg )
    --     maybeOverlayUpdate mode =
    --         mapWhenWithSelectedModel (\selected -> Overlay.update msg selected.overlay) mode
    -- in
    --     case maybeOverlayUpdate model.mode of
    --         Just ( newOverlay, _, maybeOutMsg ) ->
    --             ( { model
    --                 | mode =
    --                     (updateModeWithNewOverlay newOverlay)
    --                         >##> (switchMode maybeOutMsg)
    --                         |> defaultTransition model.mode
    --               }
    --             , commandsForOverlayOutMsg maybeOutMsg model.mode
    --             )
    --
    --         Nothing ->
    --             ( model, Cmd.none )
    let
        updateOverlay state =
            Update3.lift .overlay (\x m -> { m | overlay = x }) OverlayMsg Overlay.update msg (ModeState.untag state).selected
                |> Update3.mapModel (\selected -> mapSelectedModel (always selected) model.mode)
                |> Update3.evalMaybe (updateModeWithOverlay model.yOffset) Cmd.none
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
updateMouseOverContent : Zipper Content -> DOMState -> Model -> ( Model, Cmd Msg )
updateMouseOverContent zipper domState model =
    -- let
    --     exploreToMarkdown : Mode -> Maybe Mode
    --     exploreToMarkdown mode =
    --         maybeExplore mode
    --             |> Maybe.Extra.prev
    --                 (toMarkdown
    --                     { selectedContent = zipper
    --                     , overlay = Overlay.makeAware domState.rect Overlay.init
    --                     , editedValue = Nothing
    --                     }
    --                     mode
    --                 )
    -- in
    --     ( { model | mode = exploreToMarkdown |> defaultTransition model.mode }
    --     , Cmd.none
    --     )
    case model.mode of
        Explore state ->
            ( { model
                | mode =
                    toMarkdownWithSelectedModel
                        { selectedContent = zipper
                        , overlay = Overlay.makeAware domState.rect Overlay.init
                        , editedValue = Nothing
                        }
                        state
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


updateMouseOutContent : Model -> ( Model, Cmd Msg )
updateMouseOutContent model =
    -- ( { model
    --     | mode =
    --         mapWhenWithContent (\content -> toExplore content model.mode)
    --             |> defaultTransition model.mode
    --   }
    -- , Cmd.none
    -- )
    case model.mode of
        Markdown state ->
            ( { model | mode = toExplore state }, Cmd.none )

        _ ->
            ( model, Cmd.none )


{-| When there is a selected content model with an overlay,
forward changes in the content models rendered size to the overlay.
-}
updateResize : ResizeEvent -> Model -> ( Model, Cmd Msg )
updateResize size model =
    ( { model
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
    , Cmd.none
    )


updateBodyScroll : Move -> Model -> ( Model, Cmd Msg )
updateBodyScroll ( from, to ) model =
    ( { model
        | yOffset = to
        , mode =
            mapSelectedModel
                (\selected ->
                    { selected | overlay = Overlay.scroll ( from, to ) selected.overlay }
                )
                model.mode
      }
    , Cmd.none
    )



-- ==== Menu updates ==== --


updateContentTreeMsg : ContentTree.Msg -> Model -> ( Model, Cmd Msg )
updateContentTreeMsg msg model =
    -- let
    --     translateContentTreeOutMsg outmsg =
    --         case outmsg of
    --             ContentTree.Navigate location ->
    --                 CSE.invokeRetrieveWithContainerBySlug model.config.apiRoot CSEApi location
    --
    --     maybeContentTreeUpdate =
    --         mapWhenWithAvailable (\{ contentTree } -> ContentTree.update msg contentTree) model.menu
    -- in
    --     case maybeContentTreeUpdate of
    --         Just ( newTree, outMsg ) ->
    --             ( { model
    --                 | menu =
    --                     updateWhenWithAvailable (\available -> { available | contentTree = newTree })
    --                         |> defaultTransition model.menu
    --               }
    --             , Maybe.map translateContentTreeOutMsg outMsg |> Maybe.withDefault Cmd.none
    --             )
    --
    --         Nothing ->
    --             ( model, Cmd.none )
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


updateControlBarUpdate : ControlBar.Msg -> Model -> ( Model, Cmd Msg )
updateControlBarUpdate msg model =
    ( { model | menu = mapMenu (\menu -> { menu | controlBar = ControlBar.update msg menu.controlBar }) model.menu }
    , Cmd.none
    )


updateToggleMenu : Model -> ( Model, Cmd Msg )
updateToggleMenu model =
    let
        updateSlideButtonStyle toStyle fromStyle =
            animateStyle slideButtonEasing fromStyle toStyle

        updateMenuStyle style menu =
            { menu | menuStyle = animateStyle menuEasing menu.menuStyle style }
    in
        case model.menu of
            Available state ->
                -- ((updateSlideButtonStyle slideButtonOpenStyle)
                --     >&&> (updateMenuStyle menuOpenStyle)
                --     >&&> menuToggle
                -- )
                ( { model
                    | menu =
                        toOpen state
                            |> mapSlideButtonStyle (updateSlideButtonStyle slideButtonOpenStyle)
                            |> mapMenu (updateMenuStyle menuOpenStyle)
                  }
                , Cmd.none
                )

            Open state ->
                -- ((updateSlideButtonStyle slideButtonClosedStyle)
                --     >&&> (updateMenuStyle menuClosedStyle)
                --     >&&> menuToggle
                -- )
                ( { model
                    | menu =
                        toAvailable state
                            |> mapSlideButtonStyle (updateSlideButtonStyle slideButtonClosedStyle)
                            |> mapMenu (updateMenuStyle menuClosedStyle)
                  }
                , Cmd.none
                )

            _ ->
                ( model, Cmd.none )


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
    -- let
    --     maybeContent =
    --         mapWhenWithContent (\{ contentItem } -> contentItem) model.mode
    -- in
    --     mapWhenWithContent
    --         (\{ contentItem } ->
    --             div []
    --                 (optional
    --                     [ mapWhenWithSelectedModel (\{ overlay } -> Overlay.view overlay |> Html.map OverlayMsg) model.mode
    --                     , div []
    --                         (optional
    --                             [ mapWhenWithContent (\{ contentItem } -> contentView layouts templates model contentItem) model.mode
    --                             , mapWhenWithSlideButton (\button -> slideButton button) model.menu
    --                             , mapWhenWithAvailable (\available -> sideNav model available maybeContent) model.menu
    --                             , when (isJust (maybeOpen model.menu)) clickPlane
    --                             ]
    --                         )
    --                         |> required
    --                     ]
    --                 )
    --         )
    --         model.mode
    --         |> Maybe.withDefault emptyDiv
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
    -- div []
    --                             , mapWhenWithSlideButton (\button -> slideButton button) model.menu
    --                             , mapWhenWithAvailable (\available -> sideNav model available maybeContent) model.menu
    --                             , when (isJust (maybeOpen model.menu)) clickPlane
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
        -- div attributes
        -- [ htmlContent ]
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



-- -- State machine for the content explorer and editor.
--
--
-- type alias WithContent =
--     { contentItem : Content }
--
--
-- type alias WithSelectedModel =
--     { selectedContent : Zipper Content
--     , overlay : Overlay.Model
--     , editedValue : Maybe String
--     }
--
--
-- type alias WithInlineEditor =
--     { inlineEditorStyle : Animation.State
--     }
--
--
-- type Mode
--     = Loading
--     | Explore WithContent
--     | Markdown WithContent WithSelectedModel
--     | Preview WithContent WithSelectedModel
--     | Wysiwyg WithContent WithSelectedModel WithInlineEditor
--
--
-- maybeLoading : Mode -> Maybe Mode
-- maybeLoading state =
--     case state of
--         Loading ->
--             Just state
--
--         _ ->
--             Nothing
--
--
-- maybeExplore : Mode -> Maybe Mode
-- maybeExplore state =
--     case state of
--         Explore _ ->
--             Just state
--
--         _ ->
--             Nothing
--
--
-- maybeMarkdown : Mode -> Maybe Mode
-- maybeMarkdown state =
--     case state of
--         Markdown _ _ ->
--             Just state
--
--         _ ->
--             Nothing
--
--
-- maybePreview : Mode -> Maybe Mode
-- maybePreview state =
--     case state of
--         Preview _ _ ->
--             Just state
--
--         _ ->
--             Nothing
--
--
-- maybeWysiwyg : Mode -> Maybe Mode
-- maybeWysiwyg state =
--     case state of
--         Wysiwyg _ _ _ ->
--             Just state
--
--         _ ->
--             Nothing
--
--
-- mapWhenWithContent : (WithContent -> a) -> Mode -> Maybe a
-- mapWhenWithContent func state =
--     case state of
--         Explore content ->
--             Just <| func content
--
--         Markdown content _ ->
--             Just <| func content
--
--         Preview content _ ->
--             Just <| func content
--
--         Wysiwyg content _ _ ->
--             Just <| func content
--
--         _ ->
--             Nothing
--
--
-- mapWhenWithSelectedModel : (WithSelectedModel -> a) -> Mode -> Maybe a
-- mapWhenWithSelectedModel func state =
--     case state of
--         Markdown _ selected ->
--             Just <| func selected
--
--         Preview _ selected ->
--             Just <| func selected
--
--         Wysiwyg _ selected _ ->
--             Just <| func selected
--
--         _ ->
--             Nothing
--
--
-- mapWhenWithInlineEditor : (WithInlineEditor -> a) -> Mode -> Maybe a
-- mapWhenWithInlineEditor func state =
--     case state of
--         Wysiwyg _ _ inline ->
--             Just <| func inline
--
--         _ ->
--             Nothing
--
--
-- updateWhenWithContent : (WithContent -> WithContent) -> Mode -> Maybe Mode
-- updateWhenWithContent func state =
--     case state of
--         Explore content ->
--             func content |> Explore |> Just
--
--         Markdown content selected ->
--             func content |> (flip Markdown) selected |> Just
--
--         Preview content selected ->
--             func content |> (flip Preview) selected |> Just
--
--         Wysiwyg content selected inline ->
--             func content |> (swirll Wysiwyg) selected inline |> Just
--
--         _ ->
--             Nothing
--
--
-- updateWhenWithSelectedModel : (WithSelectedModel -> WithSelectedModel) -> Mode -> Maybe Mode
-- updateWhenWithSelectedModel func state =
--     case state of
--         Markdown content selected ->
--             func selected |> Markdown content |> Just
--
--         Preview content selected ->
--             func selected |> Preview content |> Just
--
--         Wysiwyg content selected inline ->
--             func selected |> (flip (Wysiwyg content)) inline |> Just
--
--         _ ->
--             Nothing
--
--
-- updateWhenWithInlineEditor : (WithInlineEditor -> WithInlineEditor) -> Mode -> Maybe Mode
-- updateWhenWithInlineEditor func state =
--     case state of
--         Wysiwyg content selected inline ->
--             func inline |> Wysiwyg content selected |> Just
--
--         _ ->
--             Nothing
--
--
-- toLoading : Mode -> Mode
-- toLoading _ =
--     Loading
--
--
-- toExplore : WithContent -> Mode -> Mode
-- toExplore content state =
--     Explore content
--
--
-- toMarkdown : WithSelectedModel -> Mode -> Maybe Mode
-- toMarkdown selected state =
--     case state of
--         Loading ->
--             Nothing
--
--         _ ->
--             mapWhenWithContent
--                 (\content ->
--                     Markdown content selected
--                 )
--                 state
--
--
-- toPreview : WithSelectedModel -> Mode -> Maybe Mode
-- toPreview selected state =
--     case state of
--         Loading ->
--             Nothing
--
--         _ ->
--             mapWhenWithContent
--                 (\content ->
--                     Preview content selected
--                 )
--                 state
--
--
-- toWysiwyg : WithSelectedModel -> WithInlineEditor -> Mode -> Maybe Mode
-- toWysiwyg selected inline state =
--     case state of
--         Loading ->
--             Nothing
--
--         _ ->
--             mapWhenWithContent
--                 (\content ->
--                     Wysiwyg content selected inline
--                 )
--                 state
--
--
-- selectedToMarkdown : Mode -> Maybe Mode
-- selectedToMarkdown mode =
--     mapWhenCompose mapWhenWithContent mapWhenWithSelectedModel Markdown mode
--
--
-- selectedToPreview : Mode -> Maybe Mode
-- selectedToPreview mode =
--     mapWhenCompose mapWhenWithContent mapWhenWithSelectedModel Preview mode
--
--
--
-- -- State machine for the content tree menu.
--
--
-- type alias WithSlideButton =
--     { slideButtonStyle : Animation.State }
--
--
-- type alias WithAvailable =
--     { menuStyle : Animation.State
--     , controlBar : ControlBar.Model
--     , contentTree : ContentTree.Model
--     }
--
--
-- type Menu
--     = Disabled WithSlideButton
--     | Available WithSlideButton WithAvailable
--     | Open WithSlideButton WithAvailable
--
--
-- maybeAvailable : Menu -> Maybe Menu
-- maybeAvailable state =
--     case state of
--         Available _ _ ->
--             Just state
--
--         _ ->
--             Nothing
--
--
-- maybeOpen : Menu -> Maybe Menu
-- maybeOpen state =
--     case state of
--         Open _ _ ->
--             Just state
--
--         _ ->
--             Nothing
--
--
-- maybeDisabled : Menu -> Maybe Menu
-- maybeDisabled state =
--     case state of
--         Disabled _ ->
--             Just state
--
--         _ ->
--             Nothing
--
--
-- mapWhenWithSlideButton : (WithSlideButton -> a) -> Menu -> Maybe a
-- mapWhenWithSlideButton func state =
--     case state of
--         Disabled slideButton ->
--             Just <| func slideButton
--
--         Available slideButton _ ->
--             Just <| func slideButton
--
--         Open slideButton _ ->
--             Just <| func slideButton
--
--
-- mapWhenWithAvailable : (WithAvailable -> a) -> Menu -> Maybe a
-- mapWhenWithAvailable func state =
--     case state of
--         Available _ available ->
--             Just <| func available
--
--         Open _ available ->
--             Just <| func available
--
--         _ ->
--             Nothing
--
--
-- updateWhenWithSlideButton : (WithSlideButton -> WithSlideButton) -> Menu -> Maybe Menu
-- updateWhenWithSlideButton func state =
--     case state of
--         Disabled slideButton ->
--             func slideButton |> Disabled |> Just
--
--         Available slideButton available ->
--             func slideButton |> (flip Available) available |> Just
--
--         Open slideButton available ->
--             func slideButton |> (flip Open) available |> Just
--
--
-- updateWhenWithAvailable : (WithAvailable -> WithAvailable) -> Menu -> Maybe Menu
-- updateWhenWithAvailable func state =
--     case state of
--         Available slideButton available ->
--             func available |> Available slideButton |> Just
--
--         Open slideButton available ->
--             func available |> Open slideButton |> Just
--
--         _ ->
--             Nothing
--
--
-- disabledToAvailable : WithAvailable -> Menu -> Menu
-- disabledToAvailable available state =
--     case state of
--         Available slideButton _ ->
--             Available slideButton available
--
--         Open slideButton _ ->
--             Available slideButton available
--
--         Disabled slideButton ->
--             Available slideButton available
--
--
-- menuToggle : Menu -> Maybe Menu
-- menuToggle state =
--     case state of
--         Available slideButton available ->
--             Open slideButton available |> Just
--
--         Open slideButton available ->
--             Available slideButton available |> Just
--
--         _ ->
--             Nothing
