module Client.Top
    exposing
        ( delta2url
        , location2messages
        , init
        , update
        , subscriptions
        , view
        , Model
        , Msg
        )

{-| The content editor client top module.

@docs delta2url, location2messages, init, update, subscriptions, view, Model, Msg

-}

import Auth
import Config exposing (config)
import Dict exposing (Dict)
import Editor.ContentEditor as CE
import Html exposing (Html)
import Maybe.Extra
import Navigation
import Optional exposing (optional, required, when)
import Renderer.Flexi exposing (Layout, Template)
import ResizeObserver
import RouteUrl as Routing
import ScrollPort
import Update2
import Update3
import Welcome


{-| The content editor program model.
-}
type alias Model =
    { auth : Auth.Model
    , session : Session
    }


type Session
    = Initial
    | Welcome WithWelcome
    | FailedAuth WithWelcome
    | Authenticated WithContentEditor


type alias WithWelcome =
    { welcome : Welcome.Model }


type alias WithContentEditor =
    { contentEditor : CE.Model }


{-| The content editor program top-level message types.
-}
type Msg
    = AuthMsg Auth.Msg
    | WelcomeMsg Welcome.Msg
    | ContentEditorMsg CE.Msg



-- Initialization


{-| Initiales the application state by setting it to the 'Initial' state. Requests
that an Auth refreshed be performed to check what the current authentication
state is.
-}
init : ( Model, Cmd Msg )
init =
    ( { auth = Auth.init config
      , session = Initial
      }
    , Auth.refresh
    )


setLoginLocations authState =
    { authState | logoutLocation = "#/welcome", forwardLocation = "" }



-- Subscriptions


{-| Sets up the subscriptions for the content editor.
-}
subscriptions : ResizeObserver.Resize -> ScrollPort.Scroll -> Model -> Sub Msg
subscriptions resize scroll model =
    Sub.batch
        (optional
            [ case model.session of
                Authenticated state ->
                    CE.subscriptions resize scroll state.contentEditor
                        |> Sub.map ContentEditorMsg
                        |> Just

                _ ->
                    Nothing
            ]
        )



-- Navigation


{-| Sets the navigation bar location dependant on the state of the model.
-}
delta2url : Model -> Model -> Maybe Routing.UrlChange
delta2url _ model =
    case model.session of
        Initial ->
            { entry = Routing.NewEntry
            , url = ""
            }
                |> Just

        Welcome _ ->
            { entry = Routing.NewEntry
            , url = "#/welcome"
            }
                |> Just

        FailedAuth _ ->
            { entry = Routing.NewEntry
            , url = "#/welcome"
            }
                |> Just

        Authenticated state ->
            CE.delta2url state.contentEditor
                state.contentEditor


{-| Process naviagation bar location changes.
-}
location2messages : Navigation.Location -> List Msg
location2messages location =
    if location.hash == "" || location.hash == "#/welcome" then
        []
    else
        CE.location2messages (Debug.log "location2messages" location) |> List.map ContentEditorMsg



-- Model updates


debugFilter : Msg -> Msg
debugFilter msg =
    case msg of
        WelcomeMsg _ ->
            msg

        ContentEditorMsg _ ->
            msg

        _ ->
            Debug.log "main" msg


{-| Processes state updates for the content editor.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case ( model.session, (debugFilter action) ) of
        ( _, AuthMsg msg ) ->
            Update3.lift .auth (\x m -> { m | auth = x }) AuthMsg Auth.update msg model
                |> Update3.evalMaybe updateOnAuthStatus Cmd.none

        ( Welcome state, WelcomeMsg msg ) ->
            Update3.lift .welcome (\x m -> { m | welcome = x }) WelcomeMsg Welcome.update msg state
                |> Update3.evalCmds AuthMsg

        ( FailedAuth state, WelcomeMsg msg ) ->
            Update3.lift .welcome (\x m -> { m | welcome = x }) WelcomeMsg Welcome.update msg state
                |> Update3.evalCmds AuthMsg

        ( Authenticated state, ContentEditorMsg msg ) ->
            Update2.lift .contentEditor (\x m -> { m | contentEditor = x }) ContentEditorMsg CE.update msg state

        ( _, _ ) ->
            ( model, Cmd.none )


updateOnAuthStatus : Auth.Status -> Model -> ( Model, Cmd msg )
updateOnAuthStatus status model =
    let
        session =
            case status of
                Auth.Failed ->
                    FailedAuth { welcome = Welcome.init }

                Auth.LoggedOut ->
                    Welcome { welcome = Welcome.init }

                Auth.LoggedIn authenticated ->
                    let
                        init =
                            CE.init config authenticated.subject
                    in
                        Authenticated { contentEditor = Tuple.first init }
    in
        ( { model | session = session }, Cmd.none )



-- updateWelcomeMsg : Welcome.Msg -> WithWelcome -> ( WithWelcome, Cmd Msg )
-- updateWelcomeMsg msg state =
--     case Welcome.update msg state.welcome of
--         ( welcome, cmd ) ->
--             ( { welcome = welcome }
--             , Cmd.map WelcomeMsg cmd
--             )
--
--
-- updateContentEditorMsg : CE.Msg -> WithContentEditor -> ( WithContentEditor, Cmd Msg )
-- updateContentEditorMsg msg state =
--     case CE.update msg state.contentEditor of
--         ( contentEditor, cmd ) ->
--             ( { contentEditor = contentEditor }
--             , Cmd.map ContentEditorMsg cmd
--             )
-- updateAuthMsg : Auth.Msg -> Model -> ( Model, Cmd Msg )
-- updateAuthMsg msg model =
--     let
--         ( authUpdatedModel, authUpdateCmds ) =
--             Update2.lift .auth (\x m -> { m | auth = x }) AuthMsg AuthController.update msg model
--
--         isAuthenticated =
--             AuthController.isLoggedIn authUpdatedModel.auth.authState
--
--         logonAttempted =
--             AuthController.logonAttempted authUpdatedModel.auth
--
--         hasPermission =
--             AuthController.hasPermission "content-author" authUpdatedModel.auth.authState
--
--         ( session, initCmds ) =
--             case ( model.session, isAuthenticated, hasPermission, logonAttempted ) of
--                 ( Welcome state, True, True, _ ) ->
--                     let
--                         ( contentEditor, editorInitCmds ) =
--                             CE.init config authUpdatedModel.auth.authState.username
--                     in
--                         ( toAuthenticatedWithContentEditor { contentEditor = contentEditor } state
--                         , editorInitCmds |> Cmd.map ContentEditorMsg
--                         )
--
--                 ( Welcome state, True, False, _ ) ->
--                     ( toFailedAuth state, Cmd.none )
--
--                 ( Welcome state, False, _, True ) ->
--                     ( toFailedAuth state, Cmd.none )
--
--                 -- else if not refreshAttempted then
--                 --     ( Initial, Cmd.none )
--                 ( FailedAuth state, _, _, _ ) ->
--                     ( toWelcome state, Cmd.none )
--
--                 ( Initial state, _, _, _ ) ->
--                     ( toWelcomeWithWelcome { welcome = Welcome.init } state, Cmd.none )
--
--                 ( Authenticated state, _, _, _ ) ->
--                     ( toWelcomeWithWelcome { welcome = Welcome.init } state, Cmd.none )
--
--                 ( _, _, _, _ ) ->
--                     ( model.session, Cmd.none )
--     in
--         ( { authUpdatedModel | session = session }, Cmd.batch [ authUpdateCmds, initCmds ] )
-- View


{-| Top level view function for the content editor SPA.
-}
view :
    Dict String (Layout CE.Msg)
    -> Dict String (Template CE.Msg)
    -> Model
    -> Html Msg
view layouts templates model =
    case model.session of
        Initial ->
            Html.div [] []

        Welcome state ->
            Welcome.loginView state.welcome |> Html.map WelcomeMsg

        FailedAuth state ->
            Welcome.notPermittedView state.welcome |> Html.map WelcomeMsg

        Authenticated state ->
            CE.view layouts templates state.contentEditor |> Html.map ContentEditorMsg
