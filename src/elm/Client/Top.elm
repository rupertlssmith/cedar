module Client.Top exposing (delta2url, location2messages, init, update, subscriptions, view)

{-|
The content editor client top module.

@docs delta2url, location2messages, init, update, subscriptions, view
-}

import RouteUrl as Routing
import Editor.ContentEditor as CE
import TimeTravel.Navigation as TimeTravel
import AuthController
import Welcome.Auth
import Utils exposing (lift)
import StateModel exposing (boolToMaybe, (>&&>), (>||>), (>##>), defaultTransition, mapWhenCompose)
import Navigation
import Optional exposing (optional, required, when)
import Html exposing (Html)
import Config exposing (config)
import Auth
import Maybe.Extra


type Session
    = Initial
    | Welcome WithWelcome
    | FailedAuth WithWelcome
    | Authenticated WithContentEditor


type alias WithWelcome =
    { welcome : Welcome.Auth.Model }


type alias WithContentEditor =
    { contentEditor : CE.Model }


maybeInitial : Session -> Maybe Session
maybeInitial session =
    case session of
        Initial ->
            Just session

        _ ->
            Nothing


maybeWelcome : Session -> Maybe Session
maybeWelcome session =
    case session of
        Welcome _ ->
            Just session

        _ ->
            Nothing


maybeFailedAuth : Session -> Maybe Session
maybeFailedAuth session =
    case session of
        FailedAuth _ ->
            Just session

        _ ->
            Nothing


maybeAuthenticated : Session -> Maybe Session
maybeAuthenticated session =
    case session of
        Authenticated _ ->
            Just session

        _ ->
            Nothing


mapWhenWithWelcome : (WithWelcome -> a) -> Session -> Maybe a
mapWhenWithWelcome func session =
    case session of
        Welcome welcome ->
            Just <| func welcome

        FailedAuth welcome ->
            Just <| func welcome

        _ ->
            Nothing


mapWhenWithContentEditor : (WithContentEditor -> a) -> Session -> Maybe a
mapWhenWithContentEditor func session =
    case session of
        Authenticated editor ->
            Just <| func editor

        _ ->
            Nothing


updateWhenWithWelcome : (WithWelcome -> WithWelcome) -> Session -> Maybe Session
updateWhenWithWelcome func session =
    case session of
        Welcome welcome ->
            func welcome |> Welcome |> Just

        FailedAuth welcome ->
            func welcome |> FailedAuth |> Just

        _ ->
            Nothing


updateWhenWithContentEditor : (WithContentEditor -> WithContentEditor) -> Session -> Maybe Session
updateWhenWithContentEditor func session =
    case session of
        Authenticated editor ->
            func editor |> Authenticated |> Just

        _ ->
            Nothing


toWelcome : WithWelcome -> Session -> Session
toWelcome welcome _ =
    Welcome welcome


toFailedAuth : Session -> Maybe Session
toFailedAuth session =
    case session of
        Welcome welcome ->
            FailedAuth welcome |> Just

        _ ->
            Nothing


toAuthenticated : WithContentEditor -> Session -> Maybe Session
toAuthenticated contentEditor session =
    case session of
        Welcome _ ->
            Authenticated contentEditor |> Just

        _ ->
            Nothing


type alias Model =
    { auth : AuthController.Model
    , session : Session
    }


type Msg
    = AuthMsg AuthController.Msg
    | WelcomeMsg Welcome.Auth.Msg
    | ContentEditorMsg CE.Msg



-- Initialization


{-|
Initiales the application state by setting it to the 'Initial' state. Requests
that an Auth refreshed be performed to check what the current authentication
state is.
-}
init : ( Model, Cmd Msg )
init =
    ( { auth = setLoginLocations AuthController.init
      , session =
            Initial
            --Welcome { welcome = Welcome.Auth.init }
      }
    , Auth.refresh
    )


setLoginLocations authState =
    { authState | logoutLocation = "#/welcome", forwardLocation = "" }



-- Subscriptions


{-|
Sets up the subscriptions for the content editor.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (optional
            [ Sub.map AuthMsg (AuthController.subscriptions model.auth) |> required
            , mapWhenWithContentEditor
                (\{ contentEditor } ->
                    CE.subscriptions contentEditor
                        |> Sub.map ContentEditorMsg
                )
                model.session
            ]
        )



-- Navigation


{-|
Sets the navigation bar location dependant on the state of the model.
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

        Authenticated { contentEditor } ->
            CE.delta2url contentEditor contentEditor


{-|
Process naviagation bar location changes.
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


{-|
Processes state updates for the content editor.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case (debugFilter action) of
        AuthMsg msg ->
            updateAuthMsg msg model

        WelcomeMsg msg ->
            updateWelcomeMsg msg model

        ContentEditorMsg msg ->
            updateContentEditorMsg msg model


updateAuthMsg : AuthController.Msg -> Model -> ( Model, Cmd Msg )
updateAuthMsg msg model =
    let
        ( authUpdatedModel, authUpdateCmds ) =
            lift .auth (\m x -> { m | auth = x }) AuthMsg AuthController.update msg model

        authenticated =
            AuthController.isLoggedIn authUpdatedModel.auth.authState

        logonAttempted =
            AuthController.logonAttempted authUpdatedModel.auth

        hasPermission =
            AuthController.hasPermission "content-author" authUpdatedModel.auth.authState

        ( session, initCmds ) =
            if authenticated && hasPermission then
                let
                    ( contentEditor, editorInitCmds ) =
                        CE.init config authUpdatedModel.auth.authState.username
                in
                    ( Authenticated { contentEditor = contentEditor }
                    , editorInitCmds |> Cmd.map ContentEditorMsg
                    )
            else if authenticated && not hasPermission then
                ( FailedAuth { welcome = Welcome.Auth.init }, Cmd.none )
            else if not authenticated && logonAttempted then
                ( FailedAuth { welcome = Welcome.Auth.init }, Cmd.none )
                -- else if not refreshAttempted then
                --     ( Initial, Cmd.none )
            else
                ( Welcome { welcome = Welcome.Auth.init }, Cmd.none )
    in
        ( { authUpdatedModel | session = session }, Cmd.batch [ authUpdateCmds, initCmds ] )


updateWelcomeMsg : Welcome.Auth.Msg -> Model -> ( Model, Cmd Msg )
updateWelcomeMsg msg model =
    let
        maybeUpdate =
            mapWhenWithWelcome (\{ welcome } -> Welcome.Auth.update msg welcome)
                model.session
    in
        case maybeUpdate of
            Just ( welcome, cmd ) ->
                ( { model
                    | session =
                        updateWhenWithWelcome (\withWelcome -> { withWelcome | welcome = welcome })
                            |> defaultTransition model.session
                  }
                , Cmd.map WelcomeMsg cmd
                )

            Nothing ->
                ( model, Cmd.none )


updateContentEditorMsg : CE.Msg -> Model -> ( Model, Cmd Msg )
updateContentEditorMsg msg model =
    let
        maybeUpdate =
            mapWhenWithContentEditor (\{ contentEditor } -> CE.update msg contentEditor)
                model.session
    in
        case maybeUpdate of
            Just ( contentEditor, cmd ) ->
                ( { model
                    | session =
                        updateWhenWithContentEditor (\withContentEditor -> { withContentEditor | contentEditor = contentEditor })
                            |> defaultTransition model.session
                  }
                , Cmd.map ContentEditorMsg cmd
                )

            Nothing ->
                ( model, Cmd.none )



-- View


{-|
Top level view function for the content editor SPA.
-}
view : Model -> Html Msg
view model =
    case model.session of
        Initial ->
            Html.div [] []

        Welcome { welcome } ->
            Welcome.Auth.loginView welcome |> Html.map WelcomeMsg

        FailedAuth { welcome } ->
            Welcome.Auth.notPermittedView welcome |> Html.map WelcomeMsg

        Authenticated { contentEditor } ->
            CE.view contentEditor |> Html.map ContentEditorMsg



-- The Program


{-|
The entry point for the client side content editor.
-}
main : Routing.RouteUrlProgram Never Model Msg
main =
    routeMain


debugMain =
    let
        navApp =
            Routing.navigationApp
                { delta2url = delta2url
                , location2messages = location2messages
                , init = init
                , update = update
                , subscriptions = subscriptions
                , view = view
                }
    in
        TimeTravel.program navApp.locationToMessage
            { init = navApp.init
            , subscriptions = navApp.subscriptions
            , update = navApp.update
            , view = navApp.view
            }


routeMain : Routing.RouteUrlProgram Never Model Msg
routeMain =
    Routing.program
        { delta2url = delta2url
        , location2messages = location2messages
        , init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
