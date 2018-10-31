module Welcome exposing
    ( Model
    , Msg
    , init
    , loginView
    , notPermittedView
    , update
    )

import Auth
import Html exposing (Html, div, form, h4, img, text)
import Html.Attributes exposing (action, class, href, src, title)


type alias Model =
    { mdl : Material.Model
    , username : String
    , password : String
    }


type Msg
    = Mdl (Material.Msg Msg)
    | GetStarted
    | LogIn
    | TryAgain
    | Cancel
    | UpdateUsername String
    | UpdatePassword String


init : Model
init =
    { mdl = Material.model
    , username = ""
    , password = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Auth.Msg )
update action model =
    case action of
        Mdl action_ ->
            let
                ( newModel, cmd ) =
                    Material.update Mdl action_ model
            in
            ( newModel, cmd, Cmd.none )

        GetStarted ->
            ( model, Cmd.none, Cmd.none )

        LogIn ->
            ( model, Cmd.none, Auth.login { username = model.username, password = model.password } )

        TryAgain ->
            ( model, Cmd.none, Auth.unauthed )

        Cancel ->
            ( model, Cmd.none, Cmd.none )

        UpdateUsername str ->
            ( { model | username = str }, Cmd.none, Cmd.none )

        UpdatePassword str ->
            ( { model | password = str }, Cmd.none, Cmd.none )


rhythm1SpacerDiv : Html msg
rhythm1SpacerDiv =
    div [ class "layout-spacer" ] []


loginView : Model -> Html Msg
loginView model =
    div []
        [ div [ class "layout-fixed-width--one-card" ]
            [ rhythm1SpacerDiv
            , div [ class "mdl-grid" ]
                [ div [ class "mdl-cell mdl-cell--12-col mdl-cell--8-col-tablet mdl-cell--4-col-phone mdl-card mdl-shadow--3dp" ]
                    [ div [ class "mdl-card__media" ]
                        [ img [ src "images/data_center-large.png" ]
                            []
                        ]
                    , div [ class "mdl-card__title" ]
                        [ h4 [ class "mdl-card__title-text" ]
                            [ text "Log In" ]
                        ]
                    , div [ class "mdl-card__supporting-text" ]
                        [ form [ action "#" ]
                            [ Textfield.render Mdl
                                [ 1, 1 ]
                                model.mdl
                                [ Textfield.label "Username"
                                , Textfield.floatingLabel
                                , Textfield.text_
                                , Textfield.value model.username
                                , Options.onInput UpdateUsername
                                ]
                                []
                            , Textfield.render Mdl
                                [ 1, 2 ]
                                model.mdl
                                [ Textfield.label "Password"
                                , Textfield.floatingLabel
                                , Textfield.text_
                                , Textfield.password
                                , Textfield.value model.password
                                , Options.onInput UpdatePassword
                                ]
                                []
                            ]
                        ]
                    , div [ class "mdl-card__actions" ]
                        [ div [ class "control-bar" ]
                            [ div [ class "control-bar__row" ]
                                [ div [ class "control-bar__left-0" ]
                                    [ Button.render Mdl
                                        [ 1, 2 ]
                                        model.mdl
                                        [ Button.colored
                                        , Options.onClick LogIn
                                        ]
                                        [ text "Log In"
                                        , Icon.i "chevron_right"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


notPermittedView : Model -> Html Msg
notPermittedView model =
    div []
        [ div [ class "layout-fixed-width--one-card" ]
            [ rhythm1SpacerDiv
            , div [ class "mdl-grid" ]
                [ div [ class "mdl-cell mdl-cell--12-col mdl-cell--8-col-tablet mdl-cell--4-col-phone mdl-card mdl-shadow--3dp" ]
                    [ div [ class "mdl-card__media" ]
                        [ img [ src "images/data_center-large.png" ]
                            []
                        ]
                    , div [ class "mdl-card__title" ]
                        [ h4 [ class "mdl-card__title-text" ]
                            [ text "Not Authorized" ]
                        ]
                    , div [ class "mdl-card__supporting-text" ]
                        [ form [ action "#" ]
                            [ Textfield.render Mdl
                                [ 1, 1 ]
                                model.mdl
                                [ Textfield.label "Username"
                                , Textfield.floatingLabel
                                , Textfield.text_
                                , Textfield.disabled
                                , Textfield.value model.username
                                ]
                                []
                            , Textfield.render Mdl
                                [ 1, 2 ]
                                model.mdl
                                [ Textfield.label "Password"
                                , Textfield.floatingLabel
                                , Textfield.text_
                                , Textfield.password
                                , Textfield.disabled
                                , Textfield.value model.password
                                ]
                                []
                            ]
                        ]
                    , div [ class "mdl-card__actions" ]
                        [ div [ class "control-bar" ]
                            [ div [ class "control-bar__row" ]
                                [ div [ class "control-bar__left-0" ]
                                    [ Button.render Mdl
                                        [ 2, 1 ]
                                        model.mdl
                                        [ Button.colored
                                        , Options.onClick TryAgain
                                        ]
                                        [ Icon.i "chevron_left"
                                        , text "Try Again"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
