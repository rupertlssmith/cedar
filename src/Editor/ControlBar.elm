module Editor.ControlBar exposing
    ( Model
    , Msg
    , OutMsg(..)
    , hide
    , init
    , initWithAnimation
    , show
    , subscriptions
    , update
    , view
    )

import Animation exposing (Interpolation, Property, State, percent, px)
import AnimationUtil exposing (animateStyle)
import Ease
import Html exposing (Html, div, i, li, span, text, ul)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Time exposing (Time, second)


type OutMsg
    = Selected String


type alias Model =
    { id : String
    , shown : Bool
    , animation : Maybe AnimationSpec
    , buttons : List ( String, String )
    }


type alias AnimationSpec =
    { style : Animation.State
    , hiddenStyle : List Property
    , shownStyle : List Property
    , showEasing : Interpolation
    , hideEasing : Interpolation
    }


type Msg
    = Animate String Animation.Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.animation of
        Nothing ->
            Sub.none

        Just spec ->
            Animation.subscription (Animate model.id) [ spec.style ]


init : String -> List ( String, String ) -> Model
init id buttons =
    { id = id, shown = True, animation = Nothing, buttons = buttons }


initWithAnimation :
    String
    -> List Property
    -> List Property
    -> Interpolation
    -> Interpolation
    -> Bool
    -> List ( String, String )
    -> Model
initWithAnimation id hiddenStyle shownStyle showEasing hideEasing show buttons =
    let
        style =
            if show then
                Animation.style shownStyle

            else
                Animation.style hiddenStyle
    in
    { id = id
    , shown = show
    , buttons = buttons
    , animation =
        Just
            { style = style
            , hiddenStyle = hiddenStyle
            , shownStyle = shownStyle
            , showEasing = showEasing
            , hideEasing = hideEasing
            }
    }


show : Model -> Model
show model =
    if model.shown then
        model

    else
        { model
            | shown = True
            , animation =
                Maybe.map
                    (\spec ->
                        { spec
                            | style =
                                animateStyle spec.showEasing spec.style spec.shownStyle
                        }
                    )
                    model.animation
        }


hide : Model -> Model
hide model =
    if not model.shown then
        model

    else
        { model
            | shown = False
            , animation =
                Maybe.map
                    (\spec ->
                        { spec
                            | style =
                                animateStyle spec.hideEasing spec.style spec.hiddenStyle
                        }
                    )
                    model.animation
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Animate id msg ->
            { model | animation = Maybe.map (\spec -> { spec | style = Animation.update msg spec.style }) model.animation }


view : String -> Model -> Html OutMsg
view barDivClass model =
    div
        (Maybe.withDefault []
            (Maybe.map (.style >> Animation.render) model.animation)
            ++ [ class barDivClass
               ]
        )
        (List.map
            (\( name, iconClass ) ->
                i
                    [ class iconClass
                    , onClick <| Selected name
                    ]
                    []
            )
            model.buttons
        )
