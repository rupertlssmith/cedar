module Editor.Overlay exposing
    ( Model
    , Msg
    , OutMsg(..)
    , clear
    , init
    , makeActive
    , makeAware
    , makeInactive
    , resize
    , scroll
    , subscriptions
    , update
    , view
    )

import Animation exposing (Interpolation, Property, px)
import AnimationUtil exposing (animateStyle)
import Color
import DOM exposing (Rectangle)
import Ease
import Editor.ControlBar as ControlBar
import Function exposing (swirlr)
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (class, href)
import Html.Events as Events
import Maybe exposing (andThen)
import Maybe.Extra exposing (isJust, join, orElse, unwrap)
import Optional exposing (optional, required, when)
import ResizeObserver exposing (ResizeEvent)
import ScrollPort exposing (Move)
import StateModel exposing ((>&&>), (>||>), boolToMaybe, defaultTransition, mapWhenCompose)
import Style exposing (StyleSheet)
import Time exposing (Time, second)


type Msg
    = Animate Animation.Msg
    | ControlBarUpdate ControlBar.Msg
    | ControlBar ControlBar.OutMsg
    | ClickOverlay
    | MouseOut
    | ClickOut
    | UpdateContent String


type OutMsg
    = ContentValue String
    | Closed
    | SelectMode String


type alias Model =
    { state : State }


type alias WithPosition =
    { rect : Rectangle
    , yOffset : Float
    , overlayStyle : Animation.State
    }


type alias WithValue =
    { value : String }


type alias WithControlBar =
    { controlBar : ControlBar.Model }


type State
    = Hidden
    | Aware WithPosition
    | Active WithPosition WithControlBar WithValue
    | Inactive WithPosition WithControlBar


maybeHidden : State -> Maybe State
maybeHidden state =
    case state of
        Hidden ->
            Just state

        _ ->
            Nothing


maybeAware : State -> Maybe State
maybeAware state =
    case state of
        Aware _ ->
            Just state

        _ ->
            Nothing


maybeActive : State -> Maybe State
maybeActive state =
    case state of
        Active _ _ _ ->
            Just state

        _ ->
            Nothing


maybeInactive : State -> Maybe State
maybeInactive state =
    case state of
        Inactive _ _ ->
            Just state

        _ ->
            Nothing


mapWhenWithPosition : (WithPosition -> a) -> State -> Maybe a
mapWhenWithPosition func state =
    case state of
        Aware position ->
            Just <| func position

        Active position _ _ ->
            Just <| func position

        Inactive position _ ->
            Just <| func position

        _ ->
            Nothing


mapWhenWithValue : (WithValue -> a) -> State -> Maybe a
mapWhenWithValue func state =
    case state of
        Active _ _ value ->
            Just <| func value

        _ ->
            Nothing


mapWhenWithControlBar : (WithControlBar -> a) -> State -> Maybe a
mapWhenWithControlBar func state =
    case state of
        Active _ controlBar _ ->
            Just <| func controlBar

        Inactive _ controlBar ->
            Just <| func controlBar

        _ ->
            Nothing


updateWhenWithPosition : (WithPosition -> WithPosition) -> State -> Maybe State
updateWhenWithPosition func state =
    case state of
        Aware position ->
            func position |> Aware |> Just

        Active position controlBar value ->
            func position |> (\position -> Active position controlBar value) |> Just

        Inactive position controlBar ->
            func position |> (\b a -> Inactive a b) controlBar |> Just

        _ ->
            Nothing


updateWhenWithValue : (WithValue -> WithValue) -> State -> Maybe State
updateWhenWithValue func state =
    case state of
        Active position controlBar value ->
            func value |> Active position controlBar |> Just

        _ ->
            Nothing


updateWhenWithControlBar : (WithControlBar -> WithControlBar) -> State -> Maybe State
updateWhenWithControlBar func state =
    case state of
        Active position controlBar value ->
            func controlBar |> (\b a -> Active position a b) value |> Just

        Inactive position controlBar ->
            func controlBar |> Inactive position |> Just

        _ ->
            Nothing


toHidden : State -> State
toHidden _ =
    Hidden


toAware : WithPosition -> State -> Maybe State
toAware position state =
    case state of
        Hidden ->
            Just <| Aware position

        _ ->
            Nothing


awareToActive : WithControlBar -> WithValue -> State -> Maybe State
awareToActive controlBar value =
    maybeAware
        >&&> mapWhenWithPosition (\position -> Active position controlBar value)


withControlBarToActive : WithValue -> State -> Maybe State
withControlBarToActive value =
    mapWhenCompose mapWhenWithPosition mapWhenWithControlBar (swirlr Active value)


toInactive : State -> Maybe State
toInactive state =
    mapWhenCompose mapWhenWithPosition mapWhenWithControlBar Inactive state



-- Initialization


init : Model
init =
    { state = Hidden }


initControlBar : ControlBar.Model
initControlBar =
    ControlBar.initWithAnimation "overlay"
        controlBarPositionedStyle
        controlBarActiveStyle
        controlBarEasing
        controlBarEasing
        False
        [ ( "markdown", "control-icon control-icon__markdown" )
        , ( "wysiwyg", "control-icon control-icon__wysiwyg" )
        , ( "preview", "control-icon control-icon__preview" )
        , ( "save", "control-icon control-icon__save" )
        ]



-- Position and size calculations


enlarge : Float -> Rectangle -> Rectangle
enlarge px rect =
    { rect
        | top = rect.top - px
        , left = rect.left - px
        , width = rect.width + 2 * px
        , height = rect.height + 2 * px
    }


noRect : Rectangle
noRect =
    { top = 0.0, left = 0.0, width = 0.0, height = 0.0 }


zeroPosition : Rectangle -> Rectangle
zeroPosition rect =
    { rect | top = 0.0, left = 0.0 }


translate : Float -> Float -> Rectangle -> Rectangle
translate x y rect =
    { rect
        | top = rect.top + y
        , left = rect.left + x
    }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (optional
            [ mapWhenWithPosition
                (\position -> Animation.subscription Animate [ position.overlayStyle ])
                model.state
            , mapWhenWithControlBar
                (\controlBar -> ControlBar.subscriptions controlBar.controlBar |> Sub.map ControlBarUpdate)
                model.state
            ]
        )



-- State updates


debugFilter : Msg -> Msg
debugFilter msg =
    case msg of
        Animate _ ->
            msg

        ControlBarUpdate _ ->
            msg

        _ ->
            Debug.log "overlay" msg


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    case debugFilter msg of
        Animate msg ->
            -- update animation when with position
            updateAnimate msg model

        ControlBarUpdate msg ->
            -- update control bar when with control bar
            updateControlBarUpdate msg model

        ControlBar (ControlBar.Selected name) ->
            -- turn a control bar selection into an out message
            updateControlBar name model

        ClickOverlay ->
            -- when aware select markdown mode
            updateClickOverlay model

        MouseOut ->
            -- when aware hide
            updateMouseOut model

        ClickOut ->
            -- hide
            updateClickOut model

        UpdateContent value ->
            -- when active update the content
            updateUpdateContent value model


updateAnimate : Animation.Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
updateAnimate msg model =
    ( { model
        | state =
            updateWhenWithPosition
                (\position ->
                    { position | overlayStyle = Animation.update msg position.overlayStyle }
                )
                |> defaultTransition model.state
      }
    , Cmd.none
    , Nothing
    )


updateControlBarUpdate : ControlBar.Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
updateControlBarUpdate msg model =
    ( { model
        | state =
            updateWhenWithControlBar
                (\controlBar ->
                    { controlBar | controlBar = ControlBar.update msg controlBar.controlBar }
                )
                |> defaultTransition model.state
      }
    , Cmd.none
    , Nothing
    )


updateControlBar : String -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
updateControlBar name model =
    if name == "markdown" then
        ( model, Cmd.none, Just <| SelectMode name )

    else if name == "preview" then
        ( model, Cmd.none, Just <| SelectMode name )

    else if name == "save" then
        ( model, Cmd.none, Just <| SelectMode name )

    else
        ( model, Cmd.none, Nothing )


updateClickOverlay : Model -> ( Model, Cmd Msg, Maybe OutMsg )
updateClickOverlay model =
    ( model
    , Cmd.none
    , SelectMode "markdown"
        |> Just
        |> Maybe.Extra.next (maybeAware model.state)
    )


updateMouseOut : Model -> ( Model, Cmd Msg, Maybe OutMsg )
updateMouseOut model =
    ( { model
        | state =
            Maybe.Extra.unwrap
                model.state
                toHidden
                (maybeAware model.state)
      }
    , Cmd.none
    , maybeAware model.state |> Maybe.Extra.prev (Just Closed)
    )


updateClickOut : Model -> ( Model, Cmd Msg, Maybe OutMsg )
updateClickOut model =
    ( { model | state = toHidden model.state }
    , Cmd.none
    , Just Closed
    )


updateUpdateContent : String -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
updateUpdateContent value model =
    ( { model
        | state =
            updateWhenWithValue (\withValue -> { withValue | value = value })
                |> defaultTransition model.state
      }
    , Cmd.none
    , ContentValue value |> Just
    )



-- Public API


makeAware : Rectangle -> Model -> Model
makeAware rect model =
    let
        newOverlayStyle rect =
            animateStyle
                overlayEasing
                (overlayPositionedStyle rect |> Animation.style)
                (overlayAwareStyle rect)
    in
    { model
        | state =
            toAware
                { rect = rect
                , yOffset = 0.0
                , overlayStyle = newOverlayStyle rect
                }
                |> defaultTransition model.state
    }


makeActive : Float -> String -> Model -> Model
makeActive yOffset value model =
    let
        newOverlayStyle currentStyle rect =
            animateStyle
                overlayActiveEasing
                currentStyle
                (overlayActiveStyle rect)

        controlBar =
            ControlBar.show initControlBar
    in
    { model
        | state =
            awareToActive { controlBar = controlBar } { value = value }
                >||> withControlBarToActive { value = value }
                >&&>
                    updateWhenWithPosition
                        (\position ->
                            { position
                                | overlayStyle = newOverlayStyle position.overlayStyle position.rect
                                , yOffset = yOffset
                            }
                        )
                |> defaultTransition model.state
    }


makeInactive : Model -> Model
makeInactive model =
    { model | state = toInactive |> defaultTransition model.state }


resize : ResizeEvent -> Model -> Model
resize size model =
    let
        resize size rect =
            { rect | width = size.width, height = size.height }

        overlayResizeStyle rect currentStyle styleForRect =
            animateStyle
                resizeEasing
                currentStyle
                (styleForRect rect)

        newOverlayStyle rect currentStyle =
            case model.state of
                Aware _ ->
                    overlayResizeStyle rect currentStyle overlayAwareStyle

                Active _ _ _ ->
                    overlayResizeStyle rect currentStyle overlayActiveStyle

                Inactive _ _ ->
                    overlayResizeStyle rect currentStyle overlayActiveStyle

                _ ->
                    currentStyle
    in
    { model
        | state =
            updateWhenWithPosition
                (\position ->
                    let
                        newRect =
                            resize size position.rect
                    in
                    { position
                        | rect = newRect
                        , overlayStyle = newOverlayStyle newRect position.overlayStyle
                    }
                )
                |> defaultTransition model.state
    }


scroll : Move -> Model -> Model
scroll ( from, to ) model =
    { model
        | state =
            updateWhenWithPosition (\position -> { position | yOffset = to })
                |> defaultTransition model.state
    }


clear : Model -> Model
clear model =
    { model | state = toHidden model.state }



-- Animation Styles


positionStyle : Rectangle -> List Property
positionStyle rect =
    [ Animation.left (px rect.left)
    , Animation.top (px rect.top)
    , Animation.width (px rect.width)
    , Animation.height (px rect.height)
    ]


overlayHiddenStyle : List Property
overlayHiddenStyle =
    Animation.exactly "border-style" "none"
        :: Animation.borderColor (Color.rgba 0 0 0 0.0)
        :: Animation.borderWidth (px 0.0)
        :: Animation.backgroundColor (Color.rgba 0 0 0 0.0)
        :: positionStyle noRect


overlayPositionedStyle : Rectangle -> List Property
overlayPositionedStyle rect =
    Animation.exactly "border-style" "dotted"
        :: Animation.padding (px 0.0)
        :: Animation.borderColor (Color.rgba 0 0 0 0.0)
        :: Animation.borderWidth (px 5.0)
        :: Animation.backgroundColor (Color.rgba 0 0 0 0.0)
        :: (zeroPosition >> enlarge 2.0 >> positionStyle) rect


overlayAwareStyle : Rectangle -> List Property
overlayAwareStyle rect =
    Animation.exactly "border-style" "dotted"
        :: Animation.borderColor (Color.rgba 0 0 0 0.4)
        :: Animation.borderWidth (px 1.0)
        :: Animation.backgroundColor (Color.rgba 0 0 0 0.04)
        :: (zeroPosition >> enlarge 6.0 >> positionStyle) rect


overlayActiveStyle : Rectangle -> List Property
overlayActiveStyle rect =
    Animation.exactly "border-style" "dotted"
        :: Animation.borderColor (Color.rgba 0 0 0 0.8)
        :: Animation.borderWidth (px 2.0)
        :: Animation.backgroundColor (Color.rgba 255 255 255 0.1)
        :: (zeroPosition >> enlarge 7.0 >> positionStyle) rect


overlayEasing : Interpolation
overlayEasing =
    Animation.easing
        { duration = 2.0e-1 * second
        , ease = Ease.inQuart
        }


overlayActiveEasing : Interpolation
overlayActiveEasing =
    Animation.easing
        { duration = 5.0e-2 * second
        , ease = Ease.inQuart
        }


resizeEasing : Interpolation
resizeEasing =
    Animation.easing
        { duration = 1.0e-4 * second
        , ease = Ease.inQuart
        }


controlBarPositionedStyle : List Property
controlBarPositionedStyle =
    [ Animation.top (px 0.0) ]


controlBarActiveStyle : List Property
controlBarActiveStyle =
    [ Animation.top (px -39.0) ]


controlBarEasing : Interpolation
controlBarEasing =
    Animation.easing
        { duration = 5.0e-1 * second
        , ease = Ease.inOut Ease.inQuad Ease.outBack
        }



-- Other computed styles.


type ClickPlaneSection
    = TopLeft
    | TopRight
    | BottomLeft
    | BottomRight


clickPlaneStylesheet : Rectangle -> StyleSheet ClickPlaneSection msg
clickPlaneStylesheet rect =
    Style.renderWith [ Style.base [] ]
        [ Style.class TopLeft
            [ Style.topLeft 0.0 0.0
            , Style.width <| Style.px rect.left
            , Style.height <| Style.px (rect.top + rect.height)
            ]
        , Style.class TopRight
            [ Style.topLeft rect.left 0.0
            , Style.width <| Style.percent 100.0
            , Style.height <| Style.px rect.top
            ]
        , Style.class BottomLeft
            [ Style.topLeft 0.0 (rect.top + rect.height)
            , Style.width <| Style.px (rect.left + rect.width)
            , Style.height <| Style.percent 800.0
            ]
        , Style.class BottomRight
            [ Style.topLeft (rect.left + rect.width) rect.top
            , Style.width <| Style.percent 100.0
            , Style.height <| Style.percent 800.0
            ]
        ]



-- View


view : Model -> Html Msg
view model =
    let
        attributes position =
            (positionStyle >> Animation.style >> Animation.render) position.rect
                ++ [ class "editor-overlay__container" ]
    in
    div
        (mapWhenWithPosition attributes model.state |> Maybe.withDefault [])
        (optional
            [ mapWhenWithPosition (\_ -> overlayFrame model.state) model.state
            , (maybeActive
                >||> maybeInactive
                >&&> mapWhenWithPosition (\{ rect, yOffset } -> clickPlane yOffset rect)
              )
                model.state
            , mapWhenWithControlBar controlBar model.state
            ]
        )


controlBar : WithControlBar -> Html Msg
controlBar controlBar =
    ControlBar.view "editor-overlay__controlbar" controlBar.controlBar
        |> Html.map ControlBar


clickPlane : Float -> Rectangle -> Html Msg
clickPlane yOffset rect =
    let
        stylesheet =
            clickPlaneStylesheet <| translate 0 -yOffset <| enlarge 7.0 rect
    in
    div []
        [ Style.embed stylesheet
        , div
            [ class "click-plane__section"
            , stylesheet.class TopLeft
            , Events.onClick ClickOut
            ]
            []
        , div
            [ class "click-plane__section"
            , stylesheet.class TopRight
            , Events.onClick ClickOut
            ]
            []
        , div
            [ class "click-plane__section"
            , stylesheet.class BottomLeft
            , Events.onClick ClickOut
            ]
            []
        , div
            [ class "click-plane__section"
            , stylesheet.class BottomRight
            , Events.onClick ClickOut
            ]
            []
        ]


overlayFrame : State -> Html Msg
overlayFrame state =
    let
        attributes position =
            Animation.render position.overlayStyle
                ++ [ class "editor-overlay__frame"
                   , Events.onMouseOut MouseOut
                   , Events.onClick ClickOverlay
                   ]
    in
    div
        (mapWhenWithPosition attributes state |> Maybe.withDefault [])
        (optional
            [ mapWhenWithValue overlayEditor state ]
        )


overlayEditor : WithValue -> Html Msg
overlayEditor withValue =
    div [ class "editor-overlay__editor" ]
        [ textarea
            [ class "editor-overlay__textarea"
            , Events.onInput UpdateContent
            ]
            [ text withValue.value ]
        ]