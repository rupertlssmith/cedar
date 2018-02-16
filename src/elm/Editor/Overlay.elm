module Editor.Overlay
    exposing
        ( Msg
        , OutMsg(..)
        , Model
        , init
        , subscriptions
        , update
        , view
        , makeAware
        , makeActive
        , makeInactive
        , clear
        , resize
        , scroll
        )

import Animation exposing (px, Property, Interpolation)
import AnimationUtil exposing (animateStyle)
import Color
import DOM exposing (Rectangle)
import Ease
import Editor.ControlBar as ControlBar
import Editor.OverlayState as OverlayState
    exposing
        ( OverlayState(..)
        , Position
        , State
        , Allowed
        , untag
        , hidden
        , mapPosition
        , mapValue
        , mapControlBar
        , toAwareWithPosition
        , toActiveWithControlBarAndValue
        , toActiveWithValue
        , toInactive
        )
import Html.Attributes exposing (class, href)
import Html.Events as Events
import Html exposing (Html, text, div, button, textarea)
import Mouse
import Optional exposing (optional)
import RectUtils exposing (enlarge, noRect, zeroPosition, translate)
import ResizeObserver exposing (ResizeEvent)
import ScrollPort exposing (Move)
import Style exposing (StyleSheet)
import Time exposing (second, Time)


type Msg
    = Animate Animation.Msg
    | ControlBar ControlBar.Msg
    | ControlBarSelect ControlBar.OutMsg
    | ClickOverlay
    | MouseOut
    | ClickOut
    | UpdateContent String
    | MouseMove Mouse.Position


type OutMsg
    = ContentValue String
    | Closed
    | SelectMode String


type alias Model =
    { state : OverlayState }



-- Initialization


init : Model
init =
    { state = hidden }


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



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        overlayAnimSub state =
            Animation.subscription Animate [ state.position.overlayStyle ]

        controlBarSub state =
            ControlBar.subscriptions state.controlBar |> Sub.map ControlBar
    in
        case model.state of
            Aware state ->
                Sub.batch [ overlayAnimSub (untag state), Mouse.moves MouseMove ]

            Active state ->
                Sub.batch [ overlayAnimSub (untag state), controlBarSub (untag state) ]

            Inactive state ->
                Sub.batch [ overlayAnimSub (untag state), controlBarSub (untag state) ]

            _ ->
                Sub.none



-- State updates


debugFilter : Msg -> Msg
debugFilter msg =
    case msg of
        -- Animate _ ->
        --     msg
        --
        -- ControlBar _ ->
        --     msg
        _ ->
            -- Debug.log "overlay" msg
            msg


noop model =
    ( model, Cmd.none, Nothing )


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    case model.state of
        Aware state ->
            case (debugFilter msg) of
                Animate msg ->
                    ( { model | state = mapPosition (updateAnimate msg) model.state }
                    , Cmd.none
                    , Nothing
                    )

                ClickOverlay ->
                    updateClickOverlay model

                MouseOut ->
                    updateMouseOut model

                MouseMove position ->
                    updateMouseMove position model

                _ ->
                    noop model

        Active state ->
            case (debugFilter msg) of
                Animate msg ->
                    ( { model | state = mapPosition (updateAnimate msg) model.state }
                    , Cmd.none
                    , Nothing
                    )

                ControlBar msg ->
                    ( { model | state = mapControlBar (updateControlBarUpdate msg) model.state }
                    , Cmd.none
                    , Nothing
                    )

                ControlBarSelect (ControlBar.Selected name) ->
                    updateControlBar name model

                ClickOut ->
                    updateClickOut model

                UpdateContent value ->
                    updateUpdateContent value model

                _ ->
                    noop model

        Inactive state ->
            case (debugFilter msg) of
                Animate msg ->
                    ( { model | state = mapPosition (updateAnimate msg) model.state }
                    , Cmd.none
                    , Nothing
                    )

                ControlBar msg ->
                    ( { model | state = mapControlBar (updateControlBarUpdate msg) model.state }
                    , Cmd.none
                    , Nothing
                    )

                ControlBarSelect (ControlBar.Selected name) ->
                    updateControlBar name model

                ClickOut ->
                    updateClickOut model

                _ ->
                    noop model

        Hidden state ->
            noop model


updateAnimate : Animation.Msg -> Position -> Position
updateAnimate msg position =
    { position | overlayStyle = Animation.update msg position.overlayStyle }


updateControlBarUpdate : ControlBar.Msg -> ControlBar.Model -> ControlBar.Model
updateControlBarUpdate msg controlBar =
    ControlBar.update msg controlBar


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
    , SelectMode "markdown" |> Just
    )


updateMouseOut : Model -> ( Model, Cmd Msg, Maybe OutMsg )
updateMouseOut model =
    ( { model | state = hidden }
    , Cmd.none
    , (Just Closed)
    )


isWithin : Mouse.Position -> Rectangle -> Bool
isWithin position rectangle =
    (position.x > round rectangle.left)
        && (position.x < round (rectangle.left + rectangle.width))
        && (position.y > round rectangle.top)
        && (position.y < round (rectangle.top + rectangle.height))


updateMouseMove : Mouse.Position -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
updateMouseMove position model =
    case model.state of
        Aware state ->
            let
                untagged =
                    untag state
            in
                if (isWithin position untagged.position.rect) then
                    ( model, Cmd.none, Nothing )
                else
                    ( { model | state = hidden }
                    , Cmd.none
                    , (Just Closed)
                    )

        _ ->
            ( model, Cmd.none, Nothing )


updateClickOut : Model -> ( Model, Cmd Msg, Maybe OutMsg )
updateClickOut model =
    ( { model | state = hidden }
    , Cmd.none
    , Just Closed
    )


updateUpdateContent : String -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
updateUpdateContent value model =
    ( { model | state = mapValue (always value) model.state }
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
        case model.state of
            Hidden state ->
                { model
                    | state =
                        toAwareWithPosition
                            { rect = rect
                            , yOffset = 0.0
                            , overlayStyle = newOverlayStyle rect
                            }
                            state
                }

            _ ->
                model


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

        updateStyle position =
            { position
                | overlayStyle = newOverlayStyle position.overlayStyle position.rect
                , yOffset = yOffset
            }
    in
        case model.state of
            Aware state ->
                { model | state = toActiveWithControlBarAndValue controlBar value state |> mapPosition updateStyle }

            Inactive state ->
                { model | state = toActiveWithValue value state |> mapPosition updateStyle }

            _ ->
                model


makeInactive : Model -> Model
makeInactive model =
    case model.state of
        Active state ->
            { model | state = toInactive state }

        _ ->
            model


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

                Active _ ->
                    overlayResizeStyle rect currentStyle overlayActiveStyle

                Inactive _ ->
                    overlayResizeStyle rect currentStyle overlayActiveStyle

                _ ->
                    currentStyle

        move position =
            let
                newRect =
                    resize size position.rect
            in
                { position
                    | rect = newRect
                    , overlayStyle = newOverlayStyle newRect position.overlayStyle
                }
    in
        { model | state = mapPosition move model.state }


scroll : Move -> Model -> Model
scroll ( from, to ) model =
    let
        move to position =
            { position | yOffset = to }
    in
        { model | state = mapPosition (move to) model.state }


clear : Model -> Model
clear model =
    { model | state = hidden }



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
    (Animation.exactly "border-style" "none")
        :: (Animation.borderColor (Color.rgba 0 0 0 0.0))
        :: (Animation.borderWidth (px 0.0))
        :: (Animation.backgroundColor (Color.rgba 0 0 0 0.0))
        :: positionStyle noRect


overlayPositionedStyle : Rectangle -> List Property
overlayPositionedStyle rect =
    (Animation.exactly "border-style" "dotted")
        :: (Animation.padding (px 0.0))
        :: (Animation.borderColor (Color.rgba 0 0 0 0.0))
        :: (Animation.borderWidth (px 5.0))
        :: (Animation.backgroundColor (Color.rgba 0 0 0 0.0))
        :: (zeroPosition >> enlarge 2.0 >> positionStyle) rect


overlayAwareStyle : Rectangle -> List Property
overlayAwareStyle rect =
    (Animation.exactly "border-style" "dotted")
        :: (Animation.borderColor (Color.rgba 0 0 0 0.4))
        :: (Animation.borderWidth (px 1.0))
        :: (Animation.backgroundColor (Color.rgba 0 0 0 0.04))
        :: (zeroPosition >> enlarge 6.0 >> positionStyle) rect


overlayActiveStyle : Rectangle -> List Property
overlayActiveStyle rect =
    (Animation.exactly "border-style" "dotted")
        :: (Animation.borderColor (Color.rgba 0 0 0 0.8))
        :: (Animation.borderWidth (px 2.0))
        :: (Animation.backgroundColor (Color.rgba 255 255 255 0.1))
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
        case model.state of
            Hidden state ->
                div [] []

            Aware state ->
                let
                    untagged =
                        untag state
                in
                    div (attributes untagged.position)
                        [ overlayFrame untagged Nothing ]

            Active state ->
                let
                    untagged =
                        untag state
                in
                    div (attributes untagged.position)
                        [ overlayFrame untagged (overlayEditor untagged |> Just)
                        , clickPlane untagged
                        , controlBar untagged
                        ]

            Inactive state ->
                let
                    untagged =
                        untag state
                in
                    div (attributes untagged.position)
                        [ overlayFrame untagged Nothing
                        , clickPlane untagged
                        , controlBar untagged
                        ]


controlBar : { m | controlBar : ControlBar.Model } -> Html Msg
controlBar state =
    ControlBar.view "editor-overlay__controlbar" state.controlBar
        |> Html.map ControlBarSelect


clickPlane : { m | position : Position } -> Html Msg
clickPlane state =
    let
        stylesheet =
            clickPlaneStylesheet <| translate 0 -state.position.yOffset <| enlarge 7.0 state.position.rect
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


overlayFrame : { m | position : Position } -> Maybe (Html Msg) -> Html Msg
overlayFrame state maybeEditor =
    div
        (Animation.render
            state.position.overlayStyle
            ++ [ class "editor-overlay__frame"
               , Events.onMouseOut MouseOut
               , Events.onClick ClickOverlay
               ]
        )
        (optional [ maybeEditor ])


overlayEditor : { m | value : String } -> Html Msg
overlayEditor state =
    div [ class "editor-overlay__editor" ]
        [ textarea
            [ class "editor-overlay__textarea"
            , Events.onInput UpdateContent
            ]
            [ text state.value ]
        ]
