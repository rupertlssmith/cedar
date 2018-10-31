module AnimationUtil exposing (animateStyle)

import Animation exposing (Interpolation, Property, State)


animateStyle : Interpolation -> State -> List Property -> State
animateStyle easing fromState toState =
    Animation.interrupt
        [ Animation.toWith easing toState
        ]
        fromState
