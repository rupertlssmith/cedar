module Client.Main exposing (main)

import Client.Top exposing (delta2url, location2messages, init, update, subscriptions, view)
import RouteUrl as Routing
import TimeTravel.Navigation as TimeTravel


{-|
The entry point for the client side content editor.
-}
main : ResizeObserver.Resize -> ScrollPort.Scroll -> Routing.RouteUrlProgram Never Model Msg
main resize scroll =
    routeMain resize scroll


debugMain resize scroll =
    let
        navApp =
            Routing.navigationApp
                { delta2url = delta2url
                , location2messages = location2messages
                , init = init
                , update = update
                , subscriptions = subscriptions resize scroll
                , view = view
                }
    in
        TimeTravel.program navApp.locationToMessage
            { init = navApp.init
            , subscriptions = navApp.subscriptions
            , update = navApp.update
            , view = navApp.view
            }


routeMain : ResizeObserver.Resize -> ScrollPort.Scroll -> Routing.RouteUrlProgram Never Model Msg
routeMain resize scroll =
    Routing.program
        { delta2url = delta2url
        , location2messages = location2messages
        , init = init
        , update = update
        , subscriptions = subscriptions resize scroll
        , view = view
        }
