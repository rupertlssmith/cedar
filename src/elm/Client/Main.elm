module Client.Main exposing (main)

import Client.Top exposing (delta2url, location2messages, init, update, subscriptions, view)
import RouteUrl as Routing
import TimeTravel.Navigation as TimeTravel


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
