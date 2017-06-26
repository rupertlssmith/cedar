module Config exposing (Config, config, configDecoder)

{-|
Defines the configuration that the content editor needs to run. This provides
urls for the services with which it interacts. A default configuration and a
decoder for config as json are provided.

@docs Config, config, configDecoder
-}

import Json.Decode as Decode exposing (..)
import Json.Decode.Extra exposing ((|:), withDefault)


{-|
Defines the configuration that the content editor needs to run.
-}
type alias Config =
    { applicationContextRoot : String
    , apiRoot : String
    , avatarApiRoot : String
    }


{-|
Provides a default configuration.
-}
config : Config
config =
    { applicationContextRoot = "/jtrial/"
    , apiRoot = "/jtrial/api/"
    , avatarApiRoot = "/avatar/api/"
    }


{-|
Implements a decoder for the config as json.
-}
configDecoder : Decoder Config
configDecoder =
    (Decode.succeed
        (\applicationContextRoot apiRoot avatarApiRoot ->
            { applicationContextRoot = applicationContextRoot
            , apiRoot = apiRoot
            , avatarApiRoot = avatarApiRoot
            }
        )
    )
        |: field "applicationContextRoot" Decode.string
        |: field "apiRoot" Decode.string
        |: field "avatarApiRoot" Decode.string
