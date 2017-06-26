module Config exposing (Config, config, configDecoder)

import Json.Decode as Decode exposing (..)
import Json.Decode.Extra exposing ((|:), withDefault)


type alias Config =
    { applicationContextRoot : String
    , apiRoot : String
    , avatarApiRoot : String
    }


config : Config
config =
    { applicationContextRoot = "/jtrial/"
    , apiRoot = "/jtrial/api/"
    , avatarApiRoot = "/avatar/api/"
    }


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
