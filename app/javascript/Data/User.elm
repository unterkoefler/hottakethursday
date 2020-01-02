module Data.User exposing (..)

import Json.Decode


type alias User =
    { id : Int
    , username : String
    , avatarUrl : Maybe String
    }


decoder : Json.Decode.Decoder User
decoder =
    Json.Decode.map3 User
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "username" Json.Decode.string)
        (Json.Decode.maybe (Json.Decode.field "avatar_url" Json.Decode.string))
