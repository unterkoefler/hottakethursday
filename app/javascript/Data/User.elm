module Data.User exposing (..)

import Json.Decode


type alias User =
    { id : Int
    , username : String
    }


decoder : Json.Decode.Decoder User
decoder =
    Json.Decode.map2 User
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "username" Json.Decode.string)
