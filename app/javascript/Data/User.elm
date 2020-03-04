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


{-| Use this function for comparing users. User info can come out of sync
from just plain old time delay, so stuff like changing an avatar url might
mean that now we haven't identified that you control a particular take.
-}
isSameUser : User -> User -> Bool
isSameUser u1 u2 =
    u1.id == u2.id
