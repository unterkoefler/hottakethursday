module Data.Take exposing (..)

import Data.User as User exposing (User)
import Iso8601
import Json.Decode
import Time


type alias Take =
    { content : String
    , postedBy : User
    , timePosted : Time.Posix
    , usersWhoUpLiked : List User
    }


decoder : Json.Decode.Decoder Take
decoder =
    Json.Decode.map4 Take
        (Json.Decode.field "contents" Json.Decode.string)
        (Json.Decode.field "user" User.decoder)
        (Json.Decode.field "created_at" Iso8601.decoder)
        (Json.Decode.field "users_who_liked" (Json.Decode.list User.decoder))
