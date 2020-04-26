module Data.User exposing (..)

import Json.Decode


type alias User =
    { id : Int
    , username : String
    , avatarUrl : Maybe String
    , name : String
    , bio : String
    , leastFavoriteColor : String
    }


decoder : Json.Decode.Decoder User
decoder =
    Json.Decode.map6 User
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "username" Json.Decode.string)
        (Json.Decode.maybe (Json.Decode.field "avatar_url" Json.Decode.string))
        (Json.Decode.field "full_name" Json.Decode.string)
        (Json.Decode.field "bio" Json.Decode.string)
        (Json.Decode.field "least_fav_color" Json.Decode.string)
