module Flags exposing (..)

import Json.Decode


type alias Flags =
    { storedJWT : Maybe Json.Decode.Value 
    , dimensions : Dimensions
    }

type alias Dimensions = 
    { width : Int
    , height : Int
    }


defaultFlags : Flags
defaultFlags =
    { storedJWT = Nothing 
    , dimensions = { width = 500, height = 500 }
    }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map2 Flags
        (Json.Decode.field "storedJWT" (Json.Decode.nullable Json.Decode.value))
        (Json.Decode.field "dimensions" dimensionsDecoder)

dimensionsDecoder : Json.Decode.Decoder Dimensions
dimensionsDecoder =
    Json.Decode.map2 Dimensions
        (Json.Decode.field "width" Json.Decode.int)
        (Json.Decode.field "height" Json.Decode.int)



parseFlags : Json.Decode.Value -> Flags
parseFlags flagsValue =
    Json.Decode.decodeValue decoder flagsValue
        |> Result.withDefault defaultFlags
