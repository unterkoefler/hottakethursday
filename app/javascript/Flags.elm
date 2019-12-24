module Flags exposing (..)

import Json.Decode


type alias Flags =
    { storedJWT : Maybe Json.Decode.Value }


defaultFlags : Flags
defaultFlags =
    { storedJWT = Nothing }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map Flags
        (Json.Decode.field "storedJWT" (Json.Decode.nullable Json.Decode.value))


parseFlags : Json.Decode.Value -> Flags
parseFlags flagsValue =
    Json.Decode.decodeValue decoder flagsValue
        |> Result.withDefault defaultFlags
