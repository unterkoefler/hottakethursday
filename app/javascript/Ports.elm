port module Ports exposing (..)

import Json.Decode


{-| Stores the given auth token in persistent storage.
-}
port storeAuthToken : Json.Decode.Value -> Cmd msg


{-| Clears whatever auth token was stored in persistent storage
-}
port clearAuthToken : () -> Cmd msg


{-| Called with any takes received from the server that weren't requested explicitly.
-}
port newTakeInfo : (Json.Decode.Value -> msg) -> Sub msg


{-| Logs at the info level
-}
port info : String -> Cmd msg


{-| Logs at the error level
-}
port error : String -> Cmd msg
