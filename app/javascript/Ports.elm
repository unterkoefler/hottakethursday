port module Ports exposing (..)

import Json.Decode


{-| Stores the given auth token in persistent storage.
-}
port storeAuthToken : Json.Decode.Value -> Cmd msg


{-| Clears whatever auth token was stored in persistent storage
-}
port clearAuthToken : () -> Cmd msg
