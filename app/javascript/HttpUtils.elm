module HttpUtils exposing (httpErrorToString)

import Http


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl msg ->
            "BadUrl[msg=" ++ msg ++ "]"

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus statusCode ->
            "BadStatus[statusCode=" ++ String.fromInt statusCode ++ "]"

        Http.BadBody msg ->
            "BadBody[msg=" ++ msg ++ "]"
