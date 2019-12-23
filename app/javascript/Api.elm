module Api exposing (LoginInfo, RegistrationInfo, SignInError(..), UserAuth, signIn, signOut, signUp)

import Data.User as User
import Debug
import Dict
import Http
import Json.Decode
import Json.Encode
import Url.Builder


type UserAuth
    = BearerToken String


baseUrlComponents : List String
baseUrlComponents =
    [ "api", "v1" ]


type alias RegistrationInfo a =
    { a
        | email : String
        , password : String
    }


type alias LoginInfo a =
    { a
        | email : String
        , password : String
    }


signUp : RegistrationInfo a -> (Result Http.Error () -> msg) -> Cmd msg
signUp registrationInfo onFinish =
    let
        url =
            Url.Builder.relative (baseUrlComponents ++ [ "users" ]) []

        json =
            Json.Encode.object
                [ ( "user"
                  , Json.Encode.object
                        [ ( "email", Json.Encode.string registrationInfo.email )
                        , ( "password", Json.Encode.string registrationInfo.password )
                        ]
                  )
                ]

        httpRequest =
            Http.post { url = url, body = Http.jsonBody json, expect = Http.expectWhatever onFinish }
    in
    httpRequest


type SignInError
    = HttpError Http.Error
    | InvalidUserReturned Json.Decode.Error
    | NoAuthToken


expectAuthHeader : (Result SignInError { user : User.User, auth : UserAuth } -> msg) -> Http.Expect msg
expectAuthHeader toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (HttpError <| Http.BadUrl url)

                Http.Timeout_ ->
                    Err (HttpError <| Http.Timeout)

                Http.NetworkError_ ->
                    Err (HttpError <| Http.NetworkError)

                Http.BadStatus_ metadata _ ->
                    Err (HttpError <| Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case Dict.get "authorization" metadata.headers of
                        Just auth ->
                            case Json.Decode.decodeString User.decoder body of
                                Err err ->
                                    Err (InvalidUserReturned err)

                                Ok user ->
                                    Ok { user = user, auth = BearerToken auth }

                        Nothing ->
                            Err NoAuthToken


signIn : LoginInfo a -> (Result SignInError { user : User.User, auth : UserAuth } -> msg) -> Cmd msg
signIn loginInfo onFinish =
    let
        url =
            Url.Builder.relative (baseUrlComponents ++ [ "users", "sign_in" ]) []

        json =
            Json.Encode.object
                [ ( "user"
                  , Json.Encode.object
                        [ ( "email", Json.Encode.string loginInfo.email )
                        , ( "password", Json.Encode.string loginInfo.password )
                        ]
                  )
                ]

        httpRequest =
            Http.post
                { url = url
                , body = Http.jsonBody json
                , expect = expectAuthHeader onFinish
                }
    in
    httpRequest


signOut : UserAuth -> (Result Http.Error () -> msg) -> Cmd msg
signOut (BearerToken token) onFinish =
    let
        url =
            Url.Builder.relative (baseUrlComponents ++ [ "users", "sign_out" ]) []

        httpRequest =
            Http.request
                { method = "DELETE"
                , headers = [ Http.header "authorization" token ]
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectWhatever onFinish
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    httpRequest


me : UserAuth -> (Result Http.Error User.User -> msg) -> Cmd msg
me (BearerToken token) onFinish =
    let
        url =
            Url.Builder.relative (baseUrlComponents ++ [ "users", "me" ]) []

        httpRequest =
            Http.request
                { method = "Get"
                , headers = [ Http.header "authorization" token ]
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson onFinish User.decoder
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    httpRequest
