module Api exposing (LoginInfo, RegistrationInfo, SignInError(..), UserAuth, signIn, signUp)

import Dict
import Http
import Json.Encode
import Jwt
import Result.Extra
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
    | NoAuthToken
    | MalformedAuthToken


validBearerToken : String -> Bool
validBearerToken bearerToken =
    let
        bearerPrefix =
            "Bearer "

        startsWithBearer =
            String.startsWith bearerPrefix bearerToken

        validJwt =
            Result.Extra.isOk <| Jwt.getTokenHeader (String.dropLeft (String.length bearerPrefix) bearerToken)
    in
    startsWithBearer && validJwt


expectAuthHeader : (Result SignInError UserAuth -> msg) -> Http.Expect msg
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

                Http.GoodStatus_ metadata _ ->
                    case Dict.get "Authorization" metadata.headers of
                        Just auth ->
                            if validBearerToken auth then
                                Ok (BearerToken auth)

                            else
                                Err MalformedAuthToken

                        Nothing ->
                            Err NoAuthToken


signIn : LoginInfo a -> (Result SignInError UserAuth -> msg) -> Cmd msg
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
