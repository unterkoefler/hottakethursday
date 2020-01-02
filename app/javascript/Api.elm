module Api exposing
    ( LoginInfo
    , RegistrationInfo
    , SavedUserAuthError(..)
    , SignInError(..)
    , UserAuth
    , allTakes
    , allTakesFromToday
    , encodeUserAuth
    , like
    , loadUserAuth
    , makeTake
    , me
    , signIn
    , signOut
    , signUp
    , unlike
    , uploadProfileImage
    , userById
    , usersByIds
    )

import Data.Take as Take
import Data.User as User
import Dict
import File
import Http
import Json.Decode
import Json.Encode
import Jwt
import Task
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
                    case
                        ( Dict.get "authorization" metadata.headers
                        , Json.Decode.decodeString User.decoder body
                        )
                    of
                        ( Just auth, Ok user ) ->
                            Ok { user = user, auth = BearerToken auth }

                        ( Nothing, _ ) ->
                            Err NoAuthToken

                        ( _, Err err ) ->
                            Err (InvalidUserReturned err)


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


authenticatedGet :
    { auth : UserAuth
    , url : String
    , expect : Http.Expect msg
    }
    -> Cmd msg
authenticatedGet { auth, url, expect } =
    let
        (BearerToken token) =
            auth

        httpRequest =
            Http.request
                { method = "Get"
                , headers = [ Http.header "authorization" token ]
                , url = url
                , body = Http.emptyBody
                , expect = expect
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    httpRequest


me : UserAuth -> (Result Http.Error User.User -> msg) -> Cmd msg
me auth onFinish =
    authenticatedGet
        { auth = auth
        , url = Url.Builder.relative (baseUrlComponents ++ [ "users", "me" ]) []
        , expect = Http.expectJson onFinish User.decoder
        }


withoutBearerPrefix : String -> String
withoutBearerPrefix =
    String.dropLeft (String.length "Bearer ")


{-| Encodes the user auth into a json object for uses such as saving into
localstorage
-}
encodeUserAuth : UserAuth -> Json.Encode.Value
encodeUserAuth (BearerToken auth) =
    Json.Encode.string (withoutBearerPrefix auth)


type SavedUserAuthError
    = JwtProblem Jwt.JwtError
    | TokenExpired
    | DecodingProblem Json.Decode.Error


{-| Tries to load user authentication from json. Is a command so that the validity of the jwt
can be checked.
-}
loadUserAuth : Json.Decode.Value -> (Result SavedUserAuthError UserAuth -> msg) -> Cmd msg
loadUserAuth json onFinish =
    Task.attempt onFinish <|
        case Json.Decode.decodeValue Json.Decode.string json of
            Ok token ->
                Jwt.checkTokenExpiry token
                    |> Task.mapError JwtProblem
                    |> Task.andThen
                        (\expired ->
                            if not expired then
                                Task.succeed <| BearerToken ("Bearer " ++ token)

                            else
                                Task.fail TokenExpired
                        )

            Err decodingError ->
                Task.fail (DecodingProblem decodingError)


userById : UserAuth -> Int -> (Result Http.Error User.User -> msg) -> Cmd msg
userById auth userId onFinish =
    authenticatedGet
        { auth = auth
        , url = Url.Builder.relative (baseUrlComponents ++ [ "users", "by_id" ]) [ Url.Builder.int "id" userId ]
        , expect = Http.expectJson onFinish User.decoder
        }


usersByIds : UserAuth -> List Int -> (Result Http.Error (List User.User) -> msg) -> Cmd msg
usersByIds auth userIds onFinish =
    authenticatedGet
        { auth = auth
        , url =
            Url.Builder.relative
                (baseUrlComponents ++ [ "users", "by_ids" ])
                [ Url.Builder.string "ids" (String.join "," (List.map String.fromInt userIds)) ]
        , expect = Http.expectJson onFinish (Json.Decode.list User.decoder)
        }


makeTake : UserAuth -> String -> (Result Http.Error () -> msg) -> Cmd msg
makeTake (BearerToken token) contents onFinish =
    let
        url =
            Url.Builder.relative (baseUrlComponents ++ [ "takes", "create" ]) []

        httpRequest =
            Http.request
                { method = "POST"
                , headers = [ Http.header "authorization" token ]
                , url = url
                , body = Http.jsonBody (Json.Encode.object [ ( "contents", Json.Encode.string contents ) ])
                , expect = Http.expectWhatever onFinish
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    httpRequest


allTakes : UserAuth -> (Result Http.Error (List Take.Take) -> msg) -> Cmd msg
allTakes auth onFinish =
    authenticatedGet
        { auth = auth
        , url = Url.Builder.relative (baseUrlComponents ++ [ "takes", "all" ]) []
        , expect = Http.expectJson onFinish (Json.Decode.list Take.decoder)
        }


allTakesFromToday : UserAuth -> (Result Http.Error (List Take.Take) -> msg) -> Cmd msg
allTakesFromToday auth onFinish =
    authenticatedGet
        { auth = auth
        , url = Url.Builder.relative (baseUrlComponents ++ [ "takes", "all_from_today" ]) []
        , expect = Http.expectJson onFinish (Json.Decode.list Take.decoder)
        }


like : UserAuth -> Int -> (Result Http.Error () -> msg) -> Cmd msg
like (BearerToken token) takeId onFinish =
    let
        url =
            Url.Builder.relative (baseUrlComponents ++ [ "takes", "like" ]) []

        httpRequest =
            Http.request
                { method = "POST"
                , headers = [ Http.header "authorization" token ]
                , url = url
                , body = Http.jsonBody (Json.Encode.object [ ( "take_id", Json.Encode.int takeId ) ])
                , expect = Http.expectWhatever onFinish
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    httpRequest


unlike : UserAuth -> Int -> (Result Http.Error () -> msg) -> Cmd msg
unlike (BearerToken token) takeId onFinish =
    let
        url =
            Url.Builder.relative (baseUrlComponents ++ [ "takes", "unlike" ]) []

        httpRequest =
            Http.request
                { method = "POST"
                , headers = [ Http.header "authorization" token ]
                , url = url
                , body = Http.jsonBody (Json.Encode.object [ ( "take_id", Json.Encode.int takeId ) ])
                , expect = Http.expectWhatever onFinish
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    httpRequest


uploadProfileImage : UserAuth -> File.File -> (Result Http.Error User.User -> msg) -> Cmd msg
uploadProfileImage (BearerToken token) file onFinish =
    let
        url =
            Url.Builder.relative (baseUrlComponents ++ [ "users", "me", "change_avatar" ]) []

        httpRequest =
            Http.request
                { method = "POST"
                , headers = [ Http.header "authorization" token ]
                , url = url
                , body = Http.multipartBody [ Http.filePart "avatar" file ]
                , expect = Http.expectJson onFinish User.decoder
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    httpRequest
