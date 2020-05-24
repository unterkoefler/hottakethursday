module Api exposing
    ( FormError(..)
    , LoginInfo
    , RegistrationInfo
    , SavedUserAuthError(..)
    , UserAuth
    , allTakesFromToday
    , changeBio
    , changeLeastFavoriteColor
    , changeName
    , changePassword
    , deleteAccount
    , deleteTake
    , encodeUserAuth
    , like
    , loadUserAuth
    , makeTake
    , me
    , sendForgotPasswordEmail
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
import Dict exposing (Dict)
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


type alias RegistrationInfo =
    { email : String
    , password : String
    , name : String
    , username : String
    , birthday : String
    }


type alias LoginInfo a =
    { a
        | email : String
        , password : String
    }


signUp : RegistrationInfo -> (Result FormError { user : User.User, auth : UserAuth } -> msg) -> Cmd msg
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
                        , ( "full_name", Json.Encode.string registrationInfo.name )
                        , ( "username", Json.Encode.string registrationInfo.username )
                        , ( "birthday", Json.Encode.string registrationInfo.birthday )
                        ]
                  )
                ]

        httpRequest =
            Http.post { url = url, body = Http.jsonBody json, expect = expectAuthHeader signupFieldError onFinish }
    in
    httpRequest


type FormError
    = HttpError Http.Error
    | FieldError (Dict String String)
    | InvalidUserReturned Json.Decode.Error
    | NoAuthToken


expectAuthHeader : (Http.Metadata -> String -> Result FormError { user : User.User, auth : UserAuth }) -> (Result FormError { user : User.User, auth : UserAuth } -> msg) -> Http.Expect msg
expectAuthHeader handleBadStatus toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (HttpError <| Http.BadUrl url)

                Http.Timeout_ ->
                    Err (HttpError <| Http.Timeout)

                Http.NetworkError_ ->
                    Err (HttpError <| Http.NetworkError)

                Http.BadStatus_ metadata data ->
                    handleBadStatus metadata data

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


expectFormError : (Result FormError () -> msg) -> Http.Expect msg
expectFormError toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (HttpError <| Http.BadUrl url)

                Http.Timeout_ ->
                    Err (HttpError <| Http.Timeout)

                Http.NetworkError_ ->
                    Err (HttpError <| Http.NetworkError)

                Http.BadStatus_ metadata data ->
                    signupFieldError metadata data

                Http.GoodStatus_ metadata body ->
                    Ok ()


justStatusCode : Http.Metadata -> String -> Result FormError a
justStatusCode metadata _ =
    Err (HttpError <| Http.BadStatus metadata.statusCode)


signupFieldError : Http.Metadata -> String -> Result FormError a
signupFieldError metadata body =
    case metadata.statusCode of
        400 ->
            case Json.Decode.decodeString errorDecoder body of
                Ok e ->
                    Err e

                Err _ ->
                    justStatusCode metadata body

        _ ->
            justStatusCode metadata body


errorDecoder : Json.Decode.Decoder FormError
errorDecoder =
    Json.Decode.map FieldError <|
        Json.Decode.field "errors" <|
            Json.Decode.index 0 <|
                Json.Decode.field "detail" <|
                    Json.Decode.dict <|
                        Json.Decode.index 0 <|
                            Json.Decode.string


signIn : LoginInfo a -> (Result FormError { user : User.User, auth : UserAuth } -> msg) -> Cmd msg
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
                , expect = expectAuthHeader justStatusCode onFinish
                }
    in
    httpRequest


sendForgotPasswordEmail : String -> (Result FormError () -> msg) -> Cmd msg
sendForgotPasswordEmail email onFinish =
    let
        url =
            Url.Builder.relative (baseUrlComponents ++ [ "users", "password" ]) []

        json =
            Json.Encode.object [ ( "email", Json.Encode.string email ) ]

        httpRequest =
            Http.post
                { url = url
                , body = Http.jsonBody json
                , expect = expectFormError onFinish
                }
    in
    httpRequest


changePassword : String -> String -> (Result FormError () -> msg) -> Cmd msg
changePassword token password onFinish =
    let
        url =
            Url.Builder.relative (baseUrlComponents ++ [ "users", "password" ]) []

        json =
            Json.Encode.object
                [ ( "password", Json.Encode.string password )
                , ( "reset_password_token", Json.Encode.string token )
                ]

        httpRequest =
            Http.request
                { method = "PUT"
                , headers = []
                , url = url
                , body = Http.jsonBody json
                , expect = expectFormError onFinish
                , timeout = Nothing
                , tracker = Nothing
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


type alias UserWithTakes =
    { user : User.User
    , takes : List Take.Take
    }


userWithTakesDecoder : Json.Decode.Decoder UserWithTakes
userWithTakesDecoder =
    Json.Decode.map2 UserWithTakes
        (Json.Decode.field "user" User.decoder)
        (Json.Decode.field "takes" <| Json.Decode.list Take.decoder)


userById : UserAuth -> Int -> (Result Http.Error UserWithTakes -> msg) -> Cmd msg
userById auth userId onFinish =
    authenticatedGet
        { auth = auth
        , url = Url.Builder.relative (baseUrlComponents ++ [ "users", "by_id" ]) [ Url.Builder.int "id" userId ]
        , expect = Http.expectJson onFinish userWithTakesDecoder
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


allTakesFromToday : UserAuth -> (Result Http.Error (List Take.Take) -> msg) -> Cmd msg
allTakesFromToday auth onFinish =
    authenticatedGet
        { auth = auth
        , url = Url.Builder.relative (baseUrlComponents ++ [ "takes", "all_from_today" ]) []
        , expect = Http.expectJson onFinish (Json.Decode.list Take.decoder)
        }


deleteTake : UserAuth -> Int -> (Result Http.Error () -> msg) -> Cmd msg
deleteTake (BearerToken token) takeId onFinish =
    let
        url =
            Url.Builder.relative (baseUrlComponents ++ [ "takes", "delete" ]) []

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


deleteAccount : UserAuth -> (Result Http.Error () -> msg) -> Cmd msg
deleteAccount (BearerToken token) onFinish =
    let
        url =
            Url.Builder.relative (baseUrlComponents ++ [ "users", "me", "delete_account" ]) []

        httpRequest =
            Http.request
                { method = "POST"
                , headers = [ Http.header "authorization" token ]
                , url = url
                , body = Http.emptyBody
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


changeName : UserAuth -> String -> (Result Http.Error User.User -> msg) -> Cmd msg
changeName (BearerToken token) name onFinish =
    let
        url =
            Url.Builder.relative (baseUrlComponents ++ [ "users", "me", "change_name" ]) []

        httpRequest =
            Http.request
                { method = "POST"
                , headers = [ Http.header "authorization" token ]
                , url = url
                , body = Http.jsonBody (Json.Encode.object [ ( "full_name", Json.Encode.string name ) ])
                , expect = Http.expectJson onFinish User.decoder
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    httpRequest


changeBio : UserAuth -> String -> (Result Http.Error User.User -> msg) -> Cmd msg
changeBio (BearerToken token) bio onFinish =
    let
        url =
            Url.Builder.relative (baseUrlComponents ++ [ "users", "me", "change_bio" ]) []

        httpRequest =
            Http.request
                { method = "POST"
                , headers = [ Http.header "authorization" token ]
                , url = url
                , body = Http.jsonBody (Json.Encode.object [ ( "bio", Json.Encode.string bio ) ])
                , expect = Http.expectJson onFinish User.decoder
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    httpRequest


changeLeastFavoriteColor : UserAuth -> String -> (Result Http.Error User.User -> msg) -> Cmd msg
changeLeastFavoriteColor (BearerToken token) color onFinish =
    let
        url =
            Url.Builder.relative (baseUrlComponents ++ [ "users", "me", "change_least_fav_color" ]) []

        httpRequest =
            Http.request
                { method = "POST"
                , headers = [ Http.header "authorization" token ]
                , url = url
                , body = Http.jsonBody (Json.Encode.object [ ( "least_fav_color", Json.Encode.string color ) ])
                , expect = Http.expectJson onFinish User.decoder
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    httpRequest
