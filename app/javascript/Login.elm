module Login exposing (Model, Msg, update, view)

import Api
import Browser.Navigation as Nav
import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Ports



-- MODEL


type alias Model =
    { email : String
    , password : String
    , previousInvalidAttempt : Bool
    }


type alias Profile =
    Maybe { user : User, auth : Api.UserAuth }



-- UPDATE


type Msg
    = Submit
    | EmailChanged String
    | PasswordChanged String
    | AttemptCompleted (Result Api.SignInError { user : User.User, auth : Api.UserAuth })


update : Msg -> Model -> Nav.Key -> ( Model, Profile, Cmd Msg )
update msg model navKey =
    case msg of
        Submit ->
            if model.email /= "" && model.password /= "" then
                ( model
                , Nothing
                , Api.signIn model AttemptCompleted
                )

            else
                ( { model | previousInvalidAttempt = True }
                , Nothing
                , Cmd.none
                )

        EmailChanged newEmail ->
            ( { model | email = newEmail }
            , Nothing
            , Cmd.none
            )

        PasswordChanged newPassword ->
            ( { model | password = newPassword }
            , Nothing
            , Cmd.none
            )

        AttemptCompleted (Ok profile) ->
            ( model
            , Just profile
            , Cmd.batch
                [ Ports.storeAuthToken (Api.encodeUserAuth profile.auth)
                , Nav.pushUrl navKey "/"
                ]
            )

        AttemptCompleted (Err _) ->
            -- TODO Determine based on error whether it was actually invalid creds
            ( { model | previousInvalidAttempt = True }
            , Nothing
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "loginBody", class "container-fluid" ]
        [ div [ class "row justify-content-center" ]
            [ div [ class "col form" ]
                (inputWithLabel "email" "Email" model.email EmailChanged
                    ++ inputWithLabel "password" "Password" model.password PasswordChanged
                    ++ [ div [] [ a [ href "forgot-password" ] [ text "Forgot password?" ] ]
                       , div [] [ button [ onClick Submit ] [ text "Continue" ] ]
                       ]
                    ++ (case model.previousInvalidAttempt of
                            True ->
                                [ div [] [ p [ class "text-danger" ] [ text "Invalid Username or Password" ] ] ]

                            False ->
                                []
                       )
                )
            ]
        ]


inputWithLabel : String -> String -> String -> (String -> Msg) -> List (Html Msg)
inputWithLabel id_ text_ val msg =
    let
        type__ =
            if id_ == "password" then
                "password"

            else
                "input"
    in
    [ div [] [ label [ for id_ ] [ text text_ ] ]
    , div [] [ input [ type_ type__, id id_, onInput msg, value val ] [] ]
    ]
