module Login exposing (LoginAttempt(..), Model, Msg, emptyForm, update, view)

import Api
import Browser.Navigation as Nav
import Data.User as User exposing (User)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Ports



-- MODEL


type LoginAttempt
    = NotAttempted
    | InvalidInfo -- Didn't fill out all boxes
    | IncorrectInfo -- Server doesn't like you
    | SomethingWentWrong


type alias Model =
    { email : String
    , password : String
    , previousAttempt : LoginAttempt
    }


type alias Profile =
    Maybe { user : User, auth : Api.UserAuth }


emptyForm : Model
emptyForm =
    { email = "", password = "", previousAttempt = NotAttempted }



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
                ( { model | previousAttempt = InvalidInfo }
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

        AttemptCompleted (Err (Api.HttpError (Http.BadStatus 401))) ->
            -- TODO Determine based on error whether it was actually invalid creds
            ( { model | previousAttempt = IncorrectInfo }
            , Nothing
            , Cmd.none
            )

        AttemptCompleted (Err (Api.HttpError _)) ->
            ( { model | previousAttempt = SomethingWentWrong }
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
                    ++ (case model.previousAttempt of
                            NotAttempted ->
                                []

                            InvalidInfo ->
                                [ div [] [ p [ class "text-danger" ] [ text "Invalid Username or Password" ] ] ]

                            IncorrectInfo ->
                                [ div [] [ p [ class "text-danger" ] [ text "Invalid Username or Password" ] ] ]

                            SomethingWentWrong ->
                                [ div []
                                    [ p [ class "text-danger" ]
                                        [ text "Something went wrong handling your request. Try again later." ]
                                    ]
                                ]
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
