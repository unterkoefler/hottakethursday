module Login exposing (LoginAttempt(..), Model, Msg, emptyForm, update, view)

import Api
import Browser.Navigation as Nav
import Data.User as User exposing (User)
import Element exposing (..)
import Element.Input as Input
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

        AttemptCompleted (Err _) ->
            ( { model | previousAttempt = SomethingWentWrong }
            , Nothing
            , Cmd.none
            )



-- VIEW


view : Model -> Element Msg
view model =
    column []
        [ inputWithLabel "Email" model.email EmailChanged
        , passwordWithLabel "Password" model.password PasswordChanged
        , link [] { url = "forgot-password", label = text "Forgot password?" }
        , Input.button [] { onPress = Just Submit, label = text "Continue" }
        , row [] [ text <| failureMessage model.previousAttempt ]
        ]


failureMessage : LoginAttempt -> String
failureMessage attempt =
    case attempt of
        NotAttempted ->
            ""

        InvalidInfo ->
            "Invalid Username or Password"

        IncorrectInfo ->
            "Invalid Username or Password"

        SomethingWentWrong ->
            "Something went wrong. Try again later"


inputWithLabel : String -> String -> (String -> Msg) -> Element Msg
inputWithLabel lbl val msg =
    Input.text
        []
        { onChange = msg
        , text = val
        , placeholder = Nothing
        , label = Input.labelAbove [] (text lbl)
        }


passwordWithLabel : String -> String -> (String -> Msg) -> Element Msg
passwordWithLabel lbl val msg =
    Input.currentPassword
        []
        { onChange = msg
        , text = val
        , placeholder = Nothing
        , label = Input.labelAbove [] (text lbl)
        , show = False
        }
