module Login exposing (LoginAttempt(..), Model, Msg, emptyForm, smallView, update, view)

import Api
import Browser.Navigation as Nav
import Colors exposing (ColorScheme)
import Data.User as User exposing (User)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
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
    | AttemptCompleted (Result Api.FormError { user : User.User, auth : Api.UserAuth })


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


view : Model -> ColorScheme -> Element Msg
view model colorScheme =
    column
        [ spacing 12
        , centerX
        , paddingEach { left = 15, right = 15, top = 64, bottom = 15 }
        ]
        [ el [ Region.heading 2, Font.size 36, Font.color colorScheme.secondary ] (text "Welcome Back")
        , row [ Font.size 14 ] [ text "I've missed you" ]
        , inputWithLabel colorScheme "Email" model.email EmailChanged
        , passwordWithLabel colorScheme "Password" model.password PasswordChanged
        , forgotPasswordLink colorScheme
        , submitButton colorScheme
        , paragraph [] [ text <| failureMessage model.previousAttempt ]
        ]


smallView : Model -> ColorScheme -> Element Msg
smallView model colorScheme =
    column
        [ spacing 12
        , centerX
        ]
        [ el [ Region.heading 2, Font.size 36, Font.color colorScheme.secondary ] (text "Welcome Back")
        , row [ Font.size 14 ] [ text "I've missed you" ]
        , inputWithLabel colorScheme "Email" model.email EmailChanged
        , passwordWithLabel colorScheme "Password" model.password PasswordChanged
        , forgotPasswordLink colorScheme
        , submitButton colorScheme
        , paragraph [] [ text <| failureMessage model.previousAttempt ]
        ]


submitButton : ColorScheme -> Element Msg
submitButton colorScheme =
    Input.button
        [ Background.color colorScheme.secondary
        , Border.rounded 7
        , padding 10
        , centerX
        , Font.size 24
        ]
        { onPress = Just Submit, label = text "Continue" }


forgotPasswordLink : ColorScheme -> Element Msg
forgotPasswordLink colorScheme =
    link
        [ Font.size 14, Font.color colorScheme.link ]
        { url = "forgot-password", label = text "Forgot password?" }


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


inputWithLabel : ColorScheme -> String -> String -> (String -> Msg) -> Element Msg
inputWithLabel colorScheme lbl val msg =
    Input.text
        (textInputAttributes
            colorScheme
        )
        { onChange = msg
        , text = val
        , placeholder = Nothing
        , label = Input.labelAbove [] (text lbl)
        }


passwordWithLabel : ColorScheme -> String -> String -> (String -> Msg) -> Element Msg
passwordWithLabel colorScheme lbl val msg =
    Input.currentPassword
        (textInputAttributes
            colorScheme
        )
        { onChange = msg
        , text = val
        , placeholder = Nothing
        , label = Input.labelAbove [] (text lbl)
        , show = False
        }


textInputAttributes : ColorScheme -> List (Attribute Msg)
textInputAttributes colorScheme =
    [ width (fill |> maximum 300)
    , Border.color colorScheme.secondary
    ]
