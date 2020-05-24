module Signup exposing (Model, Msg, init, smallView, update, view)

import Api
import Birthday
import Browser.Navigation as Nav
import Colors exposing (ColorScheme)
import Data.User as User exposing (User)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Field exposing (Field, addErrorFromApi, composeValidateField, notBlank, updateValue)
import Http
import Ports



-- MODEL


type alias Model =
    { name : Field String
    , username : Field String
    , email : Field String
    , password : Field String
    , confirmPassword : Field String
    , birthday : Field String
    , agreedToTos : Field Bool
    , error : Maybe String
    }


type alias Profile =
    Maybe { user : User, auth : Api.UserAuth }


init : Model
init =
    { name = Field "" Nothing
    , username = Field "" Nothing
    , email = Field "" Nothing
    , password = Field "" Nothing
    , confirmPassword = Field "" Nothing
    , birthday = Field "" Nothing
    , agreedToTos = Field False Nothing
    , error = Nothing
    }



-- UPDATE


type Msg
    = Submit
    | EditName String
    | EditUsername String
    | EditEmail String
    | EditBirthday String
    | EditPassword String
    | EditConfirmPassword String
    | AgreedToTos Bool
    | AttemptCompleted (Result Api.FormError { user : User.User, auth : Api.UserAuth })


update : Msg -> Model -> Nav.Key -> ( Model, Profile, Cmd Msg )
update msg model navKey =
    case msg of
        Submit ->
            handleSubmit model

        EditName newName ->
            ( { model | name = updateValue newName model.name }
            , Nothing
            , Cmd.none
            )

        EditUsername newUsername ->
            ( { model | username = updateValue newUsername model.username }
            , Nothing
            , Cmd.none
            )

        EditEmail newEmail ->
            ( { model | email = updateValue newEmail model.email }
            , Nothing
            , Cmd.none
            )

        EditPassword newPassword ->
            ( { model | password = updateValue newPassword model.password }
            , Nothing
            , Cmd.none
            )

        EditConfirmPassword newConfirmPassword ->
            ( { model | confirmPassword = updateValue newConfirmPassword model.confirmPassword }
            , Nothing
            , Cmd.none
            )

        EditBirthday newBday ->
            ( { model | birthday = Birthday.handleInput model.birthday newBday }
            , Nothing
            , Cmd.none
            )

        AgreedToTos val ->
            ( { model | agreedToTos = Field val Nothing }
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

        AttemptCompleted (Err (Api.FieldError errors)) ->
            ( addErrors model errors
            , Nothing
            , Cmd.none
            )

        AttemptCompleted (Err (Api.HttpError (Http.BadStatus 401))) ->
            ( model
            , Nothing
            , Nav.pushUrl navKey "/please-confirm-email"
            )

        AttemptCompleted (Err _) ->
            ( { model | error = Just "Oops. That didn't work" }
            , Nothing
            , Cmd.none
            )


addErrors : Model -> Dict String String -> Model
addErrors model errors =
    { model
        | name = addErrorFromApi "name" model.name errors
        , username = addErrorFromApi "username" model.username errors
        , password = addErrorFromApi "password" model.password errors
        , confirmPassword = addErrorFromApi "confirmPassword" model.confirmPassword errors
        , email = addErrorFromApi "email" model.email errors
        , birthday = addErrorFromApi "birthday" model.birthday errors
        , agreedToTos = addErrorFromApi "agreedToTos" model.agreedToTos errors
    }


handleSubmit : Model -> ( Model, Profile, Cmd Msg )
handleSubmit model =
    let
        ( newModel, valid ) =
            validate model
    in
    if valid then
        ( model
        , Nothing
        , Api.signUp (data model) AttemptCompleted
        )

    else
        ( newModel, Nothing, Cmd.none )


data : Model -> Api.RegistrationInfo
data model =
    { name = model.name.value
    , password = model.password.value
    , email = model.email.value
    , username = model.username.value
    , birthday = model.birthday.value
    }


validate : Model -> ( Model, Bool )
validate model =
    let
        ( name, nameValid ) =
            validateName model.name

        ( email, emailValid ) =
            validateEmail model.email

        ( password, passwordValid ) =
            validatePassword model.password

        ( confirmPassword, confirmPasswordValid ) =
            validateConfirmPassword model.password.value model.confirmPassword

        ( username, usernameValid ) =
            validateUsername model.username

        ( birthday, birthdayValid ) =
            Birthday.validate model.birthday

        ( agreedToTos, tosValid ) =
            validateTos model.agreedToTos

        checks =
            [ nameValid, emailValid, passwordValid, confirmPasswordValid, usernameValid, birthdayValid, tosValid ]
    in
    ( { name = name
      , email = email
      , password = password
      , confirmPassword = confirmPassword
      , username = username
      , birthday = birthday
      , agreedToTos = agreedToTos
      , error = model.error
      }
    , List.foldl (&&) True checks
    )


validateTos : Field Bool -> ( Field Bool, Bool )
validateTos field =
    if field.value then
        ( field, True )

    else
        ( { field | error = Just "Our lawyers want you to check this box" }
        , False
        )


validateEmail : Field String -> ( Field String, Bool )
validateEmail =
    Field.validate (\f -> String.contains "@" f.value) "You're missing an @"


validatePassword : Field String -> ( Field String, Bool )
validatePassword =
    Field.validate (\f -> String.length f.value > 7) "Little bit longer"


validateConfirmPassword : String -> Field String -> ( Field String, Bool )
validateConfirmPassword pw =
    Field.validate (\f -> f.value == pw) "Passwords do not match"


validateName =
    notBlank


validateUsername =
    notBlank



-- VIEW


view : Model -> ColorScheme -> Element Msg
view model colorScheme =
    column
        [ centerX
        , paddingEach { left = 15, right = 15, top = 64, bottom = 15 }
        , spacing 12
        ]
    <|
        body model colorScheme


smallView : Model -> ColorScheme -> Element Msg
smallView model colorScheme =
    column
        [ centerX
        , spacing 12
        ]
    <|
        body model colorScheme


body : Model -> ColorScheme -> List (Element Msg)
body model colorScheme =
    [ paragraph [ Region.heading 2, Font.size 36, Font.color colorScheme.secondary ] [ text "Create Your Account" ]
    , paragraph [ Font.size 16 ] [ text "Feed us your data" ]
    , Field.view colorScheme (textInput "Name" EditName) model.name
    , Field.view colorScheme (textInput "Email" EditEmail) model.email
    , Field.view colorScheme (passwordInput "Password" EditPassword) model.password
    , Field.view colorScheme (passwordInput "Confirm password" EditConfirmPassword) model.confirmPassword
    , Field.view colorScheme (textInput "Username" EditUsername) model.username
    , Field.view colorScheme (textInput "Birthday (MM/DD/YYYY)" EditBirthday) model.birthday
    , Field.view colorScheme (tos colorScheme) model.agreedToTos
    , submitButton colorScheme
    , Field.viewError colorScheme model.error
    ]


tos : ColorScheme -> Bool -> Element Msg
tos colorScheme val =
    Input.checkbox
        []
        { onChange = AgreedToTos
        , icon = Input.defaultCheckbox
        , checked = val
        , label =
            Input.labelRight
                [ width fill ]
                (newTabLink []
                    { url = "https://en.wikipedia.org/wiki/Echidna#Reproduction"
                    , label =
                        paragraph [ Font.color colorScheme.link ]
                            [ text "I agree to the terms and conditions" ]
                    }
                )
        }


inputWidth =
    width (fill |> maximum 350)


textInput : String -> (String -> Msg) -> String -> Element Msg
textInput lbl msg val =
    Input.text
        [ inputWidth ]
        { onChange = msg
        , text = val
        , placeholder = Nothing
        , label = Input.labelAbove [] (text lbl)
        }


passwordInput : String -> (String -> Msg) -> String -> Element Msg
passwordInput lbl msg val =
    Input.newPassword
        [ inputWidth ]
        { onChange = msg
        , text = val
        , placeholder = Nothing
        , label = Input.labelAbove [] (text lbl)
        , show = False
        }


submitButton : ColorScheme -> Element Msg
submitButton colorScheme =
    Input.button
        [ Font.size 24
        , Border.rounded 7
        , Background.color colorScheme.secondary
        , padding 10
        , centerX
        ]
        { onPress = Just Submit
        , label = text "Begin"
        }
