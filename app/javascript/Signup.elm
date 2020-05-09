module Signup exposing (Model, Msg, init, update, view)

import Api
import Birthday
import Browser.Navigation as Nav
import Colors
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
            let
                _ =
                    Debug.log "Error" e
            in
            ( addErrors model errors
            , Nothing
            , Cmd.none
            )

        AttemptCompleted (Err _) ->
            let
                _ =
                    Debug.log "Error" e
            in
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


view : Model -> Element Msg
view model =
    column
        [ centerX
        , paddingEach { left = 15, right = 15, top = 64, bottom = 15 }
        , spacing 12
        ]
        [ el [ Region.heading 2, Font.size 36, Font.color Colors.secondary ] (text "Create Your Account")
        , paragraph [ Font.size 16 ] [ text "Feed us your data" ]
        , Field.view (textInput "Name" EditName) model.name
        , Field.view (textInput "Email" EditEmail) model.email
        , Field.view (passwordInput "Password" EditPassword) model.password
        , Field.view (passwordInput "Confirm password" EditConfirmPassword) model.confirmPassword
        , Field.view (textInput "Username" EditUsername) model.username
        , Field.view (textInput "Birthday" EditBirthday) model.birthday
        , Field.view tos model.agreedToTos
        , submitButton
        , Field.viewError model.error
        ]


tos : Bool -> Element Msg
tos val =
    Input.checkbox
        []
        { onChange = AgreedToTos
        , icon = Input.defaultCheckbox
        , checked = val
        , label =
            Input.labelRight
                []
                (link []
                    { url = "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
                    , label =
                        el [ Font.color Colors.link ] (text "I agree to the terms and conditions")
                    }
                )
        }


inputWidth =
    350


textInput : String -> (String -> Msg) -> String -> Element Msg
textInput lbl msg val =
    Input.text
        [ width <| px inputWidth ]
        { onChange = msg
        , text = val
        , placeholder = Nothing
        , label = Input.labelAbove [] (text lbl)
        }


passwordInput : String -> (String -> Msg) -> String -> Element Msg
passwordInput lbl msg val =
    Input.newPassword
        [ width <| px inputWidth ]
        { onChange = msg
        , text = val
        , placeholder = Nothing
        , label = Input.labelAbove [] (text lbl)
        , show = False
        }


submitButton : Element Msg
submitButton =
    Input.button
        [ Font.size 24
        , Border.rounded 7
        , Background.color Colors.secondary
        , padding 10
        , centerX
        ]
        { onPress = Just Submit
        , label = text "Begin"
        }
