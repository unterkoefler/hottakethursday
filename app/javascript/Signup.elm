module Signup exposing (Model, Msg, init, update, view)

import Api
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
import Http
import Ports



-- MODEL


type alias Model =
    { name : Field
    , username : Field
    , email : Field
    , password : Field
    , confirmPassword : Field
    , birthday : Field
    , error : Maybe String
    }


type alias Field =
    { value : String
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
            ( { model | birthday = handleBirthdayInput model.birthday newBday }
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
    }


addErrorFromApi : String -> Field -> Dict String String -> Field
addErrorFromApi key field errors =
    let
        e =
            Dict.get key errors
    in
    { field | error = e }


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


updateValue : String -> Field -> Field
updateValue val field =
    { field | value = val, error = Nothing }


handleBirthdayInput : Field -> String -> Field
handleBirthdayInput prev new =
    if String.length prev.value < String.length new then
        if String.length new == 1 then
            if new == "0" || new == "1" then
                { prev | value = new, error = Nothing }

            else
                { prev | value = "0" ++ new ++ "/", error = Nothing }

        else if String.length new == 2 then
            case String.toInt new of
                Just _ ->
                    { prev | value = new ++ "/", error = Nothing }

                Nothing ->
                    { prev | value = new, error = Nothing }

        else if String.right 2 new == "//" then
            { prev | value = String.dropRight 1 new, error = Nothing }

        else if String.length new == 5 then
            case String.toInt <| String.right 2 new of
                Just _ ->
                    { prev | value = new ++ "/", error = Nothing }

                Nothing ->
                    if
                        (String.right 1 new == "/")
                            && (String.toInt (String.slice 3 4 new) /= Nothing)
                    then
                        { prev | value = String.slice 0 3 new ++ "0" ++ String.slice 3 4 new ++ "/", error = Nothing }

                    else
                        { prev | value = new, error = Nothing }

        else
            { prev | value = new, error = Nothing }

    else
        { prev | value = new, error = Nothing }


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
            validateBirthday model.birthday

        checks =
            [ nameValid, emailValid, passwordValid, confirmPasswordValid, usernameValid, birthdayValid ]
    in
    ( { name = name
      , email = email
      , password = password
      , confirmPassword = confirmPassword
      , username = username
      , birthday = birthday
      , error = model.error
      }
    , List.foldl (&&) True checks
    )


notBlank : Field -> ( Field, Bool )
notBlank field =
    if field.value == "" then
        ( { field | error = Just "This field is required" }
        , False
        )

    else
        ( field, True )


validateEmail : Field -> ( Field, Bool )
validateEmail =
    validateField (\f -> String.contains "@" f.value) "You're missing an @"


validatePassword : Field -> ( Field, Bool )
validatePassword =
    validateField (\f -> String.length f.value > 7) "Little bit longer"


validateConfirmPassword : String -> Field -> ( Field, Bool )
validateConfirmPassword pw =
    validateField (\f -> f.value == pw) "Passwords do not match"


validateField : (Field -> Bool) -> String -> Field -> ( Field, Bool )
validateField check msg field =
    if check field then
        ( field, True )

    else
        ( { field | error = Just msg }
        , False
        )


composeValidateField v1 v2 field =
    let
        ( f2, valid ) =
            v1 field
    in
    if valid then
        v2 field

    else
        ( f2, False )


validateName =
    notBlank


validateUsername =
    notBlank


validateBirthday =
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
        , inputWithLabelAndError "Name" model.name EditName
        , inputWithLabelAndError "Email" model.email EditEmail
        , passwordWithLabelAndError "Password" model.password EditPassword
        , passwordWithLabelAndError "Confirm password" model.confirmPassword EditConfirmPassword
        , inputWithLabelAndError "Username" model.username EditUsername
        , inputWithLabelAndError "Birthday (MM/DD/YYY)" model.birthday EditBirthday
        , submitButton
        , errorMsg model.error
        ]


inputWithLabelAndError : String -> Field -> (String -> Msg) -> Element Msg
inputWithLabelAndError lbl field msg =
    let
        input =
            inputWithLabel lbl field.value msg

        error =
            errorMsg field.error
    in
    column [ spacing 5 ]
        [ input, error ]


errorMsg : Maybe String -> Element Msg
errorMsg error =
    case error of
        Just e ->
            el [ Font.color Colors.primary ] <| text e

        Nothing ->
            Element.none


inputWithLabel : String -> String -> (String -> Msg) -> Element Msg
inputWithLabel lbl val msg =
    Input.text
        [ width <| (fill |> maximum 300) ]
        { onChange = msg
        , text = val
        , placeholder = Nothing
        , label = Input.labelAbove [] (text lbl)
        }


passwordWithLabelAndError : String -> Field -> (String -> Msg) -> Element Msg
passwordWithLabelAndError lbl field msg =
    let
        pw =
            passwordWithLabel lbl field.value msg

        error =
            errorMsg field.error
    in
    column [ spacing 5 ]
        [ pw, error ]


passwordWithLabel : String -> String -> (String -> Msg) -> Element Msg
passwordWithLabel lbl val msg =
    Input.newPassword
        [ width <| (fill |> maximum 300) ]
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
