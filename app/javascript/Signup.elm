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
    { name : Field String
    , username : Field String
    , email : Field String
    , password : Field String
    , confirmPassword : Field String
    , birthday : Field String
    , agreedToTos : Field Bool
    , error : Maybe String
    }


type alias Field a =
    { value : a
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
            ( { model | birthday = handleBirthdayInput model.birthday newBday }
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


addErrorFromApi : String -> Field a -> Dict String String -> Field a
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


updateValue : a -> Field a -> Field a
updateValue val field =
    { field | value = val, error = Nothing }


handleBirthdayInput : Field String -> String -> Field String
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


notBlank : Field String -> ( Field String, Bool )
notBlank field =
    if field.value == "" then
        ( { field | error = Just "This field is required" }
        , False
        )

    else
        ( field, True )


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
    validateField (\f -> String.contains "@" f.value) "You're missing an @"


validatePassword : Field String -> ( Field String, Bool )
validatePassword =
    validateField (\f -> String.length f.value > 7) "Little bit longer"


validateConfirmPassword : String -> Field String -> ( Field String, Bool )
validateConfirmPassword pw =
    validateField (\f -> f.value == pw) "Passwords do not match"


validateField : (Field a -> Bool) -> String -> Field a -> ( Field a, Bool )
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


validateBirthday : Field String -> ( Field String, Bool )
validateBirthday =
    composeValidateField
        (composeValidateField notBlank checkLength)
        checkParts


checkLength : Field String -> ( Field String, Bool )
checkLength bday =
    case compare (String.length bday.value) 10 of
        LT ->
            ( { bday | error = Just "You're missing something" }
            , False
            )

        EQ ->
            ( bday, True )

        GT ->
            ( { bday | error = Just "Too many characters" }
            , False
            )


checkParts : Field String -> ( Field String, Bool )
checkParts bday =
    let
        parts =
            String.split "/" bday.value
    in
    case parts of
        [ month, day, year ] ->
            let
                ( err, valid ) =
                    validateParts month day year
            in
            ( { bday | error = err }, valid )

        _ ->
            ( { bday | error = Just "That's not the right format" }
            , False
            )


validateParts : String -> String -> String -> ( Maybe String, Bool )
validateParts month day year =
    let
        ( monthErr, monthValid, monthVal ) =
            validateMonth month
    in
    if monthValid then
        let
            ( dayErr, dayValid, dayVal ) =
                validateDay monthVal day
        in
        if dayValid then
            validateYear monthVal dayVal year

        else
            ( dayErr, False )

    else
        ( monthErr, False )


validateDay : Int -> String -> ( Maybe String, Bool, Int )
validateDay month day =
    let
        maxDays =
            maxDaysInMonth month
    in
    composeValidatePart
        (checkPartLength "day" 2)
        (inRange "Days should be positive" "There aren't that many days in that month" 1 maxDays)
        day


maxDaysInMonth : Int -> Int
maxDaysInMonth m =
    case m of
        1 ->
            31

        2 ->
            29

        3 ->
            31

        4 ->
            30

        5 ->
            31

        6 ->
            30

        7 ->
            31

        8 ->
            31

        9 ->
            30

        10 ->
            31

        11 ->
            30

        12 ->
            31

        _ ->
            31


validateYear : Int -> Int -> String -> ( Maybe String, Bool )
validateYear month day year =
    let
        ( err, valid, yearVal ) =
            composeValidatePart (checkPartLength "year" 4)
                (inRange "We don't like people that old" "You're too young for these takes" 1900 2004)
                year
    in
    if valid then
        case ( month, day, modBy 4 yearVal ) of
            ( 2, 29, 0 ) ->
                ( Nothing, True )

            ( 2, 29, _ ) ->
                ( Just "Oops. That wasn't a leap year", False )

            _ ->
                ( Nothing, True )

    else
        ( err, False )


validateMonth : String -> ( Maybe String, Bool, Int )
validateMonth =
    composeValidatePart
        (checkPartLength "month" 2)
        (inRange "Months should be postive" "There are only 12 months, for now..." 1 12)


checkPartLength : String -> Int -> String -> ( Maybe String, Bool, Int )
checkPartLength partName l part =
    case compare (String.length part) l of
        LT ->
            ( Just <| "Missing some characters for the " ++ partName
            , False
            , 0
            )

        GT ->
            ( Just <| "Your " ++ partName ++ " is too long"
            , False
            , 0
            )

        EQ ->
            ( Nothing, True, 0 )


inRange : String -> String -> Int -> Int -> String -> ( Maybe String, Bool, Int )
inRange tooSmallErr tooBigErr min max part =
    case String.toInt part of
        Just val ->
            case ( compare val min, compare val max ) of
                ( LT, _ ) ->
                    ( Just tooSmallErr
                    , False
                    , 0
                    )

                ( _, GT ) ->
                    ( Just tooBigErr
                    , False
                    , 0
                    )

                _ ->
                    ( Nothing, True, val )

        Nothing ->
            ( Just <| "Please use numbers", False, 0 )


composeValidatePart : (String -> ( Maybe String, Bool, Int )) -> (String -> ( Maybe String, Bool, Int )) -> String -> ( Maybe String, Bool, Int )
composeValidatePart v1 v2 part =
    let
        ( f2, valid, _ ) =
            v1 part
    in
    if valid then
        v2 part

    else
        ( f2, False, 0 )



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
        , tos model.agreedToTos
        , submitButton
        , errorMsg model.error
        ]


inputWithLabelAndError : String -> Field String -> (String -> Msg) -> Element Msg
inputWithLabelAndError lbl field msg =
    let
        input =
            inputWithLabel lbl field.value msg

        error =
            errorMsg field.error
    in
    column [ spacing 5 ]
        [ input, error ]


tos : Field Bool -> Element Msg
tos field =
    column [ spacing 5 ]
        [ Input.checkbox
            []
            { onChange = AgreedToTos
            , icon = Input.defaultCheckbox
            , checked = field.value
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
        , errorMsg field.error
        ]


errorMsg : Maybe String -> Element Msg
errorMsg error =
    case error of
        Just e ->
            el [ Font.color Colors.primary ] <| text e

        Nothing ->
            Element.none


inputWidth =
    350


inputWithLabel : String -> String -> (String -> Msg) -> Element Msg
inputWithLabel lbl val msg =
    Input.text
        [ width <| px inputWidth ]
        { onChange = msg
        , text = val
        , placeholder = Nothing
        , label = Input.labelAbove [] (text lbl)
        }


passwordWithLabelAndError : String -> Field String -> (String -> Msg) -> Element Msg
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
