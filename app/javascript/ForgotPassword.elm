module ForgotPassword exposing (Model, Msg, init, update, view)

import Api
import Colors exposing (ColorScheme)
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
    { email : Field String
    , message : MaybeResult
    }


type MaybeResult
    = None
    | Error String
    | Success String


init : String -> Model
init email =
    { email = Field email Nothing
    , message = None
    }



-- UPDATE


type Msg
    = Submit
    | EditEmail String
    | AttemptCompleted (Result Api.FormError ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            handleSubmit model

        EditEmail newEmail ->
            ( { model | email = updateValue newEmail model.email, message = None }
            , Cmd.none
            )

        AttemptCompleted (Ok _) ->
            ( { model | message = Success "Sent! Go check your email." }
            , Cmd.none
            )

        AttemptCompleted (Err (Api.FieldError errors)) ->
            ( addErrors model errors
            , Cmd.none
            )

        AttemptCompleted (Err _) ->
            ( { model | message = Error "Oops. That didn't work." }
            , Cmd.none
            )


addErrors : Model -> Dict String String -> Model
addErrors model errors =
    { model
        | email = addErrorFromApi "email" model.email errors
    }


handleSubmit : Model -> ( Model, Cmd Msg )
handleSubmit model =
    let
        ( newModel, valid ) =
            validate model
    in
    if valid then
        ( model
        , Api.sendForgotPasswordEmail model.email.value AttemptCompleted
        )

    else
        ( newModel, Cmd.none )


validate : Model -> ( Model, Bool )
validate model =
    let
        ( email, emailValid ) =
            validateEmail model.email
    in
    ( { model | email = email }
    , emailValid
    )


validateEmail : Field String -> ( Field String, Bool )
validateEmail =
    Field.validate (\f -> String.contains "@" f.value) "You're missing an @"



-- VIEW


view : Model -> ColorScheme -> Element Msg
view model colorScheme =
    column
        [ centerX
        , paddingEach { left = 15, right = 15, top = 64, bottom = 15 }
        , spacing 24
        ]
        [ el [ Region.heading 2, Font.size 36, Font.color colorScheme.secondary ] (text "Reset Password")
        , Field.view colorScheme (textInput "Enter your email to get the special link" EditEmail) model.email
        , submitButton colorScheme
        , viewMessage colorScheme model.message
        ]


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
        , label = text "Send Email"
        }


viewMessage : ColorScheme -> MaybeResult -> Element Msg
viewMessage colorScheme m =
    case m of
        None ->
            Element.none

        Error e ->
            paragraph [ Font.color colorScheme.primary ]
                [ text e ]

        Success s ->
            paragraph []
                [ text s ]
