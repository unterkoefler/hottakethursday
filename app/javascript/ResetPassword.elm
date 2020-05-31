module ResetPassword exposing (Model, Msg, init, smallView, update, view)

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
    { password : Field String
    , confirmPassword : Field String
    , message : MaybeResult
    , token : Maybe String
    }


type MaybeResult
    = None
    | Error String
    | Success


init : Maybe String -> Model
init token =
    { password = Field "" Nothing
    , confirmPassword = Field "" Nothing
    , message = None
    , token = token
    }



-- UPDATE


type Msg
    = Submit
    | EditPassword String
    | EditConfirmPassword String
    | AttemptCompleted (Result Api.FormError ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            handleSubmit model

        EditPassword newPassword ->
            ( { model | password = updateValue newPassword model.password, message = None }
            , Cmd.none
            )

        EditConfirmPassword newConfirmPassword ->
            ( { model | confirmPassword = updateValue newConfirmPassword model.confirmPassword, message = None }
            , Cmd.none
            )

        AttemptCompleted (Ok _) ->
            ( { model | message = Success }
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
        | password = addErrorFromApi "password" model.password errors
    }


handleSubmit : Model -> ( Model, Cmd Msg )
handleSubmit model =
    let
        ( newModel, valid ) =
            validate model

        token =
            Maybe.withDefault "" model.token
    in
    if valid then
        ( model
        , Api.changePassword token model.password.value AttemptCompleted
        )

    else
        ( newModel, Cmd.none )


validate : Model -> ( Model, Bool )
validate model =
    let
        ( password, passwordValid ) =
            validatePassword model.password

        ( confirmPassword, confirmPasswordValid ) =
            validateConfirmPassword model.password.value model.confirmPassword

        ( message, tokenValid ) =
            validateToken model.token
    in
    ( { model
        | password = password
        , confirmPassword = confirmPassword
        , message = message
      }
    , passwordValid && confirmPasswordValid && tokenValid
    )


validatePassword : Field String -> ( Field String, Bool )
validatePassword =
    Field.validate (\f -> String.length f.value > 7) "Little bit longer"


validateConfirmPassword : String -> Field String -> ( Field String, Bool )
validateConfirmPassword pw =
    Field.validate (\f -> f.value == pw) "Passwords do not match"


validateToken : Maybe String -> ( MaybeResult, Bool )
validateToken token =
    case token of
        Nothing ->
            ( Error "This link looks broken", False )

        Just _ ->
            ( None, True )



-- VIEW


view : Model -> ColorScheme -> Element Msg
view model colorScheme =
    column
        [ centerX
        , paddingEach { left = 15, right = 15, top = 64, bottom = 15 }
        , spacing 24
        ]
        [ el [ Region.heading 2, Font.size 36, Font.color colorScheme.secondary ] (text "Change Your Password")
        , Field.view colorScheme (passwordInput "New Password" EditPassword) model.password
        , Field.view colorScheme (passwordInput "Confirm Password" EditConfirmPassword) model.confirmPassword
        , submitButton colorScheme
        , viewMessage colorScheme model.message
        ]


smallView : Model -> ColorScheme -> Element Msg
smallView model colorScheme =
    column
        [ centerX
        , spacing 24
        ]
        [ paragraph [ Region.heading 2, Font.size 36, Font.color colorScheme.secondary ] [ text "Change Your Password" ]
        , Field.view colorScheme (passwordInput "New Password" EditPassword) model.password
        , Field.view colorScheme (passwordInput "Confirm Password" EditConfirmPassword) model.confirmPassword
        , submitButton colorScheme
        , viewMessage colorScheme model.message
        ]


inputWidth =
    width (fill |> maximum 350)


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
        , label = text "Change Password"
        }


viewMessage : ColorScheme -> MaybeResult -> Element Msg
viewMessage colorScheme m =
    case m of
        None ->
            Element.none

        Error e ->
            paragraph [ Font.color colorScheme.primary ]
                [ text e ]

        Success ->
            paragraph []
                [ text "Success!"
                , link [ Font.color colorScheme.link ]
                    { url = "/login"
                    , label = text "To continue, please log in"
                    }
                ]
