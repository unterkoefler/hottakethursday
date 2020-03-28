module Compose exposing (Compose, Msg, update, view)

import Api
import Colors
import Data.User as User exposing (User)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import TakeCard exposing (TakeCard, createNewTake)
import Task
import Time



-- MODEL


type alias Compose =
    String



-- UPDATE


type Msg
    = EditNewTake String
    | PublishNewTakeClick User
    | PublishNewTake User Time.Posix
    | TakePublished (Result Http.Error ())


update : Msg -> Compose -> Api.UserAuth -> ( Compose, List TakeCard, Cmd Msg )
update msg compose auth =
    case msg of
        EditNewTake newTake ->
            ( newTake, [], Cmd.none )

        PublishNewTakeClick user ->
            ( compose, [], Task.perform (PublishNewTake user) Time.now )

        PublishNewTake user time ->
            ( ""
            , List.singleton (createNewTake compose user time)
            , Api.makeTake auth compose TakePublished
            )

        TakePublished _ ->
            ( ""
            , []
            , Cmd.none
            )


view : User -> String -> Element Msg
view user newTake =
    column
        [ width fill
        , spacing 12
        ]
        [ Input.multiline
            []
            { onChange = EditNewTake
            , text = newTake
            , placeholder = Just <| Input.placeholder [] (text ("Hi " ++ user.username ++ ". What's your hottest take?"))
            , label = Input.labelHidden "What's your hottest take?"
            , spellcheck = False
            }
        , publishButton user
        ]


publishButton : User -> Element Msg
publishButton user =
    Input.button
        [ padding 12
        , Border.rounded 7
        , clip
        , Background.color Colors.primary
        , Font.color Colors.textOnPrimary
        , alignRight
        ]
        { onPress = Just <| PublishNewTakeClick user
        , label = text "Publish"
        }
