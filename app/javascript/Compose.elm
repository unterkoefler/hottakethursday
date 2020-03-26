module Compose exposing (Compose, Msg, update, view)

import Api
import Data.User as User exposing (User)
import Element exposing (..)
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
        []
        [ Input.multiline
            []
            { onChange = EditNewTake
            , text = newTake
            , placeholder = Just <| Input.placeholder [] (text ("Hi " ++ user.username ++ ". What's your hottest take?"))
            , label = Input.labelHidden "What's your hottest take?"
            , spellcheck = False
            }
        , Input.button
            []
            { onPress = Just <| PublishNewTakeClick user
            , label = text "Publish"
            }
        ]
