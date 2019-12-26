module Compose exposing (Compose, Msg, update, view)

import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Take exposing (Take, createNewTake)
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


update : Msg -> Compose -> ( Compose, List Take, Cmd Msg )
update msg compose =
    case msg of
        EditNewTake newTake ->
            ( newTake, [], Cmd.none )

        PublishNewTakeClick user ->
            ( compose, [], Task.perform (PublishNewTake user) Time.now )

        PublishNewTake user time ->
            ( ""
            , List.singleton (createNewTake compose user time)
            , Cmd.none
            )


view : User -> String -> Html Msg
view user newTake =
    div
        [ style "padding-left" "15px"
        , style "padding-right" "15px"
        ]
        [ div []
            [ textarea
                [ placeholder ("Hi " ++ user.username ++ ". What's your hottest take?")
                , value newTake
                , rows 2
                , onInput EditNewTake
                , class "w-100"
                ]
                []
            ]
        , div []
            [ button
                [ onClick <| PublishNewTakeClick user
                , disabled (String.isEmpty newTake)
                ]
                [ text "Publish" ]
            ]
        ]
