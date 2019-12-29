module Compose exposing (Compose, Msg, update, view)

import Api
import Data.User as User exposing (User)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
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
