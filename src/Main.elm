module Main exposing (..)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (css, disabled, placeholder, value)
import Html.Styled.Events exposing (onClick, onInput)
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions = subscriptions
        }



-- SUSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias User =
    { name : String
    , username : String
    }


type alias Take =
    { content : String
    , postedBy : User
    , timePosted : Time.Posix
    }


type alias Model =
    { takes : List Take
    , newTake : String
    , user : User
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] "" (User "George Lucas" "starwars4lyfe")
    , Cmd.none
    )



-- UPDATE


type Msg
    = EditNewTake String
    | PublishNewTakeClick
    | PublishNewTake Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditNewTake newTake ->
            ( { model | newTake = newTake }, Cmd.none )

        PublishNewTakeClick ->
            ( model, Task.perform PublishNewTake Time.now )

        PublishNewTake time ->
            ( { model
                | takes = createNewTake model time :: model.takes
                , newTake = ""
              }
            , Cmd.none
            )


createNewTake : Model -> Time.Posix -> Take
createNewTake model time =
    { content = model.newTake
    , postedBy = model.user
    , timePosted = time
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input
            [ Attr.placeholder ("Hi " ++ model.user.name ++ ". What's your hottest take?")
            , Attr.value model.newTake
            , onInput EditNewTake
            , Attr.css
                [ width (pct 100)
                , maxWidth (px 350)
                ]
            ]
            []
        , div []
            [ button
                [ onClick PublishNewTakeClick
                , Attr.disabled (shouldDisable model)
                ]
                [ text "Publish" ]
            ]
        , ul [] (List.map viewTake model.takes)
        ]


viewTake : Take -> Html Msg
viewTake take =
    li []
        [ text
            ("@"
                ++ take.postedBy.username
                ++ ": "
                ++ take.content
                ++ " ("
                ++ formatTime take.timePosted
                ++ ")"
            )
        ]


formatTime : Time.Posix -> String
formatTime time =
    let
        hour24 =
            Time.toHour Time.utc time

        hour =
            String.fromInt (modBy 12 hour24)

        minute =
            Time.toMinute Time.utc time

        second =
            Time.toSecond Time.utc time

        xm =
            if hour24 < 12 then
                "AM"

            else
                "PM"
    in
    String.join ":" [ hour, leftPad minute, leftPad second ] ++ " " ++ xm


leftPad : Int -> String
leftPad i =
    if i < 10 then
        "0" ++ String.fromInt i

    else
        String.fromInt i


shouldDisable : Model -> Bool
shouldDisable model =
    if String.isEmpty model.newTake then
        True

    else
        False
