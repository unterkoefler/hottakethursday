module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, disabled, height, href, placeholder, src, style, value, width)
import Html.Events exposing (onClick, onInput)
import Task
import Time



-- MAIN


debug =
    True


thursday =
    True


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- SUSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (15 * 60 * 1000) Tick



-- MODEL


type alias User =
    { name : String
    , username : String
    }


type MaybeUser
    = KnownUser User
    | Visitor


type alias Take =
    { content : String
    , postedBy : User
    , timePosted : Time.Posix
    }


type alias Model =
    { takes : List Take
    , newTake : String
    , user : MaybeUser
    , time : Time.Posix
    , zone : Time.Zone
    }


initUser =
    KnownUser { name = "George Lucas", username = "starwars4lyfe" }



-- initUser = Visitor


init : () -> ( Model, Cmd Msg )
init _ =
    ( { takes = []
      , newTake = ""
      , user = initUser
      , time = Time.millisToPosix 0
      , zone = Time.utc
      }
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = EditNewTake String
    | PublishNewTakeClick
    | PublishNewTake Time.Posix
    | AdjustTimeZone Time.Zone
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditNewTake newTake ->
            ( { model | newTake = newTake }, Cmd.none )

        PublishNewTakeClick ->
            ( model, Task.perform PublishNewTake Time.now )

        PublishNewTake time ->
            case model.user of
                KnownUser user ->
                    ( { model
                        | takes = createNewTake model.newTake user time :: model.takes
                        , newTake = ""
                      }
                    , Cmd.none
                    )

                Visitor ->
                    -- a visitor should not publish new takes
                    ( model, Cmd.none )

        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )


createNewTake : String -> User -> Time.Posix -> Take
createNewTake newTake user time =
    { content = newTake
    , postedBy = user
    , timePosted = time
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ header model.user
        , body model
        ]


header : MaybeUser -> Html Msg
header maybeUser =
    nav [ class "navbar navbar-light bg-light" ]
        [ a [ class "navbar-brand pl-2", href "#" ] [ text "HTT 🔥" ]
        , ul [ class "navbar-nav ml-auto", style "flex-direction" "row" ]
            (case maybeUser of
                KnownUser user ->
                    [ navItem "🔔" "#" ""
                    , navItem "Profile" "#" ""
                    , navItem "Logout" "#" ""
                    , navItem "Delete Account" "#" ""
                    ]

                Visitor ->
                    [ navItem "Login" "#" "", navItem "Sign Up" "#" "" ]
            )
        ]


navItem : String -> String -> String -> Html Msg
navItem txt link classes =
    li [ class ("nav-item nav-link pl-3 " ++ classes) ]
        [ a [ href link ] [ text txt ] ]


body : Model -> Html Msg
body model =
    div [ class "row" ]
        [ div [ class "col-3" ] ads
        , div [ class "col-6" ] (content model)
        , div [ class "col-3" ] ads
        ]


ads =
    [ fakeAd, fakeAd, fakeAd ]


fakeAd =
    img [ class "w-100 mb-5 mt-5 pl-5 pr-5", height 200, src "assets/trash-ad.jpg" ] []


content : Model -> List (Html Msg)
content model =
    [ ul [ class "nav nav-tabs mb-3 mt-2" ]
        [ navItem "Hottest" "#" "active"
        , navItem "Coldest" "#" ""
        ]
    , div [ class "container" ]
        (case model.user of
            KnownUser user ->
                [ compose user model.newTake
                , feed model
                ]

            Visitor ->
                [ feed model ]
        )
    ]


compose : User -> String -> Html Msg
compose user newTake =
    div []
        [ div []
            [ input
                [ placeholder ("Hi " ++ user.name ++ ". What's your hottest take?")
                , value newTake
                , onInput EditNewTake
                , class "w-100"
                ]
                []
            ]
        , div []
            [ button
                [ onClick PublishNewTakeClick
                , disabled (String.isEmpty newTake)
                ]
                [ text "Publish" ]
            ]
        ]


feed : Model -> Html Msg
feed model =
    ul [] (List.map (\take -> viewTake take model.zone) model.takes)


viewTake : Take -> Time.Zone -> Html Msg
viewTake take zone =
    li []
        [ text
            ("@"
                ++ take.postedBy.username
                ++ ": "
                ++ take.content
                ++ " ("
                ++ formatTime take.timePosted zone
                ++ ")"
            )
        ]


formatTime : Time.Posix -> Time.Zone -> String
formatTime time zone =
    let
        hour24 =
            Time.toHour zone time

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
