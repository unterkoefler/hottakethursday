module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Task
import Time
import Url
import Url.Parser as Parser exposing (Parser, map, oneOf, parse, s, top)



-- MAIN


debug =
    True



--    False


thursday =
    True


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- SUSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (15 * 60 * 1000) Tick



-- ROUTES


type Route
    = HomeRoute
    | LoginRoute
    | NotFound


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        , Parser.map LoginRoute (Parser.s "login")
        ]


toRoute : String -> Route
toRoute string =
    case Url.fromString string of
        Nothing ->
            NotFound

        Just url ->
            Maybe.withDefault NotFound (Parser.parse routeParser url)



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


type alias HomeData =
    { user : User
    , takes : List Take
    , newTake : String
    }


type alias LoginData =
    { email : String
    , password : String
    }


type Page
    = VisitorHome (List Take)
    | Home HomeData
    | Login LoginData



-- | Signup SignupData
--  | Profile ProfileData


type alias Model =
    { page : Page
    , time : Time.Posix
    , zone : Time.Zone
    , url : Url.Url
    , navKey : Nav.Key
    }


initUser =
    { name = "George Lucas", username = "starwars4lyfe" }


homePage =
    Home { takes = [], newTake = "", user = initUser }


visitorHomePage =
    VisitorHome []


loginPage =
    Login <| LoginData "" ""


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { page = visitorHomePage
            , time = Time.millisToPosix 0
            , zone = Time.utc
            , url = url
            , navKey = key
            }
    in
    case toRoute <| Url.toString url of
        LoginRoute ->
            ( { model | page = loginPage }
            , Task.perform AdjustTimeZone Time.here
            )

        _ ->
            ( model, Task.perform AdjustTimeZone Time.here )



-- UPDATE


type Msg
    = EditNewTake String
    | PublishNewTakeClick
    | PublishNewTake Time.Posix
    | AdjustTimeZone Time.Zone
    | Tick Time.Posix
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Task.perform Tick Time.now
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        _ ->
            updatePage msg model


updatePage : Msg -> Model -> ( Model, Cmd Msg )
updatePage msg model =
    case model.page of
        Home data ->
            updateHomePage msg model data

        _ ->
            ( model, Cmd.none )


updateHomePage : Msg -> Model -> HomeData -> ( Model, Cmd Msg )
updateHomePage msg model data =
    case msg of
        EditNewTake newTake ->
            ( { model | page = Home { data | newTake = newTake } }
            , Cmd.none
            )

        PublishNewTakeClick ->
            ( model, Task.perform PublishNewTake Time.now )

        PublishNewTake time ->
            let
                newTake =
                    createNewTake data.newTake data.user time

                takes =
                    newTake :: data.takes
            in
            ( { model | page = Home { data | takes = takes } }, Cmd.none )

        _ ->
            ( model, Cmd.none )


createNewTake : String -> User -> Time.Posix -> Take
createNewTake newTake user time =
    { content = newTake
    , postedBy = user
    , timePosted = time
    }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    if isThursday model.time model.zone then
        { title = "HTT"
        , body =
            [ div []
                [ header model
                , body model
                ]
            ]
        }

    else
        { title = "ERROR"
        , body = [ four04 model.time model.zone ]
        }


isThursday : Time.Posix -> Time.Zone -> Bool
isThursday time zone =
    if debug then
        thursday

    else
        Time.toWeekday zone time == Time.Thu


four04 : Time.Posix -> Time.Zone -> Html Msg
four04 time zone =
    let
        weekday =
            Time.toWeekday zone time

        weekdayString =
            toWeekdayString weekday

        daysLeft =
            daysUntilThursday weekday
    in
    div [ class "container" ]
        [ h1 [] [ text ("Error (404): Site Unavaible on " ++ weekdayString) ]
        , img [ width 300, height 300, src "assets/404.jpg" ] []
        , p []
            [ text
                ("Hi. Welcome to HotTakeThursday.com, where "
                    ++ "you can voice your hottest takes, but only on Thursdays. "
                    ++ "Today is "
                    ++ weekdayString
                    ++ ". We'll see you again in "
                    ++ String.fromInt daysLeft
                    ++ plural daysLeft " day." " days."
                )
            ]
        ]


toWeekdayString : Time.Weekday -> String
toWeekdayString weekday =
    case weekday of
        Time.Mon ->
            "Monday"

        Time.Tue ->
            "Tuesday"

        Time.Wed ->
            "Wednesday"

        Time.Thu ->
            "Thursday"

        Time.Fri ->
            "Friday"

        Time.Sat ->
            "Saturday"

        Time.Sun ->
            "Sunday"


daysUntilThursday : Time.Weekday -> Int
daysUntilThursday weekday =
    case weekday of
        Time.Mon ->
            3

        Time.Tue ->
            2

        Time.Wed ->
            1

        Time.Thu ->
            0

        Time.Fri ->
            6

        Time.Sat ->
            5

        Time.Sun ->
            4


plural : Int -> String -> String -> String
plural n sing plur =
    if n == 1 then
        sing

    else
        plur


header : Model -> Html Msg
header model =
    nav [ class "navbar navbar-light bg-light" ]
        [ a [ class "navbar-brand pl-2", href "#" ] [ text "HTT 🔥" ]
        , ul [ class "navbar-nav ml-auto", style "flex-direction" "row" ]
            (case model.page of
                Home _ ->
                    [ navItem "🔔" "#" ""
                    , navItem "Profile" "#" ""
                    , navItem "Logout" "#" ""
                    , navItem "Delete Account" "#" ""
                    ]

                VisitorHome _ ->
                    [ navItem "Login" "#" "", navItem "Sign Up" "#" "" ]

                Login _ ->
                    [ navItem "Signup" "#" "" ]
            )
        ]


navItem : String -> String -> String -> Html Msg
navItem txt link classes =
    li [ class ("nav-item nav-link pl-3 " ++ classes) ]
        [ a [ href link ] [ text txt ] ]


body : Model -> Html Msg
body model =
    case model.page of
        Home _ ->
            div [ class "row" ]
                [ div [ class "col-3" ] ads
                , div [ class "col-6" ] (content model)
                , div [ class "col-3" ] ads
                ]

        VisitorHome _ ->
            div [ class "row" ]
                [ div [ class "col-3" ] ads
                , div [ class "col-6" ] (content model)
                , div [ class "col-3" ] ads
                ]

        Login data ->
            div [ class "container" ]
                [ div [] [ label [ for "email" ] [ text "Email " ] ]
                , div [] [ input [ id "email" ] [] ]
                , div [] [ label [ for "password" ] [ text "Password " ] ]
                , div [] [ input [ type_ "password", id "password" ] [] ]
                , div [] [ button [] [ text "Continue" ] ]
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
        (case model.page of
            Home data ->
                [ compose data.user data.newTake
                , feed data.takes model.zone
                ]

            VisitorHome takes ->
                [ feed takes model.zone ]

            _ ->
                []
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


feed : List Take -> Time.Zone -> Html Msg
feed takes zone =
    ul [] (List.map (\take -> viewTake take zone) takes)


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
        weekday =
            toWeekdayString (Time.toWeekday zone time)

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
