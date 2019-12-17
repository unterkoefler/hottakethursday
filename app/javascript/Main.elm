module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Task
import Time
import Url
import Url.Builder exposing (absolute)
import Url.Parser as Parser exposing ((</>), Parser, map, oneOf, parse, s, top)



-- MAIN


debug =
    True


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


type HomeSection
    = Hottest
    | Coldest


type ProfileSection
    = YourTakes
    | Following
    | Followers
    | Notifications
    | Settings


type Route
    = HomeRoute HomeSection
    | LoginRoute
    | SignupRoute
    | ProfileRoute ProfileSection
    | NotFound


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map (HomeRoute Hottest) Parser.top
        , Parser.map (HomeRoute Hottest) (Parser.s "hottest")
        , Parser.map (HomeRoute Coldest) (Parser.s "coldest")
        , Parser.map LoginRoute (Parser.s "login")
        , Parser.map SignupRoute (Parser.s "signup")
        , Parser.map (ProfileRoute Following) (Parser.s "profile" </> Parser.s "following")
        , Parser.map (ProfileRoute Followers) (Parser.s "profile" </> Parser.s "followers")
        , Parser.map (ProfileRoute Notifications) (Parser.s "profile" </> Parser.s "notifications")
        , Parser.map (ProfileRoute Settings) (Parser.s "profile" </> Parser.s "settings")
        , Parser.map (ProfileRoute YourTakes) (Parser.s "profile")
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


type alias Take =
    { content : String
    , postedBy : User
    , timePosted : Time.Posix
    }


type alias HomeData =
    { takes : List Take
    , newTake : String
    }


type alias LoginData =
    { email : String
    , password : String
    }


type alias SignupData =
    { name : String
    , username : String
    }


type Page
    = Home HomeSection HomeData
    | Login LoginData
    | Signup SignupData
    | Profile ProfileSection User


type alias Model =
    { page : Page
    , user : Maybe User
    , time : Time.Posix
    , zone : Time.Zone
    , url : Url.Url
    , navKey : Nav.Key
    }


george =
    { name = "George Lucas", username = "starwars4lyfe" }


homePage =
    Home Hottest { takes = [], newTake = "" }


homePageCold =
    Home Coldest { takes = [], newTake = "" }


loginPage =
    Login <| LoginData "" ""


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        setTimeZone =
            Task.perform AdjustTimeZone Time.here

        model =
            { page = homePage
            , user = Nothing
            , time = Time.millisToPosix 0
            , zone = Time.utc
            , url = url
            , navKey = key
            }
    in
    case toRoute <| Url.toString url of
        LoginRoute ->
            ( { model | page = loginPage }
            , setTimeZone
            )

        SignupRoute ->
            ( { model | page = Signup <| SignupData "" "" }
            , setTimeZone
            )

        _ ->
            ( model, setTimeZone )



-- UPDATE


type Msg
    = EditNewTake String
    | PublishNewTakeClick
    | PublishNewTake Time.Posix
    | AdjustTimeZone Time.Zone
    | Tick Time.Posix
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | LoginButtonPressed
    | LogoutButtonPressed
    | SignupButtonPressed
    | SignupEditName String
    | SignupEditUsername String


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
            handleUrlChange model url

        LogoutButtonPressed ->
            ( { model | user = Nothing }
            , Nav.pushUrl model.navKey "/"
            )

        _ ->
            updatePage msg model


handleUrlChange : Model -> Url.Url -> ( Model, Cmd Msg )
handleUrlChange model url =
    case toRoute <| Url.toString url of
        HomeRoute Hottest ->
            ( { model | page = homePage }, Cmd.none )

        HomeRoute Coldest ->
            ( { model | page = homePageCold }, Cmd.none )

        LoginRoute ->
            ( { model | page = loginPage }, Cmd.none )

        SignupRoute ->
            ( { model | page = Signup <| SignupData "" "" }, Cmd.none )

        ProfileRoute section ->
            case model.user of
                Just user ->
                    handleUrlChangeToProfile model section user

                Nothing ->
                    ( model, Cmd.none )

        NotFound ->
            ( model, Cmd.none )


handleUrlChangeToProfile : Model -> ProfileSection -> User -> ( Model, Cmd Msg )
handleUrlChangeToProfile model section user =
    ( { model | page = Profile section user }, Cmd.none )


updatePage : Msg -> Model -> ( Model, Cmd Msg )
updatePage msg model =
    case model.page of
        Home _ data ->
            updateHomePage msg model data

        Login data ->
            updateLoginPage msg model data

        Signup data ->
            updateSignupPage msg model data

        Profile _ _ ->
            ( model, Cmd.none )


updateSignupPage : Msg -> Model -> SignupData -> ( Model, Cmd Msg )
updateSignupPage msg model data =
    case msg of
        SignupButtonPressed ->
            if validateSignup data then
                ( { model | user = Just data }
                , Nav.pushUrl model.navKey "/"
                )

            else
                ( model, Cmd.none )

        SignupEditName newName ->
            ( { model | page = Signup { data | name = newName } }
            , Cmd.none
            )

        SignupEditUsername newUsername ->
            ( { model | page = Signup { data | username = newUsername } }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


validateSignup : SignupData -> Bool
validateSignup data =
    not (String.isEmpty data.name) && not (String.isEmpty data.username)


updateLoginPage : Msg -> Model -> LoginData -> ( Model, Cmd Msg )
updateLoginPage msg model data =
    case msg of
        LoginButtonPressed ->
            if validateLogin data then
                ( { model | user = Just george }
                , Nav.pushUrl model.navKey "/"
                )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


validateLogin : LoginData -> Bool
validateLogin _ =
    True


updateHomePage : Msg -> Model -> HomeData -> ( Model, Cmd Msg )
updateHomePage msg model data =
    case model.user of
        Just user ->
            updateHomePageSignedIn msg model data user

        Nothing ->
            ( model, Cmd.none )


updateHomePageSignedIn : Msg -> Model -> HomeData -> User -> ( Model, Cmd Msg )
updateHomePageSignedIn msg model data user =
    case msg of
        EditNewTake newTake ->
            ( { model | page = Home Hottest { data | newTake = newTake } }
            , Cmd.none
            )

        PublishNewTakeClick ->
            ( model, Task.perform PublishNewTake Time.now )

        PublishNewTake time ->
            let
                newTake =
                    createNewTake data.newTake user time

                takes =
                    newTake :: data.takes
            in
            ( { model | page = Home Hottest { data | takes = takes, newTake = "" } }, Cmd.none )

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
        , img [ width 300, height 300, src "/assets/404.jpg" ] []
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
        [ a [ class "navbar-brand pl-2", href "/" ] [ text "HTT 🔥" ]
        , ul [ class "navbar-nav ml-auto", style "flex-direction" "row" ]
            (case model.page of
                Home _ _ ->
                    case model.user of
                        Just user ->
                            [ navItem "🔔" "#" ""
                            , navItem "Profile" "profile" ""
                            , logoutButton
                            , navItem "Delete Account" "#" ""
                            ]

                        Nothing ->
                            [ navItem "Login" "login" "", navItem "Sign Up" "signup" "" ]

                Login _ ->
                    [ navItem "Sign Up" "signup" "" ]

                Signup _ ->
                    [ navItem "Login" "login" "" ]

                Profile _ _ ->
                    [ logoutButton, navItem "Delete Account" "" "" ]
            )
        ]


logoutButton =
    li [ class "nav-item nav-link pl-3" ]
        [ button [ class "btn btn-link", onClick LogoutButtonPressed ] [ text "Logout" ] ]


navItem : String -> String -> String -> Html Msg
navItem txt link classes =
    li [ class ("nav-item nav-link pl-3 " ++ classes) ]
        [ a [ href link ] [ text txt ] ]


body : Model -> Html Msg
body model =
    case model.page of
        Home _ _ ->
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
                , div [] [ button [ onClick LoginButtonPressed ] [ text "Continue" ] ]
                ]

        Signup data ->
            div [ class "container" ]
                [ h2 [] [ text "Create Account" ]
                , p [] [ text "Feed us your data" ]
                , div [] [ label [ for "name" ] [ text "Name" ] ]
                , div [] [ input [ id "name", onInput SignupEditName ] [] ]
                , div [] [ label [ for "username" ] [ text "Username" ] ]
                , div [] [ input [ id "username", onInput SignupEditUsername ] [] ]
                , div []
                    [ button
                        [ onClick SignupButtonPressed
                        , disabled <| not <| validateSignup data
                        ]
                        [ text "Begin" ]
                    ]
                ]

        Profile _ user ->
            div [ class "row" ]
                [ div [ class "col-3" ] (aboutUser user)
                , div [ class "col-9" ] (content model)
                ]


ads =
    [ fakeAd, fakeAd, fakeAd ]


fakeAd =
    img [ class "w-100 mb-5 mt-5 pl-5 pr-5", height 200, src "/assets/trash-ad.jpg" ] []


content : Model -> List (Html Msg)
content model =
    [ ul [ class "nav nav-tabs mb-3 mt-2" ]
        (navPills model.page)
    , div [ class "container" ]
        (case model.page of
            Home Hottest data ->
                case model.user of
                    Just user ->
                        [ compose user data.newTake
                        , feed data.takes model.zone
                        ]

                    Nothing ->
                        [ feed data.takes model.zone ]

            Home Coldest data ->
                [ p [] [ text "Sorry, we don't have any cold takes here" ] ]

            _ ->
                []
        )
    ]


navPills : Page -> List (Html Msg)
navPills page =
    case page of
        Home Hottest _ ->
            [ navItem "Hottest" "hottest" "active"
            , navItem "Coldest" "coldest" ""
            ]

        Home Coldest _ ->
            [ navItem "Hottest" "hottest" ""
            , navItem "Coldest" "coldest" "active"
            ]

        Profile section _ ->
            [ navItem "Your Takes" "/profile" (isActive YourTakes section)
            , navItem "Following" "/profile/following" (isActive Following section)
            , navItem "Followers" "/profile/followers" (isActive Followers section)
            , navItem "Notifications" "/profile/notifications" (isActive Notifications section)
            , navItem "Settings" "/profile/settings" (isActive Settings section)
            ]

        _ ->
            []


isActive : ProfileSection -> ProfileSection -> String
isActive thisSection currentSection =
    if thisSection == currentSection then
        "active"

    else
        ""


aboutUser : User -> List (Html Msg)
aboutUser user =
    [ h3 [] [ text <| "@" ++ user.username ]
    , img [ src "/assets/profilepic.jpg", width 100 ] []
    , p [] [ text user.name ]
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

        hourTmp =
            String.fromInt (modBy 12 hour24)

        hour =
            if hourTmp == "0" then
                "12"

            else
                hourTmp

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
