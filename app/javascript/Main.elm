module Main exposing (..)

import Api
import Browser
import Browser.Navigation as Nav
import Compose exposing (Compose)
import Data.User as User exposing (User)
import Flags
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Http
import Json.Decode
import Login
import Ports
import Signup
import Take exposing (Take, createNewTake, likeOrUnlike, toggleHover, viewTake)
import Task
import Thursday exposing (daysUntilThursday, isThursday, toWeekdayString)
import Time
import Url
import Url.Parser as Parser exposing ((</>), Parser, fragment, map, oneOf, parse, s, top)



-- MAIN


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
    | ForgotPasswordRoute
    | SignupRoute
    | ProfileRoute ProfileSection
    | NotFound


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map HomeRoute (Parser.fragment toHomeSection)
        , Parser.map LoginRoute (Parser.s "login")
        , Parser.map ForgotPasswordRoute (Parser.s "forgot-password")
        , Parser.map SignupRoute (Parser.s "signup")
        , Parser.map ProfileRoute (Parser.s "profile" </> Parser.fragment toProfileSection)
        ]


toHomeSection : Maybe String -> HomeSection
toHomeSection frag =
    case frag of
        Just "hottest" ->
            Hottest

        Just "coldest" ->
            Coldest

        Just str ->
            Hottest

        Nothing ->
            Hottest


toProfileSection : Maybe String -> ProfileSection
toProfileSection frag =
    case frag of
        Just "following" ->
            Following

        Just "followers" ->
            Followers

        Just "notifications" ->
            Notifications

        Just "settings" ->
            Settings

        Just str ->
            YourTakes

        Nothing ->
            YourTakes


toRoute : String -> Route
toRoute string =
    case Url.fromString string of
        Nothing ->
            NotFound

        Just url ->
            Maybe.withDefault NotFound (Parser.parse routeParser url)



-- MODEL


type alias HomeData =
    { takes : List Take
    , compose : Compose
    }


blankSignupData =
    { name = ""
    , username = ""
    , email = ""
    , birthday = ""
    }


type Page
    = Home HomeSection HomeData
    | Login Login.Model
    | ForgotPassword String
    | Signup Signup.Model
    | Profile ProfileSection User


type alias Model =
    { page : Page
    , profile : Maybe { user : User, auth : Api.UserAuth }
    , time : Time.Posix
    , zone : Time.Zone
    , url : Url.Url
    , navKey : Nav.Key
    , showNavBar : Bool
    }


george : User
george =
    { id = 3, username = "starwars4lyfe" }


take1 =
    { content = "Birds are not real"
    , postedBy = george
    , timePosted = Time.millisToPosix 10000
    , likedBy = []
    , hoveredOver = False
    }


homePage =
    Home Hottest { takes = [ take1 ], compose = "" }


homePageCold =
    Home Coldest { takes = [], compose = "" }


loginPage =
    Login { email = "", password = "", previousInvalidAttempt = False }


init : Json.Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        parsedFlags =
            Flags.parseFlags flags

        setTimeZone =
            Task.perform AdjustTimeZone Time.here

        model =
            { page = homePage
            , profile = Nothing
            , time = Time.millisToPosix 0
            , zone = Time.utc
            , url = url
            , navKey = key
            , showNavBar = False
            }

        loadAuthCmd =
            case parsedFlags.storedJWT of
                Just jwt ->
                    Api.loadUserAuth jwt StoredAuthValidated

                Nothing ->
                    Cmd.none
    in
    case toRoute <| Url.toString url of
        LoginRoute ->
            ( { model | page = loginPage }
            , Cmd.batch [ loadAuthCmd, setTimeZone ]
            )

        SignupRoute ->
            ( { model | page = Signup blankSignupData }
            , Cmd.batch [ loadAuthCmd, setTimeZone ]
            )

        _ ->
            ( model, Cmd.batch [ loadAuthCmd, setTimeZone ] )



-- UPDATE


type Msg
    = ComposeMsg Compose.Msg
    | LoginMsg Login.Msg
    | TakeMsg Take.Msg
    | SignupMsg Signup.Msg
    | AdjustTimeZone Time.Zone
    | Tick Time.Posix
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NavBarToggled
    | LogoutButtonPressed
    | LogoutRequestHandled (Result Http.Error ())
    | StoredAuthReceived Json.Decode.Value -- Got auth that was stored from a previous session.
    | StoredAuthValidated (Result Api.SavedUserAuthError Api.UserAuth)
    | StoredAuthUserReceived ( Api.UserAuth, Result Http.Error User )
    | NoOp


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

        NavBarToggled ->
            ( { model | showNavBar = not model.showNavBar }
            , Cmd.none
            )

        LogoutButtonPressed ->
            ( model
            , case model.profile of
                Just { auth } ->
                    Api.signOut auth LogoutRequestHandled

                Nothing ->
                    Cmd.none
            )

        LogoutRequestHandled (Ok _) ->
            ( { model | profile = Nothing }, Cmd.batch [ Ports.clearAuthToken (), Nav.pushUrl model.navKey "/" ] )

        StoredAuthValidated (Ok auth) ->
            ( model
            , Api.me auth (\user -> StoredAuthUserReceived ( auth, user ))
            )

        StoredAuthValidated (Err Api.TokenExpired) ->
            ( model, Ports.clearAuthToken () )

        StoredAuthUserReceived ( auth, Ok user ) ->
            ( { model | profile = Just { auth = auth, user = user } }, Cmd.none )

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

        ForgotPasswordRoute ->
            ( { model | page = ForgotPassword "yahoo@gmail.com" }, Cmd.none )

        SignupRoute ->
            ( { model | page = Signup blankSignupData }, Cmd.none )

        ProfileRoute section ->
            case model.profile of
                Just { user } ->
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

        ForgotPassword _ ->
            ( model, Cmd.none )

        Signup data ->
            updateSignupPage msg model data

        Profile _ _ ->
            ( model, Cmd.none )


updateLoginPage : Msg -> Model -> Login.Model -> ( Model, Cmd Msg )
updateLoginPage msg model data =
    case msg of
        LoginMsg lm ->
            let
                ( newData, profile, cmd ) =
                    Login.update lm data model.navKey
            in
            ( { model | page = Login newData, profile = profile }
            , Cmd.map (\m -> LoginMsg m) cmd
            )

        _ ->
            ( model, Cmd.none )


updateSignupPage : Msg -> Model -> Signup.Model -> ( Model, Cmd Msg )
updateSignupPage msg model data =
    case msg of
        SignupMsg sm ->
            ( { model | page = Signup (Signup.update sm data) }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


updateHomePage : Msg -> Model -> HomeData -> ( Model, Cmd Msg )
updateHomePage msg model data =
    case model.profile of
        Just { user } ->
            updateHomePageSignedIn msg model data user

        Nothing ->
            ( model, Cmd.none )


updateHomePageSignedIn : Msg -> Model -> HomeData -> User -> ( Model, Cmd Msg )
updateHomePageSignedIn msg model data user =
    case msg of
        ComposeMsg m ->
            handleComposeMsg m model data user

        TakeMsg m ->
            ( { model
                | page =
                    Home Hottest
                        { data | takes = Take.update m data.takes user }
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


handleComposeMsg : Compose.Msg -> Model -> HomeData -> User -> ( Model, Cmd Msg )
handleComposeMsg msg model data user =
    let
        ( newCompose, newTakes, cmd ) =
            Compose.update msg data.compose
    in
    ( { model
        | page =
            Home Hottest
                { data | compose = newCompose, takes = newTakes ++ data.takes }
      }
    , Cmd.map (\m -> ComposeMsg m) cmd
    )



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


plural : Int -> String -> String -> String
plural n sing plur =
    if n == 1 then
        sing

    else
        plur


header : Model -> Html Msg
header model =
    let
        links =
            navLinks model.page model.profile
    in
    if List.length links > 2 then
        headerWithToggle links model.showNavBar

    else
        headerWithoutToggle links


headerWithToggle : List (Html Msg) -> Bool -> Html Msg
headerWithToggle links expandToggle =
    let
        show =
            if expandToggle then
                " show"

            else
                ""
    in
    nav [ class "navbar navbar-light bg-light navbar-expand-sm" ]
        [ a [ class "navbar-brand pl-2", href "/" ] [ text "HotTakeThursday 🔥" ]
        , button
            [ class "navbar-toggler"
            , onClick NavBarToggled
            ]
            [ span [ class "navbar-toggler-icon" ] [] ]
        , div [ class <| "collapse navbar-collapse" ++ show ]
            [ ul
                [ class "navbar-nav ml-auto" ]
                links
            ]
        ]


headerWithoutToggle : List (Html Msg) -> Html Msg
headerWithoutToggle links =
    nav [ class "navbar navbar-light bg-light" ]
        [ a [ class "navbar-brand pl-2", href "/" ] [ text "HTT 🔥" ]
        , ul [ class "navbar-nav ml-auto", style "flex-direction" "row" ]
            links
        ]


navLinks : Page -> Maybe { a | user : User } -> List (Html Msg)
navLinks page profile =
    case page of
        Home _ _ ->
            case profile of
                Just { user } ->
                    [ notificationsLink
                    , navItem "Profile" "profile" ""
                    , logoutButton
                    , navItem "Delete Account" "#" ""
                    ]

                Nothing ->
                    [ navItem "Login" "login" "", navItem "Sign Up" "signup" "" ]

        Login _ ->
            [ navItem "Sign Up" "signup" "" ]

        ForgotPassword _ ->
            [ navItem "Login" "login" "", navItem "Sign Up" "signup" "" ]

        Signup _ ->
            [ navItem "Login" "login" "" ]

        Profile _ _ ->
            [ logoutButton, navItem "Delete Account" "" "" ]


logoutButton =
    li [ class "nav-item nav-link pl-3" ]
        [ button [ class "btn btn-link", onClick LogoutButtonPressed ] [ text "Logout" ] ]


notificationsLink =
    li [ class "nav-item nav-link pl-3" ]
        [ a [ href "profile#notifications" ]
            [ span [ class "d-inline d-sm-none" ]
                [ text "Notifications" ]
            , span [ class "d-none d-sm-inline" ]
                [ text "🔔" ]
            ]
        ]


navItem : String -> String -> String -> Html Msg
navItem txt link classes =
    li [ class ("nav-item nav-link pl-3 " ++ classes) ]
        [ a [ href link ] [ text txt ] ]


body : Model -> Html Msg
body model =
    case model.page of
        Home _ _ ->
            div [ class "row" ]
                [ div [ class "col-3 d-none d-md-block" ] ads
                , div [ class "col-md-6 col-xs-10" ] (content model)
                , div [ class "col-3 d-none d-md-block" ] ads
                ]

        Login data ->
            Html.map (\m -> LoginMsg m) (Login.view data)

        ForgotPassword email ->
            p []
                [ text <|
                    "We've sent an email to "
                        ++ email
                        ++ " with a link to reset "
                        ++ "your password. Remember to not "
                        ++ "forget your password again! I like "
                        ++ "to write mine on my forehead!"
                ]

        Signup data ->
            Html.map (\m -> SignupMsg m) (Signup.view data)

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
    [ ul [ class "nav nav-tabs mb-3 mt-2 mx-3" ]
        (navPills model.page)
    , div [ class "container" ]
        (case model.page of
            Home Hottest data ->
                case model.profile of
                    Just { user } ->
                        [ Html.map (\m -> ComposeMsg m)
                            (Compose.view user data.compose)
                        , feed data.takes model.zone (Just user)
                        ]

                    Nothing ->
                        [ feed data.takes model.zone Nothing ]

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
            [ navItem "Hottest" "#hottest" "active"
            , navItem "Coldest" "#coldest" ""
            ]

        Home Coldest _ ->
            [ navItem "Hottest" "#hottest" ""
            , navItem "Coldest" "#coldest" "active"
            ]

        Profile section _ ->
            [ navItem "Your Takes" "/profile" (isActive YourTakes section)
            , navItem "Following" "/profile#following" (isActive Following section)
            , navItem "Followers" "/profile#followers" (isActive Followers section)
            , navItem "Notifications" "/profile#notifications" (isActive Notifications section)
            , navItem "Settings" "/profile#settings" (isActive Settings section)
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
    , p [] [ text user.username ]
    ]


feed : List Take -> Time.Zone -> Maybe User -> Html Msg
feed takes zone user =
    div [ class "mt-3" ] (List.map (\take -> viewTakeFixMsg take zone user) takes)


viewTakeFixMsg : Take -> Time.Zone -> Maybe User -> Html Msg
viewTakeFixMsg take zone user =
    Html.map (\m -> TakeMsg m) (viewTake take zone user)
