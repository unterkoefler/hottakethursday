module Main exposing (..)

import Api
import Browser
import Browser.Navigation as Nav
import Compose exposing (Compose)
import Data.Take
import Data.User as User exposing (User)
import Debug
import Element exposing (..)
import Element.Input as Input
import Element.Region as Region
import Flags
import Html exposing (Html)
import Http
import Json.Decode
import Login
import Ports
import Signup
import TakeCard exposing (TakeCard, createNewTake, likeOrUnlike, toggleHover, viewTake)
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
    Sub.batch
        [ Time.every (15 * 60 * 1000) Tick
        , case ( model.profile, model.page ) of
            ( Just _, Home _ ) ->
                Ports.newTakeInfo
                    (TakeUpdate
                        << Json.Decode.decodeValue Data.Take.decoder
                    )

            _ ->
                Sub.none
        ]



-- ROUTES


type ProfileSection
    = YourTakes
    | Notifications
    | Settings


type Route
    = HomeRoute
    | LoginRoute
    | ForgotPasswordRoute
    | SignupRoute
    | ProfileRoute ProfileSection
    | NotFound


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map HomeRoute (Parser.s "hottest")
        , Parser.map HomeRoute top
        , Parser.map LoginRoute (Parser.s "login")
        , Parser.map ForgotPasswordRoute (Parser.s "forgot-password")
        , Parser.map SignupRoute (Parser.s "signup")
        , Parser.map ProfileRoute (Parser.s "profile" </> Parser.fragment toProfileSection)
        ]


toProfileSection : Maybe String -> ProfileSection
toProfileSection frag =
    case frag of
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
    { takes : List TakeCard
    , compose : Compose
    }


blankSignupData =
    { name = ""
    , username = ""
    , email = ""
    , birthday = ""
    }


type Page
    = Home HomeData
    | Login Login.Model
    | ForgotPassword String
    | Signup Signup.Model
    | Profile ProfileSection User
    | Loading Url.Url
    | Forbidden


type alias Model =
    { page : Page
    , profile : Maybe { user : User, auth : Api.UserAuth }
    , time : Time.Posix
    , zone : Time.Zone
    , url : Url.Url
    , navKey : Nav.Key
    , showNavBar : Bool
    , expandNavTabs : Bool
    }


homePage =
    Home { takes = [], compose = "" }


loginPage =
    Login Login.emptyForm


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
            , expandNavTabs = False
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

        ProfileRoute section ->
            case parsedFlags.storedJWT of
                Just _ ->
                    ( { model | page = Loading url }
                    , Cmd.batch [ loadAuthCmd, setTimeZone ]
                    )

                Nothing ->
                    ( { model | page = Forbidden }
                    , setTimeZone
                    )

        _ ->
            ( model, Cmd.batch [ loadAuthCmd, setTimeZone ] )



-- UPDATE


type Msg
    = ComposeMsg Compose.Msg
    | LoginMsg Login.Msg
    | TakeMsg TakeCard.Msg
    | SignupMsg Signup.Msg
    | FeedLoaded (Result Http.Error (List Data.Take.Take))
    | AdjustTimeZone Time.Zone
    | Tick Time.Posix
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NavBarToggled
    | NavTabsToggled
    | LogoutButtonPressed
    | LogoutRequestHandled (Result Http.Error ())
    | StoredAuthReceived Json.Decode.Value -- Got auth that was stored from a previous session.
    | StoredAuthValidated (Result Api.SavedUserAuthError Api.UserAuth)
    | StoredAuthUserReceived ( Api.UserAuth, Result Http.Error User )
    | TakeUpdate (Result Json.Decode.Error Data.Take.Take)
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

        NavTabsToggled ->
            ( { model | expandNavTabs = not model.expandNavTabs }
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
            let
                cmd =
                    case model.page of
                        Loading next ->
                            Nav.pushUrl model.navKey (Url.toString next)

                        Home _ ->
                            Api.allTakesFromToday auth FeedLoaded

                        _ ->
                            Cmd.none
            in
            ( { model | profile = Just { auth = auth, user = user } }
            , cmd
            )

        _ ->
            updatePage msg model


handleUrlChange : Model -> Url.Url -> ( Model, Cmd Msg )
handleUrlChange model url =
    case toRoute <| Url.toString url of
        HomeRoute ->
            case model.profile of
                Just { auth } ->
                    ( { model | page = homePage }, Api.allTakes auth FeedLoaded )

                Nothing ->
                    ( { model | page = homePage }, Cmd.none )

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
    ( { model | page = Profile section user, expandNavTabs = False }
    , Cmd.none
    )


updatePage : Msg -> Model -> ( Model, Cmd Msg )
updatePage msg model =
    case model.page of
        Home data ->
            updateHomePage msg model data

        Login data ->
            updateLoginPage msg model data

        ForgotPassword _ ->
            ( model, Cmd.none )

        Signup data ->
            updateSignupPage msg model data

        Profile _ _ ->
            ( model, Cmd.none )

        Loading _ ->
            ( model, Cmd.none )

        Forbidden ->
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
            , Cmd.map LoginMsg cmd
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
        Just { user, auth } ->
            updateHomePageSignedIn msg model data user auth

        Nothing ->
            ( model, Cmd.none )


updateHomePageSignedIn : Msg -> Model -> HomeData -> User -> Api.UserAuth -> ( Model, Cmd Msg )
updateHomePageSignedIn msg model data user auth =
    case msg of
        ComposeMsg m ->
            handleComposeMsg m model data user auth

        TakeMsg m ->
            handleTakeMsg m model data user auth

        FeedLoaded (Ok takes) ->
            ( { model
                | page =
                    Home { data | takes = List.map (\t -> { take = t, hovered = False }) takes }
              }
            , Cmd.none
            )

        FeedLoaded (Err m) ->
            let
                _ =
                    Debug.log "FeedLoaded error" m
            in
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleTakeMsg : TakeCard.Msg -> Model -> HomeData -> User -> Api.UserAuth -> ( Model, Cmd Msg )
handleTakeMsg msg model data user auth =
    let
        ( newTakes, cmd ) =
            TakeCard.update msg data.takes user auth
    in
    ( { model | page = Home { data | takes = newTakes } }
    , Cmd.map TakeMsg cmd
    )


handleComposeMsg : Compose.Msg -> Model -> HomeData -> User -> Api.UserAuth -> ( Model, Cmd Msg )
handleComposeMsg msg model data user auth =
    let
        ( newCompose, newTakes, cmd ) =
            Compose.update msg data.compose auth
    in
    ( { model
        | page =
            Home
                { data | compose = newCompose, takes = newTakes ++ data.takes }
      }
    , Cmd.map ComposeMsg cmd
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    if isThursday model.time model.zone then
        { title = "HTT"
        , body =
            [ layout
                []
              <|
                column []
                    [ header model
                    , body model
                    ]
            ]
        }

    else
        { title = "ERROR"
        , body = [ Element.layout [] (four04 model.time model.zone) ]
        }


four04 : Time.Posix -> Time.Zone -> Element Msg
four04 time zone =
    let
        weekday =
            Time.toWeekday zone time

        weekdayString =
            toWeekdayString weekday

        daysLeft =
            daysUntilThursday weekday
    in
    column []
        [ el [ Region.heading 1 ] <| text ("Error (404): Site Unavaible on " ++ weekdayString)
        , image
            [ width (px 300), height (px 300) ]
            { src = "/assets/404.jpg"
            , description = "Image failed to load"
            }
        , row []
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


header : Model -> Element Msg
header model =
    let
        links =
            navLinks model.page model.profile
    in
    if List.length links > 2 then
        headerWithToggle links model.showNavBar

    else
        headerWithoutToggle links


headerWithToggle : List (Element Msg) -> Bool -> Element Msg
headerWithToggle links expandToggle =
    let
        show =
            if expandToggle then
                " show"

            else
                ""
    in
    row []
        [ link [] { url = "/", label = text "HotTakeThursday 🔥" }
        , Input.button
            []
            { onPress = Just NavBarToggled
            , label = text "\\/"
            }
        , column []
            [ row
                []
                links
            ]
        ]


headerWithoutToggle : List (Element Msg) -> Element Msg
headerWithoutToggle links =
    row []
        [ link [] { url = "/", label = text "HTT 🔥" }
        , row []
            links
        ]


navLinks : Page -> Maybe { a | user : User } -> List (Element Msg)
navLinks page profile =
    case page of
        Home _ ->
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

        Loading _ ->
            []

        Forbidden ->
            [ navItem "Login" "login" "", navItem "Sign Up" "signup" "" ]


logoutButton =
    Input.button
        []
        { onPress = Just LogoutButtonPressed
        , label = text "Logout"
        }


notificationsLink =
    link [] { url = "profile#notifications", label = text "Notifications 🔔" }


navItem : String -> String -> String -> Element Msg
navItem txt link_ classes =
    link []
        { url = link_, label = text txt }


body : Model -> Element Msg
body model =
    case model.page of
        Home _ ->
            column []
                [ column [] ads
                , column [] (content model)
                , column [] ads
                ]

        Login data ->
            Element.map LoginMsg (Login.view data)

        ForgotPassword email ->
            row []
                [ text <|
                    "We've sent an email to "
                        ++ email
                        ++ " with a link to reset "
                        ++ "your password. Remember to not "
                        ++ "forget your password again! I like "
                        ++ "to write mine on my forehead!"
                ]

        Signup data ->
            Element.map SignupMsg (Signup.view data)

        Profile _ user ->
            let
                ownProfile =
                    case model.profile of
                        Just profile ->
                            user == profile.user

                        Nothing ->
                            False
            in
            column []
                [ column []
                    (aboutUser user userDetailEx1 ownProfile)
                , column [] (content model)
                ]

        Loading next ->
            row [] [ text "Loading..." ]

        Forbidden ->
            case model.profile of
                Just _ ->
                    row []
                        [ text <|
                            "Sorry! You don't have permission to view this"
                                ++ " page. If you think this is an error, please"
                                ++ " report it to noonecares@yahoo.net"
                        ]

                Nothing ->
                    row []
                        [ text <|
                            "Sorry! You don't have permission to view this"
                                ++ " page. Please login or signup using the links above"
                        ]


ads =
    [ fakeAd, fakeAd, fakeAd ]


fakeAd =
    image
        [ width (px 160)
        ]
        { src = "/assets/trash-ad.jpg"
        , description = "An advertisement for a trash can"
        }


content : Model -> List (Element Msg)
content model =
    [ navTabs model.page model.expandNavTabs
    , column []
        (case model.page of
            Home data ->
                case model.profile of
                    Just { user } ->
                        [ Element.map ComposeMsg
                            (Compose.view user data.compose)
                        , feed data.takes model.zone (Just user)
                        ]

                    Nothing ->
                        [ feed data.takes model.zone Nothing ]

            _ ->
                []
        )
    ]


navTabs : Page -> Bool -> Element Msg
navTabs page expandNavTabs =
    case page of
        Home _ ->
            row []
                [ navItem "Hottest" "#hottest" "active"
                ]

        Profile section _ ->
            navTabsCollapsable section expandNavTabs

        _ ->
            column [] []


navTabsCollapsable : ProfileSection -> Bool -> Element Msg
navTabsCollapsable section expand =
    let
        ( tabsClass, icon ) =
            if expand then
                ( "expanded-tabs", "▲" )

            else
                ( "collapsed-tabs", "▼" )

        navItems =
            [ navItem "Your Takes" "/profile" (isActive YourTakes section)
            , navItem "Notifications" "/profile#notifications" (isActive Notifications section)
            , navItem "Settings" "/profile#settings" (isActive Settings section)
            ]
    in
    column []
        [ row []
            navItems
        , column []
            (navItems
                ++ [ Input.button
                        []
                        { onPress = Just NavTabsToggled
                        , label = text icon
                        }
                   ]
            )
        ]


isActive : ProfileSection -> ProfileSection -> String
isActive thisSection currentSection =
    if thisSection == currentSection then
        "active"

    else
        ""


type Gender
    = Neutral
    | Feminine
    | Masculine


type alias UserDetail =
    { fullName : String
    , bio : String
    , pronouns : Gender
    , birthday : String -- TODO : find a reasonable type for dates
    , leastFavoriteColor : String
    , userId : Int
    }


userDetailEx1 =
    { fullName = "George Lopez"
    , bio = "Is any of this real?"
    , pronouns = Masculine
    , birthday = "May 17th"
    , leastFavoriteColor = "Olive green"
    , userId = 1
    }


aboutUser : User -> UserDetail -> Bool -> List (Element Msg)
aboutUser user detail editable =
    [ column []
        [ column []
            [ column []
                [ column []
                    [ el [ Region.heading 5 ] (text <| "@" ++ user.username) ]
                , column []
                    [ profilePicture user ]
                , aboutUserElem detail.fullName "" editable
                ]
            , column []
                (column []
                    -- hacky css
                    [ el [ Region.heading 5 ] (text <| "@" ++ user.username) ]
                    :: List.map (\( a, b ) -> aboutUserElem a b editable)
                        [ ( detail.bio, "Bio" )
                        , ( detail.birthday, "Birthday" )
                        , ( detail.leastFavoriteColor, "Least favorite color" )
                        ]
                )
            ]
        ]
    ]


profilePicture : User -> Element Msg
profilePicture user =
    let
        src_ =
            Maybe.withDefault "/assets/profilepic.jpg" user.avatarUrl
    in
    column
        []
        [ column []
            [ image
                []
                { src = src_
                , description = "Profile picture"
                }
            ]
        ]


aboutUserElem : String -> String -> Bool -> Element Msg
aboutUserElem info label editable =
    column []
        [ column []
            [ column []
                ([ text <| label ++ " "
                 , row [] [ text info ]
                 ]
                    ++ (if editable then
                            [ aboutEditButton ]

                        else
                            []
                       )
                )
            ]
        ]


aboutEditButton : Element Msg
aboutEditButton =
    row []
        [ Input.button [] { onPress = Nothing, label = text "edit" } ]


feed : List TakeCard -> Time.Zone -> Maybe User -> Element Msg
feed takes zone user =
    column [] (List.map (\take -> viewTakeFixMsg take zone user) takes)


viewTakeFixMsg : TakeCard -> Time.Zone -> Maybe User -> Element Msg
viewTakeFixMsg take zone user =
    Element.map TakeMsg (viewTake take zone user)
