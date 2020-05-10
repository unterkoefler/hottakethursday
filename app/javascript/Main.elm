module Main exposing (..)

import Api
import Browser
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav
import Colors
import Data.Take
import Data.User as User exposing (User)
import Debug
import DeleteAccount
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Feed
import Flags exposing (Dimensions)
import Html exposing (Html)
import Html.Attributes
import Http
import Json.Decode
import Login
import NavTabs exposing (navTab)
import Ports
import Profile
import Signup
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
        , onResize (\w h -> WindowResized { width = w, height = h })
        ]



-- ROUTES


type Route
    = HomeRoute Feed.FeedSection
    | LoginRoute
    | ForgotPasswordRoute
    | SignupRoute
    | ProfileRoute Profile.Section
    | DeleteAccountRoute
    | NotFound


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map HomeRoute (Parser.fragment Feed.toFeedSection)
        , Parser.map LoginRoute (Parser.s "login")
        , Parser.map ForgotPasswordRoute (Parser.s "forgot-password")
        , Parser.map SignupRoute (Parser.s "signup")
        , Parser.map ProfileRoute (Parser.s "profile" </> Parser.fragment Profile.toSection)
        , Parser.map DeleteAccountRoute (Parser.s "delete-account")
        ]


toRoute : String -> Route
toRoute string =
    case Url.fromString string of
        Nothing ->
            NotFound

        Just url ->
            Maybe.withDefault NotFound (Parser.parse routeParser url)



-- MODEL


type Page
    = Home Feed.Model
    | Login Login.Model
    | ForgotPassword
    | Signup Signup.Model
    | Profile Profile.Model
    | Loading Url.Url
    | Forbidden
    | DeleteAccount DeleteAccount.Model


type alias Model =
    { page : Page
    , profile : Maybe { user : User, auth : Api.UserAuth }
    , time : Time.Posix
    , zone : Time.Zone
    , url : Url.Url
    , navKey : Nav.Key
    , showNavBar : Bool
    , expandNavTabs : Bool
    , dimensions : Dimensions
    }


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
            { page = Home Feed.init
            , profile = Nothing
            , time = Time.millisToPosix 0
            , zone = Time.utc
            , url = url
            , navKey = key
            , showNavBar = False
            , expandNavTabs = False
            , dimensions = parsedFlags.dimensions
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
            ( { model | page = Signup Signup.init }
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

        ForgotPasswordRoute ->
            ( { model | page = ForgotPassword }
            , Cmd.batch [ loadAuthCmd, setTimeZone ]
            )

        HomeRoute section ->
            let
                initFeed =
                    Feed.init
            in
            ( { model | page = Home { initFeed | section = section } }
            , Cmd.batch [ loadAuthCmd, setTimeZone ]
            )

        DeleteAccountRoute ->
            ( { model | page = DeleteAccount DeleteAccount.init }
            , Cmd.batch [ loadAuthCmd, setTimeZone ]
            )

        NotFound ->
            ( model, Cmd.none )



-- UPDATE


type Msg
    = FeedMsg Feed.Msg
    | ProfileMsg Profile.Msg
    | LoginMsg Login.Msg
    | SignupMsg Signup.Msg
    | DeleteAccountMsg DeleteAccount.Msg
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
    | WindowResized Dimensions
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

        WindowResized dim ->
            ( { model | dimensions = dim }
            , Cmd.none
            )

        _ ->
            updatePage msg model


handleUrlChange : Model -> Url.Url -> ( Model, Cmd Msg )
handleUrlChange model url =
    case toRoute <| Url.toString url of
        HomeRoute section ->
            case model.profile of
                Just { auth } ->
                    ( { model | page = homePage model section }
                    , Api.allTakesFromToday auth FeedLoaded
                    )

                Nothing ->
                    ( { model | page = homePage model section }, Cmd.none )

        LoginRoute ->
            ( { model | page = loginPage }, Cmd.none )

        ForgotPasswordRoute ->
            ( { model | page = ForgotPassword }, Cmd.none )

        SignupRoute ->
            ( { model | page = Signup Signup.init }, Cmd.none )

        ProfileRoute section ->
            case model.profile of
                Just { user } ->
                    handleUrlChangeToProfile model section user

                Nothing ->
                    ( model, Cmd.none )

        DeleteAccountRoute ->
            ( { model | page = DeleteAccount DeleteAccount.init }, Cmd.none )

        NotFound ->
            ( model, Cmd.none )


handleUrlChangeToProfile : Model -> Profile.Section -> User -> ( Model, Cmd Msg )
handleUrlChangeToProfile model section user =
    ( { model | page = Profile <| Profile.toModel section user, expandNavTabs = False }
    , Cmd.none
    )


homePage : Model -> Feed.FeedSection -> Page
homePage model section =
    case model.page of
        Home data ->
            Home { data | section = section }

        _ ->
            let
                initFeed =
                    Feed.init
            in
            Home { initFeed | section = section }


updatePage : Msg -> Model -> ( Model, Cmd Msg )
updatePage msg model =
    case model.page of
        Home data ->
            updateHomePage msg model data

        Login data ->
            updateLoginPage msg model data

        ForgotPassword ->
            ( model, Cmd.none )

        Signup data ->
            updateSignupPage msg model data

        Profile data ->
            updateProfilePage msg model data

        Loading _ ->
            ( model, Cmd.none )

        Forbidden ->
            ( model, Cmd.none )

        DeleteAccount data ->
            updateDeleteAccountPage msg model data


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


updateDeleteAccountPage : Msg -> Model -> DeleteAccount.Model -> ( Model, Cmd Msg )
updateDeleteAccountPage msg model data =
    case msg of
        DeleteAccountMsg m ->
            let
                ( newData, cmd ) =
                    DeleteAccount.update m data model.profile
            in
            ( { model | page = DeleteAccount newData }
            , Cmd.map DeleteAccountMsg cmd
            )

        _ ->
            ( model, Cmd.none )


updateSignupPage : Msg -> Model -> Signup.Model -> ( Model, Cmd Msg )
updateSignupPage msg model data =
    case msg of
        SignupMsg sm ->
            let
                ( newData, profile, cmd ) =
                    Signup.update sm data model.navKey
            in
            ( { model | page = Signup newData, profile = profile }
            , Cmd.map SignupMsg cmd
            )

        _ ->
            ( model, Cmd.none )


updateProfilePage : Msg -> Model -> Profile.Model -> ( Model, Cmd Msg )
updateProfilePage msg model data =
    case ( msg, model.profile ) of
        ( ProfileMsg m, Just { auth } ) ->
            handleProfileMsg m model data auth

        _ ->
            ( model, Cmd.none )


updateHomePage : Msg -> Model -> Feed.Model -> ( Model, Cmd Msg )
updateHomePage msg model data =
    case model.profile of
        Just { user, auth } ->
            updateHomePageSignedIn msg model data user auth

        Nothing ->
            ( model, Cmd.none )


updateHomePageSignedIn : Msg -> Model -> Feed.Model -> User -> Api.UserAuth -> ( Model, Cmd Msg )
updateHomePageSignedIn msg model data user auth =
    case msg of
        FeedMsg m ->
            handleFeedMsg m model data user auth

        FeedLoaded (Ok takes) ->
            ( { model | page = Home <| Feed.addTakes data takes }
            , Cmd.none
            )

        FeedLoaded (Err m) ->
            let
                _ =
                    Debug.log "FeedLoaded error" m
            in
            ( model, Cmd.none )

        TakeUpdate (Err m) ->
            let
                _ =
                    Debug.log "Failed to load new take" m
            in
            ( model, Cmd.none )

        TakeUpdate (Ok take) ->
            ( { model | page = Home <| Feed.addOrUpdateTake data take }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


handleFeedMsg : Feed.Msg -> Model -> Feed.Model -> User -> Api.UserAuth -> ( Model, Cmd Msg )
handleFeedMsg msg model data user auth =
    let
        ( newFeed, cmd ) =
            Feed.update msg data user auth
    in
    ( { model | page = Home newFeed }
    , Cmd.map FeedMsg cmd
    )


handleProfileMsg : Profile.Msg -> Model -> Profile.Model -> Api.UserAuth -> ( Model, Cmd Msg )
handleProfileMsg msg model data auth =
    let
        ( newData, cmd ) =
            Profile.update msg data auth
    in
    ( { model | page = Profile newData }
    , Cmd.map ProfileMsg cmd
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    if isThursday model.time model.zone then
        { title = "HTT"
        , body = [ viewForThursday model ]
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
    column [ padding 15 ]
        [ el [ Region.heading 1 ] <| text ("Error (404): Site Unavaible on " ++ weekdayString)
        , image
            [ width (px 300), height (px 300) ]
            { src = "/assets/404.jpg"
            , description = "Image failed to load"
            }
        , paragraph []
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


viewForThursday : Model -> Html Msg
viewForThursday model =
    let
        { class, orientation } =
            classifyDevice model.dimensions
    in
    case ( class, orientation ) of
        ( Desktop, _ ) ->
            largeDeviceView model

        ( BigDesktop, _ ) ->
            largeDeviceView model

        ( Tablet, Landscape ) ->
            largeDeviceView model

        ( Tablet, Portrait ) ->
            smallDeviceView model

        ( Phone, _ ) ->
            smallDeviceView model


noFocus : FocusStyle
noFocus =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


largeDeviceView : Model -> Html Msg
largeDeviceView model =
    let
        shouldShowAds =
            showAds model

        maybeAds =
            if shouldShowAds then
                [ inFront <| ads alignLeft, inFront <| ads alignRight ]

            else
                []
    in
    layoutWith
        { options = [ focusStyle noFocus ] }
        ([ width fill ]
            ++ maybeAds
            ++ [ inFront <| largeDeviceHeader model ]
        )
    <|
        largeDeviceBody model.dimensions shouldShowAds <|
            largeDeviceContent model


largeDeviceBody : Dimensions -> Bool -> Element Msg -> Element Msg
largeDeviceBody dim shouldShowAds elmt =
    el
        [ paddingXY 12 76
        , centerX
        , width <| fullWidth dim shouldShowAds
        ]
        elmt


fullWidth : Dimensions -> Bool -> Length
fullWidth dim shouldShowAds =
    if shouldShowAds then
        fill |> (maximum <| dim.width - (2 * adsWidth))

    else
        fill


adsWidth =
    adWidth + 2 * adsPaddingX


adWidth =
    160


adsPaddingX =
    15


smallDeviceView : Model -> Html Msg
smallDeviceView model =
    layout
        []
    <|
        paragraph [] [ text "Website under construction for this screen size. Try stretching your screen a lil bit" ]


largeDeviceHeader : Model -> Element Msg
largeDeviceHeader model =
    let
        links =
            navLinks model.page model.profile
    in
    row
        [ width fill
        , padding 15
        , Background.color Colors.primary
        , Font.color Colors.textOnPrimary
        ]
        [ link [ Font.size 24 ] { url = "/", label = text "HotTakeThursday 🔥" }
        , el [ alignRight ] (row [ spacing 24 ] links)
        ]


navLinks : Page -> Maybe { a | user : User } -> List (Element Msg)
navLinks page profile =
    case page of
        Home _ ->
            case profile of
                Just { user } ->
                    [ notificationsLink
                    , navItem "Profile" "profile"
                    , navItem "Delete Account" "delete-account"
                    , logoutButton
                    ]

                Nothing ->
                    [ navItem "Login" "login", navItem "Sign Up" "signup" ]

        Login _ ->
            [ navItem "Sign Up" "signup" ]

        ForgotPassword ->
            [ navItem "Login" "login", navItem "Sign Up" "signup" ]

        Signup _ ->
            [ navItem "Login" "login" ]

        Profile _ ->
            [ logoutButton, navItem "Delete Account" "delete-account" ]

        Loading _ ->
            []

        Forbidden ->
            [ navItem "Login" "login", navItem "Sign Up" "signup" ]

        DeleteAccount _ ->
            [ logoutButton ]


showAds : Model -> Bool
showAds model =
    case model.page of
        Home _ ->
            model.dimensions.width > Feed.feedWidth + 2 * adsWidth

        _ ->
            False


logoutButton =
    Input.button
        []
        { onPress = Just LogoutButtonPressed
        , label = text "Logout"
        }


notificationsLink =
    link [] { url = "profile#notifications", label = text "Notifications" }


navItem : String -> String -> Element Msg
navItem txt link_ =
    link []
        { url = link_, label = text txt }


largeDeviceContent : Model -> Element Msg
largeDeviceContent model =
    case ( model.page, model.profile ) of
        ( Home data, Just { user } ) ->
            Element.map FeedMsg <| Feed.view data (Just user)

        ( Home data, Nothing ) ->
            Element.map FeedMsg <| Feed.view data Nothing

        ( Login data, Nothing ) ->
            Element.map LoginMsg (Login.view data)

        ( Login data, Just { user } ) ->
            alreadySignedIn user.username

        ( ForgotPassword, Nothing ) ->
            paragraph [ spacing 12, Font.size 24 ]
                [ text <|
                    "We've sent you an email"
                        ++ " with a link to reset "
                        ++ "your password. Remember to not "
                        ++ "forget your password again! I like "
                        ++ "to write mine on my forehead!"
                ]

        ( ForgotPassword, Just { user } ) ->
            alreadySignedIn user.username

        ( Signup data, Nothing ) ->
            Element.map SignupMsg (Signup.view data)

        ( Signup data, Just { user } ) ->
            alreadySignedIn user.username

        ( Profile data, Just { user } ) ->
            Element.map ProfileMsg (Profile.view data (Just user))

        ( Profile data, Nothing ) ->
            Element.map ProfileMsg (Profile.view data Nothing)

        ( Loading next, _ ) ->
            row [] [ text "Loading..." ]

        ( Forbidden, Just _ ) ->
            row []
                [ text <|
                    "Sorry! You don't have permission to view this"
                        ++ " page. If you think this is an error, please"
                        ++ " report it to noonecares@yahoo.net"
                ]

        ( Forbidden, Nothing ) ->
            row []
                [ text <|
                    "Sorry! You don't have permission to view this"
                        ++ " page. Please login or signup using the links above"
                ]

        ( DeleteAccount data, Just { user } ) ->
            Element.map DeleteAccountMsg <| DeleteAccount.view data

        ( DeleteAccount _, Nothing ) ->
            text "You're not even signed in lol"


alreadySignedIn : String -> Element Msg
alreadySignedIn username =
    paragraph [ spacing 12, Font.size 24 ]
        [ text <|
            "You're already signed in as "
                ++ username
                ++ " . Go back home."
        ]


ads : Attribute Msg -> Element Msg
ads alignment =
    column
        [ spacing 30
        , paddingXY adsPaddingX 84
        , alignment
        , alignTop
        ]
        [ fakeAd, fakeAd, fakeAd ]


fakeAd =
    image
        [ width (px adWidth)
        ]
        { src = "/assets/trash-ad.jpg"
        , description = "An advertisement for a trash can"
        }


deleteAccountView : Element Msg
deleteAccountView =
    column
        [ spacing 12
        , padding 15
        ]
        [ textColumn [ spacing 10, width <| maximum 500 fill ]
            [ paragraph []
                [ text <|
                    "You are trying to delete your account. "
                        ++ "This will cause you (and us) terrible pain, "
                        ++ "loss of advertising income, and/or happiness. "
                        ++ " This cannot be undone."
                ]
            , paragraph []
                [ text <|
                    "The button below will cause your takes, likes, "
                        ++ "bio, profile picture, and joy to be "
                        ++ "permanently deleted in an absolutely unrecoverable "
                        ++ "manner. Please keep in mind that such a deletion "
                        ++ "might be computationally expensive and could take "
                        ++ "several units of time."
                ]
            , paragraph []
                [ text <|
                    "Please also note that deleting your account has "
                        ++ "little effect on copies of your takes that other "
                        ++ "people may have screenshotted and stored. "
                        ++ "It also may cause your Thursday's to be slightly "
                        ++ "cooler than usual. Consider purchasing an extra "
                        ++ "cardigan."
                ]
            ]
        , Input.button
            [ Border.width 1
            , Border.color Colors.secondary
            , padding 10
            , Border.rounded 7
            , Font.color Colors.primary
            ]
            { onPress = Nothing
            , label = text "Goodbye :("
            }
        ]
