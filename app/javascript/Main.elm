module Main exposing (..)

import Api
import Browser
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav
import Colors exposing (ColorScheme, colorSchemeForUser)
import Data.Take
import Data.User as User exposing (User)
import DeleteAccount
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Feed
import Flags exposing (Dimensions)
import ForgotPassword
import Html exposing (Html)
import Html.Attributes
import Http
import HttpUtils exposing (httpErrorToString)
import Json.Decode
import Login
import NavTabs exposing (navTab)
import Ports
import Profile
import ResetPassword
import Signup
import Task
import Thursday exposing (daysUntilThursday, isThursday, toWeekdayString)
import Time
import Url
import Url.Parser as Parser exposing ((</>), (<?>), Parser, fragment, map, oneOf, parse, s, top)
import Url.Parser.Query as Query



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
        [ Time.every (60 * 1000) Tick
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
    | ResetPasswordRoute (Maybe String)
    | SignupRoute
    | ProfileRoute (Maybe Int) Profile.Section
    | DeleteAccountRoute
    | PleaseConfirmEmailRoute
    | NotFound


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map HomeRoute (Parser.fragment Feed.toFeedSection)
        , Parser.map LoginRoute (Parser.s "login")
        , Parser.map ForgotPasswordRoute (Parser.s "forgot-password")
        , Parser.map SignupRoute (Parser.s "signup")
        , Parser.map ProfileRoute (Parser.s "profile" <?> Query.int "uid" </> Parser.fragment Profile.toSection)
        , Parser.map DeleteAccountRoute (Parser.s "delete-account")
        , Parser.map PleaseConfirmEmailRoute (Parser.s "please-confirm-email")
        , Parser.map ResetPasswordRoute (Parser.s "reset-password" <?> Query.string "reset_password_token")
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
    | ForgotPassword ForgotPassword.Model
    | ResetPassword ResetPassword.Model
    | Signup Signup.Model
    | Profile Profile.Model
    | Loading Url.Url
    | Forbidden
    | DeleteAccount DeleteAccount.Model
    | PleaseConfirmEmail
    | Error String


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
    , profileSubject : Maybe { user : User, takes : List Data.Take.Take }
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
            , profileSubject = Nothing
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

        ProfileRoute userId section ->
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
            ( { model | page = ForgotPassword <| ForgotPassword.init "" }
            , Cmd.batch [ loadAuthCmd, setTimeZone ]
            )

        ResetPasswordRoute token ->
            ( { model | page = ResetPassword <| ResetPassword.init token }
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

        PleaseConfirmEmailRoute ->
            ( { model | page = PleaseConfirmEmail }
            , Cmd.none
            )

        NotFound ->
            ( model, Cmd.none )



-- UPDATE


type Msg
    = FeedMsg Feed.Msg
    | ProfileMsg Profile.Msg
    | ProfileSubjectLoaded (Result Http.Error { user : User, takes : List Data.Take.Take })
    | LoginMsg Login.Msg
    | SignupMsg Signup.Msg
    | DeleteAccountMsg DeleteAccount.Msg
    | FeedLoaded (Result Http.Error (List Data.Take.Take))
    | ForgotPasswordMsg ForgotPassword.Msg
    | ResetPasswordMsg ResetPassword.Msg
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
            ( { model | dimensions = dim, showNavBar = False }
            , Cmd.none
            )

        ProfileSubjectLoaded (Ok subject) ->
            case model.page of
                Loading next ->
                    ( { model | profileSubject = Just subject }
                    , Nav.pushUrl model.navKey (Url.toString next)
                    )

                _ ->
                    ( model, Cmd.none )

        ProfileSubjectLoaded (Err _) ->
            case model.page of
                Loading next ->
                    ( { model | page = Error "The user you are looking for does not exist" }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            updatePage msg model


handleUrlChange : Model -> Url.Url -> ( Model, Cmd Msg )
handleUrlChange mdl url =
    let
        model =
            { mdl | showNavBar = False }
    in
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
            let
                email =
                    case model.page of
                        Login m ->
                            m.email

                        _ ->
                            ""
            in
            ( { model | page = ForgotPassword <| ForgotPassword.init email }, Cmd.none )

        ResetPasswordRoute token ->
            ( { model | page = ResetPassword <| ResetPassword.init token }
            , Cmd.none
            )

        SignupRoute ->
            ( { model | page = Signup Signup.init }, Cmd.none )

        ProfileRoute userId section ->
            case model.profile of
                Just { user, auth } ->
                    handleUrlChangeToProfile model section user auth userId url

                Nothing ->
                    ( model, Cmd.none )

        DeleteAccountRoute ->
            ( { model | page = DeleteAccount DeleteAccount.init }, Cmd.none )

        PleaseConfirmEmailRoute ->
            ( { model | page = PleaseConfirmEmail }, Cmd.none )

        NotFound ->
            ( model, Cmd.none )


handleUrlChangeToProfile : Model -> Profile.Section -> User -> Api.UserAuth -> Maybe Int -> Url.Url -> ( Model, Cmd Msg )
handleUrlChangeToProfile model section user auth maybeUserId url =
    let
        userId =
            Maybe.withDefault user.id maybeUserId
    in
    case model.profileSubject of
        Nothing ->
            ( { model | page = Loading url }
            , Api.userById auth userId ProfileSubjectLoaded
            )

        Just subject ->
            if subject.user.id == userId then
                ( { model
                    | page = Profile <| Profile.toModel section subject.user subject.takes
                  }
                , Cmd.none
                )

            else
                ( { model | page = Loading url, profileSubject = Nothing }
                , Api.userById auth userId ProfileSubjectLoaded
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

        ForgotPassword data ->
            updateForgotPasswordPage msg model data

        ResetPassword data ->
            updateResetPasswordPage msg model data

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

        PleaseConfirmEmail ->
            ( model, Cmd.none )

        Error _ ->
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


updateForgotPasswordPage : Msg -> Model -> ForgotPassword.Model -> ( Model, Cmd Msg )
updateForgotPasswordPage msg model data =
    case msg of
        ForgotPasswordMsg m ->
            let
                ( newData, cmd ) =
                    ForgotPassword.update m data
            in
            ( { model | page = ForgotPassword newData }
            , Cmd.map ForgotPasswordMsg cmd
            )

        _ ->
            ( model, Cmd.none )


updateResetPasswordPage : Msg -> Model -> ResetPassword.Model -> ( Model, Cmd Msg )
updateResetPasswordPage msg model data =
    case msg of
        ResetPasswordMsg m ->
            let
                ( newData, cmd ) =
                    ResetPassword.update m data
            in
            ( { model | page = ResetPassword newData }
            , Cmd.map ResetPasswordMsg cmd
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
            ( model, Ports.error <| "FeedLoaded error: " ++ httpErrorToString m )

        TakeUpdate (Err m) ->
            ( model, Ports.error <| "FeedLoaded error: " ++ Json.Decode.errorToString m )

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
            case model.profile of
                Just { user } ->
                    Profile.update msg data user auth

                Nothing ->
                    ( data, Cmd.none )

        newProfile =
            case ( model.profile, Profile.updatedUserInfo msg ) of
                ( Just { user }, Just userFromProfile ) ->
                    if user.id == userFromProfile.id then
                        model.profile
                            |> Maybe.map (\profile -> { profile | user = userFromProfile })

                    else
                        model.profile

                _ ->
                    model.profile
    in
    ( { model | page = Profile newData, profile = newProfile }
    , Cmd.map ProfileMsg cmd
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        colorScheme =
            colorSchemeForUser (Maybe.map .user model.profile)
    in
    if isThursday model.time model.zone then
        { title = "HTT"
        , body = [ viewForThursday model colorScheme ]
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


viewForThursday : Model -> ColorScheme -> Html Msg
viewForThursday model colorScheme =
    if isSmallScreen model.dimensions then
        smallDeviceView model colorScheme

    else
        largeDeviceView model colorScheme


noFocus : FocusStyle
noFocus =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


largeDeviceView : Model -> ColorScheme -> Html Msg
largeDeviceView model colorScheme =
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
            ++ [ inFront <| largeDeviceHeader model colorScheme ]
        )
    <|
        largeDeviceBody model.dimensions shouldShowAds <|
            largeDeviceContent model colorScheme


largeDeviceBody : Dimensions -> Bool -> Element Msg -> Element Msg
largeDeviceBody dim shouldShowAds elmt =
    el
        [ paddingXY 12 76
        , centerX
        , width <| fullWidth dim shouldShowAds
        ]
        elmt


smallDeviceBody : Element Msg -> Element Msg
smallDeviceBody elmt =
    el
        [ paddingXY 12 76
        , centerX
        , width fill
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


smallDeviceView : Model -> ColorScheme -> Html Msg
smallDeviceView model colorScheme =
    layoutWith
        { options = [ focusStyle noFocus ] }
        [ width fill
        , inFront <| smallDeviceHeader model colorScheme
        ]
    <|
        smallDeviceBody <|
            smallDeviceContent model colorScheme


largeDeviceHeader : Model -> ColorScheme -> Element Msg
largeDeviceHeader model colorScheme =
    let
        links =
            navLinks colorScheme model.page model.profile model.profileSubject
    in
    row
        [ width fill
        , padding 15
        , Background.color colorScheme.primary
        , Font.color colorScheme.textOnPrimary
        ]
        [ title
        , el [ alignRight ] (row [ spacing 24 ] <| List.map (\f -> f False True) links)
        ]


smallDeviceHeader : Model -> ColorScheme -> Element Msg
smallDeviceHeader model colorScheme =
    let
        links =
            navLinks colorScheme model.page model.profile model.profileSubject

        titleAndHamburger =
            [ title
            , hamburger
            ]
    in
    case model.showNavBar of
        True ->
            column
                [ width fill
                , paddingEach { top = 15, bottom = 0, left = 0, right = 0 }
                , spacing 20
                , Background.color colorScheme.primary
                , Font.color colorScheme.textOnPrimary
                ]
                [ row [ width fill, paddingXY 15 0 ] titleAndHamburger
                , column
                    [ Background.color colorScheme.primaryDark
                    , spacing 15
                    , width fill
                    , padding 15
                    ]
                  <|
                    borderBetween (List.map (\f -> f True) links)
                ]

        False ->
            row
                [ width fill
                , padding 15
                , Background.color colorScheme.primary
                , Font.color colorScheme.textOnPrimary
                ]
                titleAndHamburger


title : Element Msg
title =
    link [ Font.size 24 ] { url = "/", label = text "HotTakeThursday 🔥" }


hamburger : Element Msg
hamburger =
    Input.button
        [ alignRight
        , Font.size 30
        ]
        { label = text "☰"
        , onPress = Just NavBarToggled
        }


navLinks : ColorScheme -> Page -> Maybe { a | user : User } -> Maybe { b | user : User } -> List (Bool -> Bool -> Element Msg)
navLinks colorScheme page profile profileSubject =
    case page of
        Home _ ->
            case profile of
                Just { user } ->
                    [ navItem colorScheme "Notifications" "/profile#notifications"
                    , navItem colorScheme "Profile" "profile"
                    , navItem colorScheme "Delete Account" "delete-account"
                    , logoutButton colorScheme
                    ]

                Nothing ->
                    [ navItem colorScheme "Login" "login"
                    , navItem colorScheme "Sign Up" "signup"
                    ]

        Login _ ->
            [ navItem colorScheme "Sign Up" "signup" ]

        ForgotPassword _ ->
            [ navItem colorScheme "Login" "login"
            , navItem colorScheme "Sign Up" "signup"
            ]

        ResetPassword _ ->
            [ navItem colorScheme "Login" "login"
            , navItem colorScheme "Sign Up" "signup"
            ]

        Signup _ ->
            [ navItem colorScheme "Login" "login" ]

        Profile _ ->
            case ( profile, profileSubject ) of
                ( Just { user }, Just subject ) ->
                    if user.id == subject.user.id then
                        [ logoutButton colorScheme
                        , navItem colorScheme "Delete Account" "delete-account"
                        ]

                    else
                        [ navItem colorScheme "Notifications" "/profile#notifications"
                        , navItem colorScheme "Profile" "profile"
                        , navItem colorScheme "Delete Account" "delete-account"
                        , logoutButton colorScheme
                        ]

                ( Nothing, _ ) ->
                    [ navItem colorScheme "Login" "login"
                    , navItem colorScheme "Sign Up" "signup"
                    ]

                ( Just { user }, Nothing ) ->
                    [ logoutButton colorScheme
                    , navItem colorScheme "Delete Account" "delete-account"
                    ]

        Loading _ ->
            []

        Forbidden ->
            [ navItem colorScheme "Login" "login"
            , navItem colorScheme "Sign Up" "signup"
            ]

        DeleteAccount _ ->
            [ logoutButton colorScheme ]

        PleaseConfirmEmail ->
            []

        Error _ ->
            []


showAds : Model -> Bool
showAds model =
    case model.page of
        Home _ ->
            model.dimensions.width > Feed.feedWidth + 2 * adsWidth

        _ ->
            False


logoutButton : ColorScheme -> Bool -> Bool -> Element Msg
logoutButton colorScheme expand first =
    Input.button
        (navItemAttributes colorScheme expand first)
        { onPress = Just LogoutButtonPressed
        , label = text "Logout"
        }


navItem : ColorScheme -> String -> String -> Bool -> Bool -> Element Msg
navItem colorScheme txt link_ expand first =
    link
        (navItemAttributes colorScheme expand first)
        { url = link_, label = text txt }


navItemAttributes : ColorScheme -> Bool -> Bool -> List (Attribute Msg)
navItemAttributes colorScheme expand first =
    let
        expandAttributes =
            [ width fill, Font.alignRight, padding 12, Font.size 24 ]

        borderAttributes =
            [ Border.widthEach { left = 0, right = 0, top = 1, bottom = 0 }
            , Border.color colorScheme.lightGray
            ]
    in
    if expand && not first then
        borderAttributes ++ expandAttributes

    else if expand then
        expandAttributes

    else
        []


largeDeviceContent : Model -> ColorScheme -> Element Msg
largeDeviceContent model colorScheme =
    case ( model.page, model.profile ) of
        ( Home data, Just { user } ) ->
            Element.map FeedMsg <| Feed.view data colorScheme  user model.time

        ( Home data, Nothing ) ->
            welcomePage colorScheme

        ( Login data, Nothing ) ->
            Element.map LoginMsg (Login.view data colorScheme)

        ( Login data, Just { user } ) ->
            alreadySignedIn user.username

        ( ForgotPassword email, Nothing ) ->
            Element.map ForgotPasswordMsg <| ForgotPassword.view email colorScheme

        ( ForgotPassword _, Just { user } ) ->
            alreadySignedIn user.username

        ( ResetPassword email, Nothing ) ->
            Element.map ResetPasswordMsg <| ResetPassword.view email colorScheme

        ( ResetPassword _, Just { user } ) ->
            alreadySignedIn user.username

        ( Signup data, Nothing ) ->
            Element.map SignupMsg (Signup.view data colorScheme)

        ( Signup data, Just { user } ) ->
            alreadySignedIn user.username

        ( Profile data, Just { user } ) ->
            Element.map ProfileMsg (Profile.view data colorScheme user model.time)

        ( Profile data, Nothing ) ->
            forbiddenView NotLoggedIn

        ( Loading next, _ ) ->
            text "Loading..."

        ( Forbidden, Just _ ) ->
            forbiddenView LoggedIn

        ( Forbidden, Nothing ) ->
            forbiddenView NotLoggedIn

        ( DeleteAccount data, Just { user } ) ->
            Element.map DeleteAccountMsg <| DeleteAccount.view data colorScheme

        ( DeleteAccount _, Nothing ) ->
            text "You're not even signed in lol"

        ( PleaseConfirmEmail, Just { user } ) ->
            text "You're signed in. Go home."

        ( PleaseConfirmEmail, Nothing ) ->
            text "Thanks! Check your email for a confirmation link!"

        ( Error m, _ ) ->
            text m


smallDeviceContent : Model -> ColorScheme -> Element Msg
smallDeviceContent model colorScheme =
    case ( model.page, model.profile ) of
        ( Home data, Just { user } ) ->
            Element.map FeedMsg <| Feed.smallView data colorScheme  user model.time

        ( Home data, Nothing ) ->
            welcomePage colorScheme

        ( Login data, Nothing ) ->
            Element.map LoginMsg (Login.smallView data colorScheme)

        ( Login data, Just { user } ) ->
            alreadySignedIn user.username

        ( ForgotPassword email, Nothing ) ->
            Element.map ForgotPasswordMsg <| ForgotPassword.smallView email colorScheme

        ( ForgotPassword _, Just { user } ) ->
            alreadySignedIn user.username

        ( ResetPassword email, Nothing ) ->
            Element.map ResetPasswordMsg <| ResetPassword.smallView email colorScheme

        ( ResetPassword _, Just { user } ) ->
            alreadySignedIn user.username

        ( Signup data, Nothing ) ->
            Element.map SignupMsg (Signup.smallView data colorScheme)

        ( Signup data, Just { user } ) ->
            alreadySignedIn user.username

        ( Profile data, Just { user } ) ->
            Element.map ProfileMsg (Profile.smallView data colorScheme user model.time)

        ( Profile data, Nothing ) ->
            forbiddenView NotLoggedIn

        ( Loading next, _ ) ->
            text "Loading..."

        ( Forbidden, Just _ ) ->
            forbiddenView LoggedIn

        ( Forbidden, Nothing ) ->
            forbiddenView NotLoggedIn

        ( DeleteAccount data, Just { user } ) ->
            Element.map DeleteAccountMsg <| DeleteAccount.view data colorScheme

        ( DeleteAccount _, Nothing ) ->
            text "You're not even signed in lol"

        ( PleaseConfirmEmail, Just { user } ) ->
            text "You're signed in. Go home."

        ( PleaseConfirmEmail, Nothing ) ->
            text "Thanks! Check your email for a confirmation link!"

        ( Error m, _ ) ->
            text m


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

type LoginState = LoggedIn | NotLoggedIn

forbiddenView : LoginState -> Element Msg
forbiddenView loginState =
    let
        msg = 
            case loginState of
            LoggedIn ->
                "Sorry! You don't have permission to view this"
                    ++ " page. If you think this is an error, please"
                    ++ " report it to noonecares@yahoo.net."
            
            NotLoggedIn ->
                "Sorry! You don't have permission to view this"
                    ++ " page. Please login or signup using the links above."
    in
    paragraph [] [ text msg ]



forgotPasswordView : String -> Element Msg
forgotPasswordView email =
    paragraph [ spacing 12, Font.size 24 ]
        [ text <|
            "We've sent you an email"
                ++ " with a link to reset "
                ++ "your password. Remember to not "
                ++ "forget your password again! I like "
                ++ "to write mine on my forehead!"
        ]


deleteAccountView : ColorScheme -> Element Msg
deleteAccountView colorScheme =
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
            , Border.color colorScheme.secondary
            , padding 10
            , Border.rounded 7
            , Font.color colorScheme.primary
            ]
            { onPress = Nothing
            , label = text "Goodbye :("
            }
        ]


welcomePage : ColorScheme -> Element Msg
welcomePage colorScheme = 
            column
                [ spacing 24
                , centerX
                , padding 24
                ]
                [ paragraph [] [ text "Hi!" ]
                , paragraph [ spacing 12 ] 
                    [ text "Welcome to HotTakeThursday, the best site for sharing your hot takes, but only on Thursdays. To see or post some hot takes, please "
                    , link [ Font.color colorScheme.link ] { url = "/login", label = text "log in" }
                    , text " or "
                    , link [ Font.color colorScheme.link ] { url = "/signup", label = text "sign up" }
                    , text ". It's free!"
                    ]
                ]


isSmallScreen : { window | height : Int, width : Int } -> Bool
isSmallScreen window =
    window.width < 730



-- there is probably a better way to do this


borderBetween : List (Bool -> Element Msg) -> List (Element Msg)
borderBetween toElements =
    case toElements of
        first :: rest ->
            first True :: List.map (\f -> f False) rest

        [] ->
            []
