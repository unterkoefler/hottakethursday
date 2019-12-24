module Main exposing (..)

import Api
import Browser
import Browser.Navigation as Nav
import Data.User as User exposing (User)
import Flags
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Http
import Json.Decode
import Ports
import Task
import Time
import Url
import Url.Parser as Parser exposing ((</>), Parser, fragment, map, oneOf, parse, s, top)



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


type alias Take =
    { content : String
    , postedBy : User
    , timePosted : Time.Posix
    , likedBy : List User
    , hoveredOver : Bool
    }


type alias HomeData =
    { takes : List Take
    , newTake : String
    }


type alias LoginData =
    { email : String
    , password : String
    , previousInvalidAttempt : Bool
    }


type alias SignupData =
    { name : String
    , username : String
    , email : String
    , birthday : String
    }


blankSignupData =
    { name = ""
    , username = ""
    , email = ""
    , birthday = ""
    }


type Page
    = Home HomeSection HomeData
    | Login LoginData
    | ForgotPassword String
    | Signup SignupData
    | Profile ProfileSection User


type alias Model =
    { page : Page
    , profile : Maybe { user : User, auth : Api.UserAuth }
    , time : Time.Posix
    , zone : Time.Zone
    , url : Url.Url
    , navKey : Nav.Key
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
    Home Hottest { takes = [ take1 ], newTake = "" }


homePageCold =
    Home Coldest { takes = [], newTake = "" }


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
    = EditNewTake String
    | PublishNewTakeClick
    | PublishNewTake Time.Posix
    | AdjustTimeZone Time.Zone
    | Tick Time.Posix
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | LoginButtonPressed
    | LoginEmailChanged String
    | LoginPasswordChanged String
    | LoginAttemptCompleted (Result Api.SignInError { user : User.User, auth : Api.UserAuth })
    | LogoutButtonPressed
    | LogoutRequestHandled (Result Http.Error ())
    | SignupButtonPressed
    | SignupEditName String
    | SignupEditUsername String
    | SignupEditEmail String
    | SignupEditBirthday String
    | FireButtonPressed Take
    | TakeHovered Take
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


updateSignupPage : Msg -> Model -> SignupData -> ( Model, Cmd Msg )
updateSignupPage msg model data =
    case msg of
        SignupButtonPressed ->
            if validateSignup data then
                ( model, Cmd.none )
                -- TODO: Fix this so that a sign up actually does something

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

        SignupEditEmail newEmail ->
            ( { model | page = Signup { data | email = newEmail } }
            , Cmd.none
            )

        SignupEditBirthday newBirthday ->
            ( { model | page = Signup { data | birthday = handleBirthdayInput data.birthday newBirthday } }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


handleBirthdayInput : String -> String -> String
handleBirthdayInput prev new =
    if String.length prev < String.length new then
        if String.length new == 1 then
            if new == "0" || new == "1" then
                new

            else
                "0" ++ new ++ "/"

        else if String.length new == 2 then
            case String.toInt new of
                Just _ ->
                    new ++ "/"

                Nothing ->
                    new

        else if String.right 2 new == "//" then
            String.dropRight 1 new

        else if String.length new == 5 then
            case String.toInt <| String.right 2 new of
                Just _ ->
                    new ++ "/"

                Nothing ->
                    if
                        (String.right 1 new == "/")
                            && (String.toInt (String.slice 3 4 new) /= Nothing)
                    then
                        String.slice 0 3 new ++ "0" ++ String.slice 3 4 new ++ "/"

                    else
                        new

        else
            new

    else
        new


validateSignup : SignupData -> Bool
validateSignup data =
    not (String.isEmpty data.name) && not (String.isEmpty data.username)


updateLoginPage : Msg -> Model -> LoginData -> ( Model, Cmd Msg )
updateLoginPage msg model data =
    case msg of
        LoginButtonPressed ->
            if data.email /= "" && data.password /= "" then
                ( model
                , Api.signIn data LoginAttemptCompleted
                )

            else
                ( { model | page = Login { data | previousInvalidAttempt = True } }, Cmd.none )

        LoginEmailChanged newEmail ->
            ( { model | page = Login { data | email = newEmail } }, Cmd.none )

        LoginPasswordChanged newPassword ->
            ( { model | page = Login { data | password = newPassword } }, Cmd.none )

        LoginAttemptCompleted (Ok profile) ->
            ( { model | profile = Just profile }
            , Cmd.batch
                [ Ports.storeAuthToken (Api.encodeUserAuth profile.auth)
                , Nav.pushUrl model.navKey "/"
                ]
            )

        LoginAttemptCompleted (Err _) ->
            -- TODO Determine based on error whether it was actually invalid creds
            ( { model | page = Login { data | previousInvalidAttempt = True } }, Cmd.none )

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

        FireButtonPressed take ->
            ( { model | page = Home Hottest (handleFireButtonPress take data user) }, Cmd.none )

        TakeHovered take ->
            ( { model | page = Home Hottest (handleTakeHover take data user) }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


handleTakeHover : Take -> HomeData -> User -> HomeData
handleTakeHover take data user =
    { data | takes = List.map (\tk -> toggleHover tk take) data.takes }


toggleHover : Take -> Take -> Take
toggleHover takeA takeB =
    if takeA == takeB then
        { takeA | hoveredOver = not takeA.hoveredOver }

    else
        takeA


handleFireButtonPress : Take -> HomeData -> User -> HomeData
handleFireButtonPress take data user =
    if List.member user take.likedBy then
        { data | takes = List.map (\tk -> unlikeTake tk take user) data.takes }

    else
        { data | takes = List.map (\tk -> likeTake tk take user) data.takes }


unlikeTake : Take -> Take -> User -> Take
unlikeTake takeA takeB user =
    if takeA == takeB then
        { takeA | likedBy = List.filter (\u -> u /= user) takeA.likedBy }

    else
        takeA


likeTake : Take -> Take -> User -> Take
likeTake takeA takeB user =
    if takeA == takeB then
        { takeA | likedBy = user :: takeA.likedBy }

    else
        takeA


createNewTake : String -> User -> Time.Posix -> Take
createNewTake newTake user time =
    { content = newTake
    , postedBy = user
    , timePosted = time
    , likedBy = []
    , hoveredOver = False
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
                    case model.profile of
                        Just { user } ->
                            [ navItem "🔔" "profile#notifications" ""
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
            loginBody data

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
            signupBody data

        Profile _ user ->
            div [ class "row" ]
                [ div [ class "col-3" ] (aboutUser user)
                , div [ class "col-9" ] (content model)
                ]


ads =
    [ fakeAd, fakeAd, fakeAd ]


fakeAd =
    img [ class "w-100 mb-5 mt-5 pl-5 pr-5", height 200, src "/assets/trash-ad.jpg" ] []


loginBody : LoginData -> Html Msg
loginBody data =
    div [ id "loginBody" ]
        [ div [ class "container form mx-auto" ]
            (inputWithLabel "email" "Email" data.email LoginEmailChanged
                ++ inputWithLabel "password" "Password" data.password LoginPasswordChanged
                ++ [ div [] [ a [ href "forgot-password" ] [ text "Forgot password?" ] ]
                   , div [] [ button [ onClick LoginButtonPressed ] [ text "Continue" ] ]
                   ]
                ++ (case data.previousInvalidAttempt of
                        True ->
                            [ div [] [ p [ class "text-danger" ] [ text "Invalid Username or Password" ] ] ]

                        False ->
                            []
                   )
            )
        ]


signupBody : SignupData -> Html Msg
signupBody data =
    div [ class "container" ]
        ([ h2 [] [ text "Create Account" ]
         , p [] [ text "Feed us your data" ]
         ]
            ++ inputWithLabel "name" "Name" data.name SignupEditName
            ++ inputWithLabel "username" "Username" data.username SignupEditUsername
            ++ inputWithLabel "email" "Email" data.email SignupEditEmail
            ++ inputWithLabel "bday" "Birthday (MM/DD/YYYY)" data.birthday SignupEditBirthday
            ++ [ div []
                    [ button
                        [ onClick SignupButtonPressed
                        , disabled <| not <| validateSignup data
                        ]
                        [ text "Begin" ]
                    ]
               ]
        )


inputWithLabel : String -> String -> String -> (String -> Msg) -> List (Html Msg)
inputWithLabel id_ text_ val msg =
    let
        type__ =
            if id_ == "password" then
                "password"

            else
                "input"
    in
    [ div [] [ label [ for id_ ] [ text text_ ] ]
    , div [] [ input [ type_ type__, id id_, onInput msg, value val ] [] ]
    ]


content : Model -> List (Html Msg)
content model =
    [ ul [ class "nav nav-tabs mb-3 mt-2" ]
        (navPills model.page)
    , div [ class "container px-0" ]
        (case model.page of
            Home Hottest data ->
                case model.profile of
                    Just { user } ->
                        [ compose user data.newTake
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


compose : User -> String -> Html Msg
compose user newTake =
    div
        [ style "padding-left" "15px"
        , style "padding-right" "15px"
        ]
        [ div []
            [ input
                [ placeholder ("Hi " ++ user.username ++ ". What's your hottest take?")
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


feed : List Take -> Time.Zone -> Maybe User -> Html Msg
feed takes zone user =
    div [ class "mt-3" ] (List.map (\take -> viewTake take zone user) takes)


viewTake : Take -> Time.Zone -> Maybe User -> Html Msg
viewTake take zone user =
    div
        [ class "media border border-warning p-3"
        , onMouseEnter <| TakeHovered take
        , onMouseLeave <| TakeHovered take
        ]
        [ img [ class "mr-2", width 64, height 64, src "assets/profilepic.jpg" ] []
        , div [ class "media-body pr-3" ]
            ([ p [ class "mb-0" ] [ text ("\"" ++ take.content ++ "\"") ]
             , p [ class "text-right" ] [ text <| "- @" ++ take.postedBy.username ]
             ]
                ++ hoverButtons take user
            )
        , fireButton take user take.likedBy
        ]


hoverButtons : Take -> Maybe User -> List (Html Msg)
hoverButtons take user =
    let
        buttons =
            if Just take.postedBy == user then
                [ takeHoverButton "edit"
                , text " | "
                , takeHoverButton "delete"
                ]

            else
                [ takeHoverButton "report" ]
    in
    if take.hoveredOver then
        [ div
            [ class "text-center" ]
            buttons
        ]

    else
        []


takeHoverButton : String -> Html Msg
takeHoverButton txt =
    button [ class "btn-link" ] [ text txt ]


fireButton : Take -> Maybe User -> List User -> Html Msg
fireButton take maybeUser likers =
    case maybeUser of
        Just user ->
            if List.member user likers then
                button
                    [ class "align-self-end align-self-center fire-button"
                    , onClick (FireButtonPressed take)
                    ]
                    [ text <| String.fromInt <| List.length likers ]

            else
                button
                    [ class "align-self-end align-self-center fire-button-transparent"
                    , onClick (FireButtonPressed take)
                    ]
                    [ text <| String.fromInt <| List.length likers ]

        Nothing ->
            button
                [ class "align-self-end align-self-center fire-button" ]
                [ text <| String.fromInt <| List.length likers ]


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
