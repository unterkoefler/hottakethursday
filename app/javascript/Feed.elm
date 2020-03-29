module Feed exposing (FeedSection, Model, Msg, feedWidth, fromTakes, init, toFeedSection, update, view)

import Api
import Colors
import Data.Take as Take exposing (Take)
import Data.User as User exposing (User)
import Debug
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import NavTabs exposing (navTab)
import Task
import Thursday exposing (toWeekdayString)
import Time



-- MODEL


type alias Model =
    { compose : Compose
    , cards : List TakeCard
    , section : FeedSection
    }


type FeedSection
    = Hottest
    | Coldest


init : Model
init =
    { cards = []
    , compose = { content = "", state = Composing }
    , section = Hottest
    }


toFeedSection : Maybe String -> FeedSection
toFeedSection frag =
    case frag of
        Just "hottest" ->
            Hottest

        Just "coldest" ->
            Coldest

        _ ->
            Hottest


type alias Compose =
    { content : String
    , state : ComposeState
    }


type alias TakeCard =
    { take : Take
    , state : CardState
    }


type CardState
    = Deleting
    | FailedToDelete
    | Focused
    | Default


type ComposeState
    = Composing
    | Posting
    | FailedToPost



-- UPDATE


type Msg
    = FireButtonPressed TakeCard
    | TakeFocused TakeCard
    | EditTake TakeCard
    | DeleteTake TakeCard
    | ReportTake TakeCard
    | LikeHandled (Result Http.Error ())
    | DeleteHandled Int (Result Http.Error ())
    | EditNewTake String
    | PublishNewTakeClick
    | PublishNewTake Time.Posix
    | TakePublished (Result Http.Error Take)


update : Msg -> Model -> User -> Api.UserAuth -> ( Model, Cmd Msg )
update msg model user auth =
    case msg of
        EditNewTake newTake ->
            ( { model | compose = updateContent model.compose newTake }
            , Cmd.none
            )

        PublishNewTakeClick ->
            ( model, Task.perform PublishNewTake Time.now )

        PublishNewTake time ->
            ( { model
                | compose = toComposeState model.compose Posting
              }
            , Api.makeTake auth model.compose.content TakePublished
            )

        TakePublished (Err m) ->
            ( { model
                | compose = toComposeState model.compose FailedToPost
              }
            , Cmd.none
            )

        TakePublished (Ok take) ->
            ( { model
                | cards = { take = take, state = Default } :: model.cards
                , compose = { state = Composing, content = "" }
              }
            , Cmd.none
            )

        FireButtonPressed card ->
            ( { model | cards = findAndApply card (likeOrUnlike user) model.cards }
            , sendLikeOrUnlike user auth card.take
            )

        TakeFocused card ->
            ( { model | cards = findAndApply card toggleFocus model.cards }
            , Cmd.none
            )

        LikeHandled (Err m) ->
            let
                _ =
                    Debug.log "like handled error" m
            in
            ( model
            , Cmd.none
            )

        DeleteTake card ->
            ( { model | cards = findAndApply card deleteTake model.cards }
            , Api.deleteTake auth card.take.id (DeleteHandled card.take.id)
            )

        DeleteHandled takeId (Ok _) ->
            ( { model | cards = removeCard takeId model.cards }
            , Cmd.none
            )

        DeleteHandled takeId (Err m) ->
            ( { model | cards = findAndApplyById takeId failedToDelete model.cards }
            , Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )


toComposeState : Compose -> ComposeState -> Compose
toComposeState compose state =
    { compose | state = state }


updateContent : Compose -> String -> Compose
updateContent compose newContent =
    { compose | content = newContent }



-- if x is found in l, applies f to x and returns the new list


findAndApply : a -> (a -> a) -> List a -> List a
findAndApply x f l =
    List.map
        (\e ->
            if e == x then
                f x

            else
                e
        )
        l


findAndApplyById : Int -> (TakeCard -> TakeCard) -> List TakeCard -> List TakeCard
findAndApplyById id f l =
    List.map
        (\card ->
            if card.take.id == id then
                f card

            else
                card
        )
        l


removeCard : Int -> List TakeCard -> List TakeCard
removeCard id cards =
    List.filter (\c -> c.take.id /= id) cards


fromTakes : List Take -> List TakeCard
fromTakes takes =
    List.map (\t -> { take = t, state = Default }) takes



--createNewTake : String -> User -> Time.Posix -> TakeCard
--createNewTake newTake user time =
--    let
--        take =
--            { id = 0
--            , content = newTake
--            , postedBy = user
--            , timePosted = time
--            , usersWhoLiked = []
--            }
--    in
--    { take = take
--    , state = Posting
--    }
--failedToPost : TakeCard -> TakeCard
--failedToPost t =
--    case t.state of
--        Posting ->
--            { t | state = FailedToPost }
--        _ ->
--            t
--makeDefault : TakeCard -> TakeCard
--makeDefault t =
--    case t.state of
--        Posting ->
--            { t | state = Default }
--        _ ->
--            t


failedToDelete : TakeCard -> TakeCard
failedToDelete t =
    case t.state of
        Deleting ->
            { t | state = FailedToDelete }

        _ ->
            t


toggleFocus : TakeCard -> TakeCard
toggleFocus t =
    case t.state of
        Default ->
            { t | state = Focused }

        Focused ->
            { t | state = Default }

        _ ->
            t


deleteTake : TakeCard -> TakeCard
deleteTake card =
    case card.state of
        Default ->
            { card | state = Deleting }

        Focused ->
            { card | state = Deleting }

        FailedToDelete ->
            { card | state = Deleting }

        _ ->
            card


likeOrUnlike : User -> TakeCard -> TakeCard
likeOrUnlike user card =
    { card | take = likeOrUnlikeTake user card.take }


likeOrUnlikeTake : User -> Take -> Take
likeOrUnlikeTake user take =
    if List.member user take.usersWhoLiked then
        { take | usersWhoLiked = List.filter (\u -> u /= user) take.usersWhoLiked }

    else
        { take | usersWhoLiked = user :: take.usersWhoLiked }


sendLikeOrUnlike : User -> Api.UserAuth -> Take -> Cmd Msg
sendLikeOrUnlike user auth take =
    if List.member user take.usersWhoLiked then
        Api.unlike auth take.id LikeHandled

    else
        Api.like auth take.id LikeHandled



-- VIEW


feedWidth =
    2
        * cardSpacing
        + 2
        * cardPadding
        + takeWidth
        + thumbnailWidth
        + fireButtonWidth
        + 50


cardSpacing =
    12


cardPadding =
    6


takeWidth =
    500


thumbnailWidth =
    64


thumbnailHeight =
    thumbnailWidth


fireButtonWidth =
    36


fireButtonHeight =
    fireButtonWidth


cardBorderWidth =
    1


viewTake : TakeCard -> Maybe User -> Element Msg
viewTake card user =
    case card.state of
        Default ->
            defaultView card user

        Focused ->
            focusedView card user

        Deleting ->
            deletingView

        FailedToDelete ->
            column [ spacing 24 ]
                [ failedView "Failed to delete."
                , focusedView card user
                ]


defaultView : TakeCard -> Maybe User -> Element Msg
defaultView card user =
    Input.button
        [ Border.width 1
        , Border.rounded 7
        , Border.color Colors.secondary
        ]
        { onPress = Just <| TakeFocused card
        , label = takeCardContents card user False
        }


focusedView : TakeCard -> Maybe User -> Element Msg
focusedView card user =
    Input.button
        [ Border.width 1
        , Border.rounded 7
        , Border.color Colors.secondary
        ]
        { onPress = Just <| TakeFocused card
        , label = takeCardContents card user True
        }


postingView : Element Msg
postingView =
    text "Posting the take..."


failedView : String -> Element Msg
failedView error =
    text <| error ++ " Have you tried plugging it in?"


deletingView : Element Msg
deletingView =
    text "Deleting the take..."


takeCardContents : TakeCard -> Maybe User -> Bool -> Element Msg
takeCardContents card user focused =
    column
        [ spacing cardSpacing
        , padding cardPadding
        ]
        [ row
            [ spacing cardSpacing
            , padding cardPadding
            ]
            [ profilePicThumbnail card
            , el [ width (px takeWidth) ] <| takeAndAuthor card.take
            , fireButton card user card.take.usersWhoLiked
            ]
        , if focused then
            focusButtons card user

          else
            none
        ]


takeAndAuthor : Take -> Element Msg
takeAndAuthor take =
    textColumn
        [ spacing 12
        , paddingEach { left = 5, right = 30, top = 5, bottom = 5 }
        , width (px takeWidth)
        , alignLeft
        ]
        [ paragraph [] [ text <| "\"" ++ take.content ++ "\"" ]
        , el [ Font.alignRight ]
            (text <| "- @" ++ take.postedBy.username)
        ]


profilePicThumbnail : TakeCard -> Element Msg
profilePicThumbnail card =
    image
        [ width (px thumbnailWidth)
        , height (px thumbnailHeight)
        , clip
        , alignTop
        , Border.rounded 500
        ]
        { src = Maybe.withDefault "/assets/profilepic.jpg" card.take.postedBy.avatarUrl
        , description = "User's profile picture"
        }


focusButtons : TakeCard -> Maybe User -> Element Msg
focusButtons card user =
    let
        buttons =
            if Just card.take.postedBy == user then
                [ takeFocusButton "delete" (DeleteTake card) ]

            else
                [ takeFocusButton "report" (ReportTake card) ]
    in
    row [ centerX ] buttons


takeFocusButton : String -> Msg -> Element Msg
takeFocusButton txt msg =
    Input.button [ Font.color Colors.link ]
        { onPress = Just msg, label = text txt }


fireButton : TakeCard -> Maybe User -> List User -> Element Msg
fireButton card maybeUser likers =
    let
        likeCount =
            List.length likers

        onPress =
            case maybeUser of
                Just _ ->
                    Just <| FireButtonPressed card

                Nothing ->
                    Nothing

        canLike =
            not <| memberWithMaybe maybeUser likers True

        url =
            if canLike then
                "/assets/fire-transparent.png"

            else
                "/assets/fire.png"
    in
    Input.button
        [ Border.color Colors.secondaryLight
        , Border.width 1
        , Border.rounded 7
        ]
        { onPress = onPress
        , label = fireAndLikeCount url likeCount
        }


fireAndLikeCount : String -> Int -> Element Msg
fireAndLikeCount url likeCount =
    row
        [ padding 5
        , spacing 3
        ]
        [ image [ width (px fireButtonWidth) ] { src = url, description = "A fire emoji" }
        , likeCountLabel likeCount
        ]


likeCountLabel : Int -> Element Msg
likeCountLabel likeCount =
    el
        [ Font.size 16
        , Font.family [ Font.monospace ]
        , centerY
        ]
    <|
        text <|
            rightPad (String.fromInt likeCount) " " 3


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


rightPad : String -> String -> Int -> String
rightPad s padder to =
    if String.length s < to then
        rightPad (s ++ padder) padder to

    else
        s


memberWithMaybe : Maybe a -> List a -> Bool -> Bool
memberWithMaybe e l default =
    case e of
        Just e_ ->
            List.member e_ l

        Nothing ->
            default


view : Model -> Maybe User -> Element Msg
view data maybeUser =
    let
        maybeCompose =
            case ( maybeUser, data.section ) of
                ( Just user, Hottest ) ->
                    composeView user data.compose

                _ ->
                    Element.none

        maybeFeed =
            case data.section of
                Hottest ->
                    feed data.cards maybeUser

                Coldest ->
                    noColdTakes
    in
    column
        [ spacing 24
        , centerX
        ]
        [ homeNavTabs data.section
        , maybeCompose
        , maybeFeed
        ]


composeView : User -> Compose -> Element Msg
composeView user compose =
    column
        [ width fill
        , spacing 12
        ]
        [ Input.multiline
            []
            { onChange = EditNewTake
            , text = compose.content
            , placeholder = Just <| Input.placeholder [] (text ("Hi " ++ user.username ++ ". What's your hottest take?"))
            , label = Input.labelHidden "What's your hottest take?"
            , spellcheck = False
            }
        , publishButton user
        , composeMessage compose.state
        ]


composeMessage : ComposeState -> Element Msg
composeMessage state =
    case state of
        Composing ->
            Element.none

        Posting ->
            postingView

        FailedToPost ->
            failedView "Failed to post."


publishButton : User -> Element Msg
publishButton user =
    Input.button
        [ padding 12
        , Border.rounded 7
        , clip
        , Background.color Colors.primary
        , Font.color Colors.textOnPrimary
        , alignRight
        ]
        { onPress = Just <| PublishNewTakeClick
        , label = text "Publish"
        }


homeNavTabs : FeedSection -> Element Msg
homeNavTabs section =
    row
        [ alignLeft
        , alignTop
        , Border.widthEach { top = 0, bottom = 2, left = 0, right = 0 }
        , width fill
        , Border.color Colors.secondary
        ]
        [ navTab "Hottest" "#hottest" (section == Hottest)
        , navTab "Coldest" "#coldest" (section == Coldest)
        ]


noColdTakes : Element Msg
noColdTakes =
    paragraph
        [ Font.size 24
        , padding 36
        , width (px feedWidth)
        ]
        [ text <| "Just kidding! We don't have any cold takes here." ]


feed : List TakeCard -> Maybe User -> Element Msg
feed takes user =
    column
        [ spacing 12 ]
        (List.map (\t -> viewTake t user) takes)
