module Feed exposing (FeedSection, Model, Msg, addOrUpdateTake, addTakes, feed, feedWidth, fromTakes, init, smallFeed, smallView, toFeedSection, update, view)

import Api
import Colors exposing (ColorScheme)
import Data.Take exposing (Take)
import Data.User exposing (User)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Http
import HttpUtils exposing (httpErrorToString)
import NavTabs exposing (navTab)
import Ports
import Thursday exposing (toWeekdayString)
import Time
import Time.Distance exposing (inWords)



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


maxCharacterCount =
    169


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
    | PostingError String



-- UPDATE


type Msg
    = FireButtonPressed TakeCard
    | TakeFocused TakeCard
    | EditTake TakeCard
    | DeleteTake TakeCard
    | LikeHandled (Result Http.Error ())
    | DeleteHandled Int (Result Http.Error ())
    | EditNewTake String
    | PublishNewTake
    | TakePublished (Result Http.Error ())


update : Msg -> Model -> User -> Api.UserAuth -> ( Model, Cmd Msg )
update msg model user auth =
    case msg of
        EditNewTake newTake ->
            ( { model | compose = updateContent model.compose newTake }
            , Cmd.none
            )

        PublishNewTake ->
            let
                ( compose, valid ) =
                    validateCompose model.compose
            in
            if valid then
                ( { model | compose = compose }
                , Api.makeTake auth model.compose.content TakePublished
                )

            else
                ( { model | compose = compose }
                , Cmd.none
                )

        TakePublished (Err m) ->
            ( { model
                | compose = toComposeState model.compose (PostingError "Failed to post. Have you tried plugging it in?")
              }
            , Cmd.none
            )

        TakePublished (Ok _) ->
            ( { model
                | compose = { state = Composing, content = "" }
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
            ( model
            , Ports.info <| "like handled error: " ++ httpErrorToString m
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


validateCompose : Compose -> ( Compose, Bool )
validateCompose compose =
    if String.length compose.content <= maxCharacterCount then
        ( toComposeState compose Posting, True )

    else
        ( toComposeState compose (PostingError "Too long. Brevity is the soul of hotness"), False )


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


addOrUpdateTake : Model -> Take -> Model
addOrUpdateTake model take =
    case replaceTake model.cards take of
        Just newCards ->
            { model | cards = newCards }

        Nothing ->
            { model | cards = { take = take, state = Default } :: model.cards }


addTakes : Model -> List Take -> Model
addTakes model takes =
    let
        oldTakeIds =
            List.map (\c -> c.take.id) model.cards

        newTakes =
            List.filter (\take -> not <| List.member take.id oldTakeIds) takes
    in
    { model | cards = fromTakes newTakes ++ model.cards }


replaceTake : List TakeCard -> Take -> Maybe (List TakeCard)
replaceTake cards take =
    let
        takeIds =
            List.map (\c -> c.take.id) cards
    in
    if List.member take.id takeIds then
        Just <|
            List.map
                (\c ->
                    if c.take.id == take.id then
                        { take = take, state = c.state }

                    else
                        c
                )
                cards

    else
        Nothing


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
    3
        * cardSpacing
        + 2
        * cardPadding
        + takeWidth
        + thumbnailWidth
        + fireAndLikeCountWidth


cardSpacing =
    12


cardPadding =
    6


takeWidth =
    490


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


fireAndLikeCountWidth =
    2 * fireAndLikeCountPadding + fireAndLikeCountSpacing + fireButtonWidth + estLikeCountWidth


fireAndLikeCountPadding =
    5


fireAndLikeCountSpacing =
    3


estLikeCountWidth =
    30


viewTake : ColorScheme -> TakeCard -> Maybe User -> Time.Posix -> Element Msg
viewTake colorScheme card user now =
    case card.state of
        Default ->
            defaultView colorScheme card user now

        Focused ->
            focusedView colorScheme card user now

        Deleting ->
            deletingView

        FailedToDelete ->
            column [ spacing 24 ]
                [ failedView "Failed to delete."
                , focusedView colorScheme card user now
                ]


defaultView : ColorScheme -> TakeCard -> Maybe User -> Time.Posix -> Element Msg
defaultView colorScheme card user now =
    Input.button
        [ Border.width 1
        , Border.rounded 7
        , Border.color colorScheme.secondary
        ]
        { onPress = Just <| TakeFocused card
        , label = takeCardContents colorScheme card user False now
        }


focusedView : ColorScheme -> TakeCard -> Maybe User -> Time.Posix -> Element Msg
focusedView colorScheme card user now =
    Input.button
        [ Border.width 1
        , Border.rounded 7
        , Border.color colorScheme.secondary
        ]
        { onPress = Just <| TakeFocused card
        , label = takeCardContents colorScheme card user True now
        }


postingView : Element Msg
postingView =
    text "Posting the take..."


failedView : String -> Element Msg
failedView error =
    text <| error


deletingView : Element Msg
deletingView =
    text "Deleting the take..."


takeCardContents : ColorScheme -> TakeCard -> Maybe User -> Bool -> Time.Posix -> Element Msg
takeCardContents colorScheme card user focused now =
    column
        [ spacing cardSpacing
        , padding cardPadding
        ]
        [ row
            [ spacing cardSpacing
            , padding cardPadding
            ]
            [ profilePicThumbnail card
            , el [ width (px takeWidth) ] <| takeAndAuthor card.take now
            , fireButton colorScheme card user card.take.usersWhoLiked
            ]
        , if focused then
            focusButtons colorScheme card user

          else
            none
        ]


takeAndAuthor : Take -> Time.Posix -> Element Msg
takeAndAuthor take now =
    textColumn
        [ spacing 12
        , paddingEach { left = 5, right = 30, top = 5, bottom = 5 }
        , width (px takeWidth)
        , Html.Attributes.style "overflow-wrap" "break-word" |> htmlAttribute
        , alignLeft
        ]
        [ paragraph [] [ text <| "\"" ++ take.content ++ "\"" ]
        , el [ alignRight ] <|
            link [ Font.alignRight ]
                { url = "/profile?uid=" ++ String.fromInt take.postedBy.id
                , label = text <| "- @" ++ take.postedBy.username ++ " (" ++ inWords take.timePosted now ++ ")"
                }
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


focusButtons : ColorScheme -> TakeCard -> Maybe User -> Element Msg
focusButtons colorScheme card user =
    let
        buttons =
            if Just card.take.postedBy == user then
                [ takeFocusButton colorScheme "delete" (DeleteTake card) ]

            else
                []
    in
    row [ centerX ] buttons


takeFocusButton : ColorScheme -> String -> Msg -> Element Msg
takeFocusButton colorScheme txt msg =
    Input.button [ Font.color colorScheme.link ]
        { onPress = Just msg, label = text txt }


fireButton : ColorScheme -> TakeCard -> Maybe User -> List User -> Element Msg
fireButton colorScheme card maybeUser likers =
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
        [ Border.color colorScheme.secondaryLight
        , Border.width 1
        , Border.rounded 7
        ]
        { onPress = onPress
        , label = fireAndLikeCount url likeCount
        }


fireAndLikeCount : String -> Int -> Element Msg
fireAndLikeCount url likeCount =
    row
        [ padding fireAndLikeCountPadding
        , spacing fireAndLikeCountSpacing
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


view : Model -> ColorScheme -> Maybe User -> Time.Posix -> Element Msg
view data colorScheme maybeUser now =
    let
        maybeCompose =
            case ( maybeUser, data.section ) of
                ( Just user, Hottest ) ->
                    composeView colorScheme user data.compose

                _ ->
                    Element.none

        maybeFeed =
            case data.section of
                Hottest ->
                    feed colorScheme data.cards maybeUser now

                Coldest ->
                    noColdTakes [ width <| px feedWidth ]
    in
    column
        [ spacing 24
        , centerX
        , width (px feedWidth)
        ]
        [ homeNavTabs colorScheme data.section
        , maybeCompose
        , maybeFeed
        ]


composeView : ColorScheme -> User -> Compose -> Element Msg
composeView colorScheme user compose =
    column
        [ width fill
        , spacing 12
        ]
        [ Input.multiline
            [ width (fill |> maximum feedWidth)
            , clipX
            , height (fill |> minimum 100)
            ]
            { onChange = EditNewTake
            , text = compose.content
            , placeholder = Just <| Input.placeholder [] (text ("Hi " ++ user.username ++ ". What's your hottest take?"))
            , label = Input.labelHidden "What's your hottest take?"
            , spellcheck = False
            }
        , row [ spacing 12, width fill ]
            [ characterCount colorScheme <| String.length compose.content
            , publishButton colorScheme user
            ]
        , composeMessage compose.state
        ]


characterCount : ColorScheme -> Int -> Element Msg
characterCount colorScheme count =
    let
        fontColor =
            if count <= maxCharacterCount then
                colorScheme.black

            else
                colorScheme.primary
    in
    el
        [ alignLeft
        , Font.color fontColor
        ]
    <|
        text (String.fromInt count ++ "/" ++ String.fromInt maxCharacterCount)


composeMessage : ComposeState -> Element Msg
composeMessage state =
    case state of
        Composing ->
            Element.none

        Posting ->
            postingView

        PostingError m ->
            failedView m


publishButton : ColorScheme -> User -> Element Msg
publishButton colorScheme user =
    Input.button
        [ padding 12
        , Border.rounded 7
        , clip
        , Background.color colorScheme.primary
        , Font.color colorScheme.textOnPrimary
        , alignRight
        ]
        { onPress = Just <| PublishNewTake
        , label = text "Publish"
        }


homeNavTabs : ColorScheme -> FeedSection -> Element Msg
homeNavTabs colorScheme section =
    row
        [ alignLeft
        , alignTop
        , Border.widthEach { top = 0, bottom = 2, left = 0, right = 0 }
        , width fill
        , Border.color colorScheme.secondary
        ]
        [ navTab colorScheme "Hottest" "#hottest" (section == Hottest)
        , navTab colorScheme "Coldest" "#coldest" (section == Coldest)
        ]


noColdTakes : List (Attribute Msg) -> Element Msg
noColdTakes attributes =
    paragraph
        ([ Font.size 24
         , padding 36
         ]
            ++ attributes
        )
        [ text <| "Just kidding! We don't have any cold takes here." ]


feed : ColorScheme -> List TakeCard -> Maybe User -> Time.Posix -> Element Msg
feed colorScheme takes user now =
    column
        [ spacing 12 ]
        (List.map (\t -> viewTake colorScheme t user now) <|
            List.reverse takes
        )



-- SMALL VIEW


smallView : Model -> ColorScheme -> Maybe User -> Time.Posix -> Element Msg
smallView data colorScheme maybeUser now =
    let
        maybeCompose =
            case ( maybeUser, data.section ) of
                ( Just user, Hottest ) ->
                    composeView colorScheme user data.compose

                _ ->
                    Element.none

        maybeFeed =
            case data.section of
                Hottest ->
                    smallFeed colorScheme data.cards maybeUser now

                Coldest ->
                    noColdTakes []
    in
    column
        [ spacing 24
        , centerX
        , width fill
        ]
        [ homeNavTabs colorScheme data.section
        , maybeCompose
        , maybeFeed
        ]


smallFeed : ColorScheme -> List TakeCard -> Maybe User -> Time.Posix -> Element Msg
smallFeed colorScheme takes user now =
    column
        [ spacing 6, width fill ]
        (List.map (\t -> smallViewTake colorScheme t user now) <|
            List.reverse takes
        )


smallViewTake : ColorScheme -> TakeCard -> Maybe User -> Time.Posix -> Element Msg
smallViewTake colorScheme card user now =
    case card.state of
        Default ->
            smallDefaultView colorScheme card user now

        Focused ->
            smallFocusedView colorScheme card user now

        Deleting ->
            deletingView

        FailedToDelete ->
            column [ spacing 24 ]
                [ failedView "Failed to delete."
                , smallFocusedView colorScheme card user now
                ]


smallDefaultView : ColorScheme -> TakeCard -> Maybe User -> Time.Posix -> Element Msg
smallDefaultView colorScheme card user now =
    Input.button
        [ Border.width 1
        , Border.rounded 7
        , Border.color colorScheme.secondary
        , width fill
        ]
        { onPress = Just <| TakeFocused card
        , label = smallTakeCardContents colorScheme card user False now
        }


smallFocusedView : ColorScheme -> TakeCard -> Maybe User -> Time.Posix -> Element Msg
smallFocusedView colorScheme card user now =
    Input.button
        [ Border.width 1
        , Border.rounded 7
        , Border.color colorScheme.secondary
        , width fill
        ]
        { onPress = Just <| TakeFocused card
        , label = smallTakeCardContents colorScheme card user True now
        }


smallTakeCardContents : ColorScheme -> TakeCard -> Maybe User -> Bool -> Time.Posix -> Element Msg
smallTakeCardContents colorScheme card user focused now =
    column
        [ spacing cardSpacing
        , padding cardPadding
        , width fill
        ]
        [ row
            [ spacing cardSpacing
            , padding cardPadding
            , width fill
            ]
            [ el [ width fill ] <| smallTakeAndAuthor card.take now
            , smallFireButton colorScheme card user card.take.usersWhoLiked
            ]
        , if focused then
            focusButtons colorScheme card user

          else
            none
        ]


smallTakeAndAuthor : Take -> Time.Posix -> Element Msg
smallTakeAndAuthor take now =
    textColumn
        [ spacing 12
        , paddingEach { left = 5, right = 10, top = 5, bottom = 5 }
        , width fill
        , Html.Attributes.style "word-break" "break-all" |> htmlAttribute
        , alignLeft
        , Font.size 14
        ]
        [ paragraph [] [ text <| "\"" ++ take.content ++ "\"" ]
        , el [ alignRight ] <|
            link [ Font.alignRight ]
                { url = "/profile?uid=" ++ String.fromInt take.postedBy.id
                , label = text <| "- @" ++ take.postedBy.username ++ " (" ++ inWords take.timePosted now ++ ")"
                }
        ]


smallFireButton : ColorScheme -> TakeCard -> Maybe User -> List User -> Element Msg
smallFireButton colorScheme card maybeUser likers =
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
        [ Border.color colorScheme.secondaryLight
        , Border.width 1
        , Border.rounded 7
        , alignRight
        ]
        { onPress = onPress
        , label = smallFireAndLikeCount url likeCount
        }


smallFireAndLikeCount : String -> Int -> Element Msg
smallFireAndLikeCount url likeCount =
    column
        [ padding fireAndLikeCountPadding
        , spacing fireAndLikeCountSpacing
        , centerX
        ]
        [ image [ width (px 30) ] { src = url, description = "A fire emoji" }
        , el
            [ centerX
            , Font.size 16
            , Font.family [ Font.monospace ]
            ]
          <|
            text (String.fromInt likeCount)
        ]
