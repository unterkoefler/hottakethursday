module TakeCard exposing (Msg, TakeCard, createNewTake, likeOrUnlike, minCardWidth, toggleHover, update, viewTake)

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
import Thursday exposing (toWeekdayString)
import Time



-- MODEL


type alias TakeCard =
    { take : Take
    , hovered : Bool
    }



-- UPDATE


type Msg
    = FireButtonPressed TakeCard
    | TakeHovered TakeCard
    | EditTake TakeCard
    | DeleteTake TakeCard
    | ReportTake TakeCard
    | LikeHandled (Result Http.Error ())


update : Msg -> List TakeCard -> User -> Api.UserAuth -> ( List TakeCard, Cmd Msg )
update msg cards user auth =
    case msg of
        FireButtonPressed card ->
            ( findAndApply card (likeOrUnlike user) cards
            , sendLikeOrUnlike user auth card.take
            )

        TakeHovered card ->
            ( findAndApply card toggleHover cards
            , Cmd.none
            )

        LikeHandled (Err m) ->
            let
                _ =
                    Debug.log "like handled error" m
            in
            ( cards
            , Cmd.none
            )

        _ ->
            ( cards
            , Cmd.none
            )



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


createNewTake : String -> User -> Time.Posix -> TakeCard
createNewTake newTake user time =
    let
        take =
            { id = 0
            , content = newTake
            , postedBy = user
            , timePosted = time
            , usersWhoLiked = []
            }
    in
    { take = take
    , hovered = False
    }


toggleHover : TakeCard -> TakeCard
toggleHover t =
    { t | hovered = not t.hovered }


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


minCardWidth =
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


viewTake : TakeCard -> Time.Zone -> Maybe User -> Element Msg
viewTake card zone user =
    let
        borderWidth =
            if card.hovered then
                2 * cardBorderWidth

            else
                cardBorderWidth
    in
    Input.button
        [ Border.width borderWidth
        , Border.rounded 7
        , Border.color Colors.secondary
        ]
        { onPress = Just <| TakeHovered card
        , label = takeCardContents card zone user
        }


takeCardContents : TakeCard -> Time.Zone -> Maybe User -> Element Msg
takeCardContents card zone user =
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
        , hoverButtons card user
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


hoverButtons : TakeCard -> Maybe User -> Element Msg
hoverButtons card user =
    let
        buttons =
            if Just card.take.postedBy == user then
                [ takeHoverButton "delete" (DeleteTake card) ]

            else
                [ takeHoverButton "report" (ReportTake card) ]
    in
    if card.hovered then
        row [ centerX ] buttons

    else
        none


takeHoverButton : String -> Msg -> Element Msg
takeHoverButton txt msg =
    Input.button []
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
