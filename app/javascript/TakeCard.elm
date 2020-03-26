module TakeCard exposing (Msg, TakeCard, createNewTake, likeOrUnlike, toggleHover, update, viewTake)

import Api
import Data.Take as Take exposing (Take)
import Data.User as User exposing (User)
import Debug
import Element exposing (..)
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


viewTake : TakeCard -> Time.Zone -> Maybe User -> Element Msg
viewTake card zone user =
    column
        []
        [ image
            [ width (px 64), height (px 64) ]
            { src = Maybe.withDefault "/assets/profilepic.jpg" card.take.postedBy.avatarUrl, description = "User's profile picture" }
        , column []
            ([ el [] (text <| ("\"" ++ card.take.content ++ "\""))
             , el [] (text <| "- @" ++ card.take.postedBy.username)
             ]
                ++ hoverButtons card user
            )
        , fireButton card user card.take.usersWhoLiked
        ]


hoverButtons : TakeCard -> Maybe User -> List (Element Msg)
hoverButtons card user =
    let
        buttons =
            if Just card.take.postedBy == user then
                [ takeHoverButton "edit" (EditTake card)
                , text " | "
                , takeHoverButton "delete" (DeleteTake card)
                ]

            else
                [ takeHoverButton "report" (ReportTake card) ]
    in
    if card.hovered then
        [ column
            []
            buttons
        ]

    else
        []


takeHoverButton : String -> Msg -> Element Msg
takeHoverButton txt msg =
    Input.button []
        { onPress = Just msg, label = text txt }


fireButton : TakeCard -> Maybe User -> List User -> Element Msg
fireButton card maybeUser likers =
    case maybeUser of
        Just user ->
            if List.member user likers then
                Input.button []
                    { onPress = Just <| FireButtonPressed card
                    , label = text <| String.fromInt <| List.length likers
                    }

            else
                Input.button []
                    { onPress = Just <| FireButtonPressed card
                    , label = text <| String.fromInt <| List.length likers
                    }

        Nothing ->
            Input.button
                []
                { onPress = Nothing
                , label = text <| String.fromInt <| List.length likers
                }


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
