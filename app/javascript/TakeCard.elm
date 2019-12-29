module TakeCard exposing (Msg, TakeCard, createNewTake, likeOrUnlike, toggleHover, update, viewTake)

import Data.Take as Take exposing (Take)
import Data.User as User exposing (User)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onMouseEnter, onMouseLeave)
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


update : Msg -> List TakeCard -> User -> List TakeCard
update msg cards user =
    case msg of
        FireButtonPressed card ->
            findAndApply card (likeOrUnlike user) cards

        TakeHovered card ->
            findAndApply card toggleHover cards

        _ ->
            cards



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



-- VIEW


viewTake : TakeCard -> Time.Zone -> Maybe User -> Html Msg
viewTake card zone user =
    div
        [ class "media border border-warning p-3"
        , onMouseEnter <| TakeHovered card
        , onMouseLeave <| TakeHovered card
        ]
        [ img [ class "mr-2", width 64, height 64, src "assets/profilepic.jpg" ] []
        , div [ class "media-body pr-3" ]
            ([ p [ class "mb-0" ] [ text ("\"" ++ card.take.content ++ "\"") ]
             , p [ class "text-right" ] [ text <| "- @" ++ card.take.postedBy.username ]
             ]
                ++ hoverButtons card user
            )
        , fireButton card user card.take.usersWhoLiked
        ]


hoverButtons : TakeCard -> Maybe User -> List (Html Msg)
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
        [ div
            [ class "text-center" ]
            buttons
        ]

    else
        []


takeHoverButton : String -> Msg -> Html Msg
takeHoverButton txt msg =
    button [ class "btn-link", onClick msg ] [ text txt ]


fireButton : TakeCard -> Maybe User -> List User -> Html Msg
fireButton card maybeUser likers =
    case maybeUser of
        Just user ->
            if List.member user likers then
                button
                    [ class "align-self-end align-self-center fire-button"
                    , onClick (FireButtonPressed card)
                    ]
                    [ text <| String.fromInt <| List.length likers ]

            else
                button
                    [ class "align-self-end align-self-center fire-button-transparent"
                    , onClick (FireButtonPressed card)
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
