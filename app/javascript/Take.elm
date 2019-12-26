module Take exposing (Msg, Take, createNewTake, likeOrUnlike, toggleHover, update, viewTake)

import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Thursday exposing (toWeekdayString)
import Time



-- MODEL


type alias Take =
    { content : String
    , postedBy : User
    , timePosted : Time.Posix
    , likedBy : List User
    , hoveredOver : Bool
    }



-- UPDATE


type Msg
    = FireButtonPressed Take
    | TakeHovered Take
    | EditTake Take
    | DeleteTake Take
    | ReportTake Take


update : Msg -> List Take -> User -> List Take
update msg takes user =
    case msg of
        FireButtonPressed take ->
            findAndApply take (likeOrUnlike user) takes

        TakeHovered take ->
            findAndApply take toggleHover takes

        _ ->
            takes



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


createNewTake : String -> User -> Time.Posix -> Take
createNewTake newTake user time =
    { content = newTake
    , postedBy = user
    , timePosted = time
    , likedBy = []
    , hoveredOver = False
    }


toggleHover : Take -> Take
toggleHover t =
    { t | hoveredOver = not t.hoveredOver }


likeOrUnlike : User -> Take -> Take
likeOrUnlike user take =
    if List.member user take.likedBy then
        { take | likedBy = List.filter (\u -> u /= user) take.likedBy }

    else
        { take | likedBy = user :: take.likedBy }



-- VIEW


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
                [ takeHoverButton "edit" (EditTake take)
                , text " | "
                , takeHoverButton "delete" (DeleteTake take)
                ]

            else
                [ takeHoverButton "report" (ReportTake take) ]
    in
    if take.hoveredOver then
        [ div
            [ class "text-center" ]
            buttons
        ]

    else
        []


takeHoverButton : String -> Msg -> Html Msg
takeHoverButton txt msg =
    button [ class "btn-link", onClick msg ] [ text txt ]


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
