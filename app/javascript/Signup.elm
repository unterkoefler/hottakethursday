module Signup exposing (Model, Msg, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MODEL


type alias Model =
    { name : String
    , username : String
    , email : String
    , birthday : String
    }



-- UPDATE


type Msg
    = Submit
    | EditName String
    | EditUsername String
    | EditEmail String
    | EditBirthday String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Submit ->
            if validateSignup model then
                model
                -- TODO: Fix this so that a sign up actually does something

            else
                model

        EditName newName ->
            { model | name = newName }

        EditUsername newUsername ->
            { model | username = newUsername }

        EditEmail newEmail ->
            { model | email = newEmail }

        EditBirthday newBday ->
            { model | birthday = handleBirthdayInput model.birthday newBday }


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


validateSignup : Model -> Bool
validateSignup model =
    not (String.isEmpty model.name) && not (String.isEmpty model.username)



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "signupBody", class "container-fluid" ]
        [ div [ class "row justify-content-center" ]
            [ div [ class "col form" ]
                ([ h2 [] [ text "Create Account" ]
                 , p [] [ text "Feed us your data" ]
                 ]
                    ++ inputWithLabel "name" "Name" model.name EditName
                    ++ inputWithLabel "username" "Username" model.username EditUsername
                    ++ inputWithLabel "email" "Email" model.email EditEmail
                    ++ inputWithLabel "bday" "Birthday (MM/DD/YYYY)" model.birthday EditBirthday
                    ++ [ div []
                            [ button
                                [ onClick Submit
                                , disabled <| not <| validateSignup model
                                ]
                                [ text "Begin" ]
                            ]
                       ]
                )
            ]
        ]


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
