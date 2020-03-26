module Signup exposing (Model, Msg, update, view)

import Element exposing (..)
import Element.Input as Input
import Element.Region as Region



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


view : Model -> Element Msg
view model =
    column
        []
        [ el [ Region.heading 2 ] (text "Create Account")
        , row [] [ text "Feed us your data" ]
        , inputWithLabel "Name" model.name EditName
        , inputWithLabel "Username" model.username EditUsername
        , inputWithLabel "Email" model.email EditEmail
        , inputWithLabel "Birthday (MM/DD/YYY)" model.birthday EditBirthday
        , Input.button [] { onPress = Just Submit, label = text "Begin" }
        ]


inputWithLabel : String -> String -> (String -> Msg) -> Element Msg
inputWithLabel lbl val msg =
    Input.text
        []
        { onChange = msg
        , text = val
        , placeholder = Nothing
        , label = Input.labelAbove [] (text lbl)
        }
