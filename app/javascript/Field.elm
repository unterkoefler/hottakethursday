module Field exposing (Field, addErrorFromApi, composeValidateField, notBlank, updateValue, validate, view, viewError)

import Colors exposing (ColorScheme)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Font as Font



-- MODEL


type alias Field a =
    { value : a
    , error : Maybe String
    }



-- UPDATE


updateValue : a -> Field a -> Field a
updateValue val field =
    { field | value = val, error = Nothing }


addErrorFromApi : String -> Field a -> Dict String String -> Field a
addErrorFromApi key field errors =
    let
        e =
            Dict.get key errors
    in
    { field | error = e }



-- VALIDATE


validate : (Field a -> Bool) -> String -> Field a -> ( Field a, Bool )
validate check msg field =
    if check field then
        ( field, True )

    else
        ( { field | error = Just msg }
        , False
        )


composeValidateField v1 v2 field =
    let
        ( f2, valid ) =
            v1 field
    in
    if valid then
        v2 field

    else
        ( f2, False )


notBlank : Field String -> ( Field String, Bool )
notBlank field =
    if field.value == "" then
        ( { field | error = Just "This field is required" }
        , False
        )

    else
        ( field, True )



-- VIEW


view : ColorScheme -> (a -> Element msg) -> Field a -> Element msg
view colorScheme viewInput field =
    let
        input =
            viewInput field.value

        error =
            viewError colorScheme field.error
    in
    column [ spacing 5 ]
        [ input, error ]


viewError : ColorScheme -> Maybe String -> Element msg
viewError colorScheme e =
    case e of
        Nothing ->
            Element.none

        Just m ->
            paragraph [ Font.color colorScheme.primary ]
                [ text m ]
