module Birthday exposing (handleInput, validate)

import Field exposing (Field, composeValidateField, notBlank)


handleInput : Field String -> String -> Field String
handleInput prev new =
    if String.length prev.value < String.length new then
        if String.length new == 1 then
            if new == "0" || new == "1" then
                { prev | value = new, error = Nothing }

            else
                { prev | value = "0" ++ new ++ "/", error = Nothing }

        else if String.length new == 2 then
            case String.toInt new of
                Just _ ->
                    { prev | value = new ++ "/", error = Nothing }

                Nothing ->
                    { prev | value = new, error = Nothing }

        else if String.right 2 new == "//" then
            { prev | value = String.dropRight 1 new, error = Nothing }

        else if String.length new == 5 then
            case String.toInt <| String.right 2 new of
                Just _ ->
                    { prev | value = new ++ "/", error = Nothing }

                Nothing ->
                    if
                        (String.right 1 new == "/")
                            && (String.toInt (String.slice 3 4 new) /= Nothing)
                    then
                        { prev | value = String.slice 0 3 new ++ "0" ++ String.slice 3 4 new ++ "/", error = Nothing }

                    else
                        { prev | value = new, error = Nothing }

        else
            { prev | value = new, error = Nothing }

    else
        { prev | value = new, error = Nothing }


validate : Field String -> ( Field String, Bool )
validate =
    composeValidateField
        (composeValidateField notBlank checkLength)
        checkParts


checkLength : Field String -> ( Field String, Bool )
checkLength bday =
    case compare (String.length bday.value) 10 of
        LT ->
            ( { bday | error = Just "You're missing something" }
            , False
            )

        EQ ->
            ( bday, True )

        GT ->
            ( { bday | error = Just "Too many characters" }
            , False
            )


checkParts : Field String -> ( Field String, Bool )
checkParts bday =
    let
        parts =
            String.split "/" bday.value
    in
    case parts of
        [ month, day, year ] ->
            let
                ( err, valid ) =
                    validateParts month day year
            in
            ( { bday | error = err }, valid )

        _ ->
            ( { bday | error = Just "That's not the right format" }
            , False
            )


validateParts : String -> String -> String -> ( Maybe String, Bool )
validateParts month day year =
    let
        ( monthErr, monthValid, monthVal ) =
            validateMonth month
    in
    if monthValid then
        let
            ( dayErr, dayValid, dayVal ) =
                validateDay monthVal day
        in
        if dayValid then
            validateYear monthVal dayVal year

        else
            ( dayErr, False )

    else
        ( monthErr, False )


validateDay : Int -> String -> ( Maybe String, Bool, Int )
validateDay month day =
    let
        maxDays =
            maxDaysInMonth month
    in
    composeValidatePart
        (checkPartLength "day" 2)
        (inRange "Days should be positive" "There aren't that many days in that month" 1 maxDays)
        day


maxDaysInMonth : Int -> Int
maxDaysInMonth m =
    case m of
        1 ->
            31

        2 ->
            29

        3 ->
            31

        4 ->
            30

        5 ->
            31

        6 ->
            30

        7 ->
            31

        8 ->
            31

        9 ->
            30

        10 ->
            31

        11 ->
            30

        12 ->
            31

        _ ->
            31


validateYear : Int -> Int -> String -> ( Maybe String, Bool )
validateYear month day year =
    let
        ( err, valid, yearVal ) =
            composeValidatePart (checkPartLength "year" 4)
                (inRange "We don't like people that old" "You're too young for these takes" 1901 2004)
                year
    in
    if valid then
        case ( month, day, modBy 4 yearVal ) of
            ( 2, 29, 0 ) ->
                ( Nothing, True )

            ( 2, 29, _ ) ->
                ( Just "Oops. That wasn't a leap year", False )

            _ ->
                ( Nothing, True )

    else
        ( err, False )


validateMonth : String -> ( Maybe String, Bool, Int )
validateMonth =
    composeValidatePart
        (checkPartLength "month" 2)
        (inRange "Months should be postive" "There are only 12 months, for now..." 1 12)


checkPartLength : String -> Int -> String -> ( Maybe String, Bool, Int )
checkPartLength partName l part =
    case compare (String.length part) l of
        LT ->
            ( Just <| "Missing some characters for the " ++ partName
            , False
            , 0
            )

        GT ->
            ( Just <| "Your " ++ partName ++ " is too long"
            , False
            , 0
            )

        EQ ->
            ( Nothing, True, 0 )


inRange : String -> String -> Int -> Int -> String -> ( Maybe String, Bool, Int )
inRange tooSmallErr tooBigErr min max part =
    case String.toInt part of
        Just val ->
            case ( compare val min, compare val max ) of
                ( LT, _ ) ->
                    ( Just tooSmallErr
                    , False
                    , 0
                    )

                ( _, GT ) ->
                    ( Just tooBigErr
                    , False
                    , 0
                    )

                _ ->
                    ( Nothing, True, val )

        Nothing ->
            ( Just <| "Please use numbers", False, 0 )


composeValidatePart : (String -> ( Maybe String, Bool, Int )) -> (String -> ( Maybe String, Bool, Int )) -> String -> ( Maybe String, Bool, Int )
composeValidatePart v1 v2 part =
    let
        ( f2, valid, _ ) =
            v1 part
    in
    if valid then
        v2 part

    else
        ( f2, False, 0 )
