module DeleteAccount exposing (Model, Msg, init, update, view)

import Api
import Colors
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http



-- MODEL


type alias Model =
    { state : State }


type State
    = Viewing
    | Submitting
    | Done
    | Error


init : Model
init =
    { state = Viewing }



-- UPDATE


type Msg
    = Submit
    | Submitted (Result Http.Error ())


update : Msg -> Model -> Maybe { a | auth : Api.UserAuth } -> ( Model, Cmd Msg )
update msg model maybeAuth =
    case ( msg, maybeAuth ) of
        ( Submit, Just { auth } ) ->
            ( { model | state = Submitting }
            , Api.deleteAccount auth Submitted
            )

        ( Submitted (Ok _), _ ) ->
            ( { model | state = Done }
            , Cmd.none
            )

        ( Submitted (Err _), _ ) ->
            ( { model | state = Error }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Element Msg
view model =
    case model.state of
        Viewing ->
            info

        Submitting ->
            submitting

        Done ->
            done

        Error ->
            error


info : Element Msg
info =
    column
        [ spacing 12
        , padding 15
        ]
        [ textColumn [ spacing 10, width <| maximum 500 fill ]
            [ paragraph []
                [ text <|
                    "You are trying to delete your account. "
                        ++ "This will cause you (and us) terrible pain, "
                        ++ "loss of advertising income, and/or happiness. "
                        ++ " This cannot be undone."
                ]
            , paragraph []
                [ text <|
                    "The button below will cause your takes, likes, "
                        ++ "bio, profile picture, and joy to be "
                        ++ "permanently deleted in an absolutely unrecoverable "
                        ++ "manner. Please keep in mind that such a deletion "
                        ++ "might be computationally expensive and could take "
                        ++ "several units of time."
                ]
            , paragraph []
                [ text <|
                    "Please also note that deleting your account has "
                        ++ "little effect on copies of your takes that other "
                        ++ "people may have screenshotted and stored. "
                        ++ "It also may cause your Thursday's to be slightly "
                        ++ "cooler than usual. Consider purchasing an extra "
                        ++ "cardigan."
                ]
            ]
        , Input.button
            [ Border.width 1
            , Border.color Colors.secondary
            , padding 10
            , Border.rounded 7
            , Font.color Colors.primary
            ]
            { onPress = Just Submit
            , label = text "Goodbye :("
            }
        ]


submitting : Element msg
submitting =
    el
        [ padding 15 ]
    <|
        paragraph []
            [ text "Here we go..." ]


done : Element msg
done =
    el
        [ padding 15 ]
    <|
        paragraph []
            [ text "You no longer exist. Kindly logout to complete your demise." ]


error : Element msg
error =
    el
        [ padding 15 ]
    <|
        paragraph []
            [ text "Well, that didn't work. Refresh the page and try again I guess." ]
