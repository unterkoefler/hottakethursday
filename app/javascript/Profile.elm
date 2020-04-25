module Profile exposing (Model, Msg, Section, toModel, toSection, update, view)

import Api
import AssocList as Dict exposing (Dict)
import Colors
import Data.User exposing (User)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes
import Http
import NavTabs exposing (navTab)



-- MODEL


type alias Model =
    { items : Dict String BioItem
    , subject : User
    , section : Section
    }


type alias BioItem =
    { value : String
    , oldValue : Maybe String
    , state : EditingState
    , error : Maybe String
    }


newBioItem : String -> BioItem
newBioItem val =
    { value = val
    , oldValue = Nothing
    , state = Viewing
    , error = Nothing
    }


type Section
    = YourTakes
    | Notifications
    | Settings


type EditingState
    = Viewing
    | Editing
    | Saving


toModel : Section -> User -> Model
toModel section subject =
    { items = itemsFromUser subject
    , subject = subject
    , section = section
    }


itemsFromUser : User -> Dict BioItemKey BioItem
itemsFromUser subject =
    Dict.fromList
        [ ( "Least favorite color", newBioItem subject.leastFavoriteColor )
        , ( "Bio", newBioItem subject.bio )
        , ( "Name", newBioItem subject.name )
        ]



-- UPDATE


type alias BioItemKey =
    String


type Msg
    = BeginEditingItem BioItemKey
    | EditItem BioItemKey String
    | SaveItem BioItemKey
    | CancelEditingItem BioItemKey
    | ItemSaved BioItemKey (Result Http.Error User)


toSection : Maybe String -> Section
toSection frag =
    case frag of
        Just "notifications" ->
            Notifications

        Just "settings" ->
            Settings

        Just str ->
            YourTakes

        Nothing ->
            YourTakes


update : Msg -> Model -> Api.UserAuth -> ( Model, Cmd Msg )
update msg model auth =
    case msg of
        EditItem key newValue ->
            ( { model | items = Dict.update key (Maybe.map (\i -> { i | value = newValue })) model.items }
            , Cmd.none
            )

        BeginEditingItem key ->
            ( { model | items = updateDict key beginEditing model.items }
            , Cmd.none
            )

        CancelEditingItem key ->
            ( { model | items = updateDict key cancelEditing model.items }
            , Cmd.none
            )

        SaveItem key ->
            ( { model | items = updateDict key saveItem model.items }
            , saveItemApiRequest key model.items auth
            )

        ItemSaved key (Err e) ->
            let
                _ =
                    Debug.log "edit bio error" e
            in
            ( { model | items = updateDict key (addError "Failed to save") model.items }
            , Cmd.none
            )

        ItemSaved key (Ok user) ->
            ( { model | items = updateDict key valueSaved model.items }
            , Cmd.none
            )


updateDict : a -> (b -> b) -> Dict a b -> Dict a b
updateDict key f =
    Dict.update key (Maybe.map f)


beginEditing : BioItem -> BioItem
beginEditing item =
    { item | state = Editing, oldValue = Just item.value, error = Nothing }


cancelEditing : BioItem -> BioItem
cancelEditing item =
    { item | state = Viewing, value = Maybe.withDefault "" item.oldValue, error = Nothing }


saveItem : BioItem -> BioItem
saveItem item =
    { item | state = Saving, error = Nothing }


addError : String -> BioItem -> BioItem
addError e item =
    { item | state = Editing, error = Just e }


valueSaved : BioItem -> BioItem
valueSaved item =
    { item | state = Viewing, oldValue = Nothing }


saveItemApiRequest : String -> Dict String BioItem -> Api.UserAuth -> Cmd Msg
saveItemApiRequest key items auth =
    let
        maybeItem =
            Dict.get key items
    in
    case ( maybeItem, key ) of
        ( Just item, "Name" ) ->
            Api.changeName auth item.value (ItemSaved key)

        ( Just item, "Bio" ) ->
            Api.changeBio auth item.value (ItemSaved key)

        ( Just item, "Least favorite color" ) ->
            Api.changeLeastFavoriteColor auth item.value (ItemSaved key)

        _ ->
            Cmd.none



-- VIEW


view : Model -> Maybe User -> Element Msg
view model maybeUser =
    let
        ownProfile =
            case maybeUser of
                Nothing ->
                    False

                Just user ->
                    user.id == model.subject.id
    in
    row [ spacing 36, width fill, height fill ]
        [ aboutUser model.subject model.items ownProfile
        , profileContent model.section
        ]


profileContent : Section -> Element Msg
profileContent section =
    column
        [ alignTop
        , alignLeft
        , padding 12
        , width fill
        , spacing 12
        ]
        [ profileNavTabs section
        , paragraph
            [ Font.size 24
            ]
            [ text "Under construction" ]
        ]


profileNavTabs : Section -> Element Msg
profileNavTabs section =
    row
        [ alignLeft
        , alignTop
        , Border.widthEach { top = 0, bottom = 2, left = 0, right = 0 }
        , width fill
        , Border.color Colors.secondary
        ]
        [ navTab "Your Takes" "/profile" (YourTakes == section)
        , navTab "Notifications" "/profile#notifications" (Notifications == section)
        , navTab "Settings" "/profile#settings" (Settings == section)
        ]


aboutUserSidebarWidth =
    320


aboutUserContentWidth =
    300


aboutUser : User -> Dict String BioItem -> Bool -> Element Msg
aboutUser user items editable =
    el
        [ Border.widthEach { left = 0, right = 1, top = 0, bottom = 0 }
        , Border.color Colors.lightGray
        , width (px aboutUserSidebarWidth)
        ]
    <|
        column
            [ spacing 12
            , width (px aboutUserContentWidth)
            ]
            ([ profilePicture user
             , el [ Region.heading 5, Font.size 24, centerX ] (text <| "@" ++ user.username)
             ]
                ++ List.map (\( a, b ) -> aboutUserElem a b editable) (Dict.toList items)
            )


profilePicture : User -> Element Msg
profilePicture user =
    let
        src_ =
            Maybe.withDefault "/assets/profilepic.jpg" user.avatarUrl
    in
    el
        [ Background.color Colors.lightGray
        , padding 8
        , centerX
        , Border.rounded 500
        ]
    <|
        image
            [ width (px 200)
            , height (px 200)
            , Border.rounded 500
            , clip
            , centerX
            , Html.Attributes.style "object-fit" "cover" |> htmlAttribute
            ]
            { src = src_
            , description = "Profile picture"
            }


aboutUserElem : String -> BioItem -> Bool -> Element Msg
aboutUserElem label item editable =
    case item.state of
        Viewing ->
            bioItem label item editable

        Editing ->
            editingBioItem label item

        Saving ->
            savingBioItem label item


breakLongWords =
    Html.Attributes.style "word-break" "break-all" |> htmlAttribute


bioItem : String -> BioItem -> Bool -> Element Msg
bioItem label item editable =
    let
        lbl =
            if editable then
                editableLabel label Viewing

            else
                el [ Font.bold ] <| text label
    in
    column
        [ spacing 6
        , width fill
        , breakLongWords
        , padding 6
        ]
        [ lbl
        , paragraph [] [ text item.value ]
        ]


editingBioItem : String -> BioItem -> Element Msg
editingBioItem label item =
    column
        [ spacing 6
        , width fill
        , breakLongWords
        , padding 6
        ]
        [ editableLabel label Editing
        , errorMsg item.error
        , Input.multiline [ width (px aboutUserContentWidth) ]
            { onChange = EditItem label
            , text = item.value
            , placeholder = Nothing
            , label = Input.labelHidden label
            , spellcheck = False
            }
        ]


errorMsg : Maybe String -> Element Msg
errorMsg error =
    case error of
        Just err ->
            paragraph [ Font.color Colors.primary ] [ text err ]

        Nothing ->
            Element.none


savingBioItem : String -> BioItem -> Element Msg
savingBioItem label item =
    column
        [ spacing 6
        , width fill
        , breakLongWords
        , padding 6
        ]
        [ editableLabel label Saving
        , paragraph [] [ text item.value ]
        ]


editableLabel : String -> EditingState -> Element Msg
editableLabel label state =
    let
        buttons =
            case state of
                Viewing ->
                    [ aboutEditButton "(edit)" (Just <| BeginEditingItem label) ]

                Editing ->
                    [ aboutEditButton "cancel" (Just <| CancelEditingItem label)
                    , text "|"
                    , aboutEditButton "save" (Just <| SaveItem label)
                    ]

                Saving ->
                    [ el [ alignRight ] (text "Saving...") ]
    in
    row [ width fill, spacing 6 ] <|
        [ el [ Font.bold ] <| text label
        ]
            ++ buttons


aboutEditButton : String -> Maybe Msg -> Element Msg
aboutEditButton label onPress =
    Input.button
        [ alignRight
        , Font.color Colors.link
        ]
        { onPress = onPress
        , label = text label
        }
