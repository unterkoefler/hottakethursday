module Profile exposing (Model, Msg, Section, toModel, toSection, update, updatedUserInfo, view)

import Api
import AssocList as Dict exposing (Dict)
import Colors exposing (ColorScheme)
import Data.User exposing (User)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import File exposing (File)
import File.Select as Select
import Html.Attributes
import Http
import HttpUtils exposing (httpErrorToString)
import NavTabs exposing (navTab)
import Ports



-- MODEL


type alias Model =
    { items : Dict String BioItem
    , subject : User
    , section : Section
    , error : Maybe String
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
    , error = Nothing
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
    | ChangeProfileImage
    | ProfileImageSelected File
    | ProfileImageUpdated (Result Http.Error User)


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


{-| Extracts any new user info that the rest of the app might care about
-}
updatedUserInfo : Msg -> Maybe User
updatedUserInfo msg =
    case msg of
        ItemSaved _ (Ok user) ->
            Just user

        ProfileImageUpdated (Ok user) ->
            Just user

        _ ->
            Nothing


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
            ( { model | items = updateDict key (addError "Failed to save") model.items }
            , Ports.error <| "edit bio error: " ++ httpErrorToString e
            )

        ItemSaved key (Ok user) ->
            ( { model | items = updateDict key valueSaved model.items }
            , Cmd.none
            )

        ChangeProfileImage ->
            ( model
            , Select.file [ "image/png", "image/jpg" ] ProfileImageSelected
            )

        ProfileImageSelected file ->
            ( model
            , Api.uploadProfileImage auth file ProfileImageUpdated
            )

        ProfileImageUpdated (Err e) ->
            let
                _ =
                    Debug.log "Profile image failed to update" e
            in
            ( { model | error = Just "Failed to upload new profile" }
            , Ports.error <| "Profile image failed to update: " ++ httpErrorToString e
            )

        ProfileImageUpdated (Ok user) ->
            ( { model | subject = user }
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


view : Model -> ColorScheme -> Maybe User -> Element Msg
view model colorScheme maybeUser =
    let
        ownProfile =
            case maybeUser of
                Nothing ->
                    False

                Just user ->
                    user.id == model.subject.id
    in
    row [ spacing 36, width fill, height fill ]
        [ aboutUser colorScheme model.subject model.items ownProfile model.error
        , profileContent colorScheme model.section
        ]


profileContent : ColorScheme -> Section -> Element Msg
profileContent colorScheme section =
    column
        [ alignTop
        , alignLeft
        , padding 12
        , width fill
        , spacing 12
        ]
        [ profileNavTabs colorScheme section
        , paragraph
            [ Font.size 24
            ]
            [ text "Under construction" ]
        ]


profileNavTabs : ColorScheme -> Section -> Element Msg
profileNavTabs colorScheme section =
    row
        [ alignLeft
        , alignTop
        , Border.widthEach { top = 0, bottom = 2, left = 0, right = 0 }
        , width fill
        , Border.color colorScheme.secondary
        ]
        [ navTab colorScheme "Your Takes" "/profile" (YourTakes == section)
        , navTab colorScheme "Notifications" "/profile#notifications" (Notifications == section)
        , navTab colorScheme "Settings" "/profile#settings" (Settings == section)
        ]


aboutUserSidebarWidth =
    320


aboutUserContentWidth =
    300


aboutUser : ColorScheme -> User -> Dict String BioItem -> Bool -> Maybe String -> Element Msg
aboutUser colorScheme user items editable error =
    el
        [ Border.widthEach { left = 0, right = 1, top = 0, bottom = 0 }
        , Border.color colorScheme.lightGray
        , width (px aboutUserSidebarWidth)
        ]
    <|
        column
            [ spacing 12
            , width (px aboutUserContentWidth)
            ]
            ([ profilePicture colorScheme user editable
             , el [ centerX ] <| errorMsg colorScheme error
             , el [ Region.heading 5, Font.size 24, centerX ] (text <| "@" ++ user.username)
             ]
                ++ List.map (\( a, b ) -> aboutUserElem colorScheme a b editable) (Dict.toList items)
            )


profilePicture : ColorScheme -> User -> Bool -> Element Msg
profilePicture colorScheme user editable =
    if editable then
        Input.button
            [ centerX ]
            { onPress = Just ChangeProfileImage
            , label = profilePictureNoButton colorScheme user
            }

    else
        profilePictureNoButton colorScheme user


profilePictureNoButton : ColorScheme -> User -> Element Msg
profilePictureNoButton colorScheme user =
    let
        src_ =
            Maybe.withDefault "/assets/profilepic.jpg" user.avatarUrl
    in
    el
        [ Background.color colorScheme.lightGray
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


aboutUserElem : ColorScheme -> String -> BioItem -> Bool -> Element Msg
aboutUserElem colorScheme label item editable =
    case item.state of
        Viewing ->
            bioItem colorScheme label item editable

        Editing ->
            editingBioItem colorScheme label item

        Saving ->
            savingBioItem colorScheme label item


breakLongWords =
    Html.Attributes.style "word-break" "break-all" |> htmlAttribute


bioItem : ColorScheme -> String -> BioItem -> Bool -> Element Msg
bioItem colorScheme label item editable =
    let
        lbl =
            if editable then
                editableLabel colorScheme label Viewing

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


editingBioItem : ColorScheme -> String -> BioItem -> Element Msg
editingBioItem colorScheme label item =
    column
        [ spacing 6
        , width fill
        , breakLongWords
        , padding 6
        ]
        [ editableLabel colorScheme label Editing
        , errorMsg colorScheme item.error
        , Input.multiline [ width (px aboutUserContentWidth) ]
            { onChange = EditItem label
            , text = item.value
            , placeholder = Nothing
            , label = Input.labelHidden label
            , spellcheck = False
            }
        ]


errorMsg : ColorScheme -> Maybe String -> Element Msg
errorMsg colorScheme error =
    case error of
        Just err ->
            paragraph [ Font.color colorScheme.primary ] [ text err ]

        Nothing ->
            Element.none


savingBioItem : ColorScheme -> String -> BioItem -> Element Msg
savingBioItem colorScheme label item =
    column
        [ spacing 6
        , width fill
        , breakLongWords
        , padding 6
        ]
        [ editableLabel colorScheme label Saving
        , paragraph [] [ text item.value ]
        ]


editableLabel : ColorScheme -> String -> EditingState -> Element Msg
editableLabel colorScheme label state =
    let
        buttons =
            case state of
                Viewing ->
                    [ aboutEditButton colorScheme "(edit)" (Just <| BeginEditingItem label) ]

                Editing ->
                    [ aboutEditButton colorScheme "cancel" (Just <| CancelEditingItem label)
                    , text "|"
                    , aboutEditButton colorScheme "save" (Just <| SaveItem label)
                    ]

                Saving ->
                    [ el [ alignRight ] (text "Saving...") ]
    in
    row [ width fill, spacing 6 ] <|
        [ el [ Font.bold ] <| text label
        ]
            ++ buttons


aboutEditButton : ColorScheme -> String -> Maybe Msg -> Element Msg
aboutEditButton colorScheme label onPress =
    Input.button
        [ alignRight
        , Font.color colorScheme.link
        ]
        { onPress = onPress
        , label = text label
        }
