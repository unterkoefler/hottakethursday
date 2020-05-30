module Profile exposing (Model, Msg, Section, smallView, toModel, toSection, update, updatedUserInfo, view)

import Api
import AssocList as Dict exposing (Dict)
import Colors exposing (ColorScheme)
import Data.Take exposing (Take)
import Data.User exposing (User)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Feed
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
    , takes : Feed.Model
    , expandTabs : Bool
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
    = Takes
    | Notifications
    | Settings


type EditingState
    = Viewing
    | Editing
    | Saving


toModel : Section -> User -> List Take -> Model
toModel section subject takes =
    { items = itemsFromUser subject
    , subject = subject
    , section = section
    , error = Nothing
    , takes = Feed.addTakes Feed.init takes
    , expandTabs = False
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
    | FeedMsg Feed.Msg
    | ToggleTabs


toSection : Maybe String -> Section
toSection frag =
    case frag of
        Just "notifications" ->
            Notifications

        Just "settings" ->
            Settings

        Just str ->
            Takes

        Nothing ->
            Takes


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


update : Msg -> Model -> User -> Api.UserAuth -> ( Model, Cmd Msg )
update msg model user auth =
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

        ItemSaved key (Ok _) ->
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
            ( { model | error = Just "Failed to upload new profile" }
            , Ports.error <| "Profile image failed to update: " ++ httpErrorToString e
            )

        ProfileImageUpdated (Ok subject) ->
            ( { model | subject = subject }
            , Cmd.none
            )

        FeedMsg m ->
            let
                ( takes, cmd ) =
                    Feed.update m model.takes user auth
            in
            ( { model | takes = takes }
            , Cmd.map FeedMsg cmd
            )

        ToggleTabs ->
            ( { model | expandTabs = not model.expandTabs }
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
        , profileContent colorScheme model ownProfile maybeUser
        ]


profileContent : ColorScheme -> Model -> Bool -> Maybe User -> Element Msg
profileContent colorScheme model ownProfile maybeUser =
    let
        userId =
            model.subject.id
    in
    column
        [ alignTop
        , alignLeft
        , padding 12
        , width fill
        , spacing 12
        ]
        [ profileNavTabs colorScheme model.section ownProfile userId
        , profileBody colorScheme model ownProfile maybeUser
        ]


profileBody : ColorScheme -> Model -> Bool -> Maybe User -> Element Msg
profileBody colorScheme model ownProfile maybeUser =
    case model.section of
        Takes ->
            let
                takes =
                    model.takes.cards
            in
            Element.map FeedMsg <| Feed.feed colorScheme takes maybeUser

        _ ->
            paragraph
                [ Font.size 24
                ]
                [ text "Under construction" ]


profileNavTabs : ColorScheme -> Section -> Bool -> Int -> Element Msg
profileNavTabs colorScheme section ownProfile userId =
    row
        [ alignLeft
        , alignTop
        , Border.widthEach { top = 0, bottom = 2, left = 0, right = 0 }
        , width fill
        , Border.color colorScheme.secondary
        ]
    <|
        if ownProfile then
            [ navTab colorScheme "Takes" "/profile" (Takes == section)
            , navTab colorScheme "Notifications" "/profile#notifications" (Notifications == section)
            , navTab colorScheme "Settings" "/profile#settings" (Settings == section)
            ]

        else
            [ navTab colorScheme
                "Takes"
                ("/profile?uid=" ++ String.fromInt userId)
                True
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



-- SMALL VIEW


smallView : Model -> ColorScheme -> Maybe User -> Element Msg
smallView model colorScheme maybeUser =
    let
        ownProfile =
            case maybeUser of
                Nothing ->
                    False

                Just user ->
                    user.id == model.subject.id
    in
    column [ spacing 36, width fill ]
        [ smallAboutUser colorScheme model.subject model.items ownProfile model.error
        , smallProfileContent colorScheme model ownProfile maybeUser
        ]


smallAboutUser : ColorScheme -> User -> Dict String BioItem -> Bool -> Maybe String -> Element Msg
smallAboutUser colorScheme user items editable error =
    el
        [ Border.widthEach { left = 0, right = 0, top = 0, bottom = 1 }
        , Border.color colorScheme.lightGray
        , width fill
        , paddingEach { left = 0, right = 0, top = 0, bottom = 36 }
        ]
    <|
        column
            [ spacing 12
            ]
            ([ profilePicture colorScheme user editable
             , el [ centerX ] <| errorMsg colorScheme error
             , el [ Region.heading 5, Font.size 24, centerX ] (text <| "@" ++ user.username)
             ]
                ++ List.map (\( a, b ) -> aboutUserElem colorScheme a b editable) (Dict.toList items)
            )


smallProfileContent : ColorScheme -> Model -> Bool -> Maybe User -> Element Msg
smallProfileContent colorScheme model ownProfile maybeUser =
    let
        userId =
            model.subject.id
    in
    column
        [ alignTop
        , alignLeft
        , padding 12
        , width fill
        , spacing 12
        ]
        [ smallProfileNavTabs colorScheme model.section model.expandTabs ownProfile userId
        , smallProfileBody colorScheme model ownProfile maybeUser
        ]


smallProfileBody : ColorScheme -> Model -> Bool -> Maybe User -> Element Msg
smallProfileBody colorScheme model ownProfile maybeUser =
    case model.section of
        Takes ->
            let
                takes =
                    model.takes.cards
            in
            Element.map FeedMsg <| Feed.smallFeed colorScheme takes maybeUser

        _ ->
            paragraph
                [ Font.size 24
                ]
                [ text "Under construction" ]


expanded_ =
    True


smallProfileNavTabs : ColorScheme -> Section -> Bool -> Bool -> Int -> Element Msg
smallProfileNavTabs colorScheme section expandTabs ownProfile userId =
    let
        attributes =
            [ Border.width 2
            , Border.rounded 12
            , Border.color colorScheme.secondary
            , centerX
            , width fill
            ]
    in
    case expandTabs of
        False ->
            Input.button
                attributes
                { onPress = Just ToggleTabs
                , label = currentNavTabLabel colorScheme section expandTabs ownProfile
                }

        True ->
            column
                ([ spacing 0
                 , paddingEach { left = 0, right = 0, top = 0, bottom = 0 }
                 ]
                    ++ attributes
                )
                [ Input.button
                    [ width fill
                    ]
                    { onPress = Just ToggleTabs
                    , label = currentNavTabLabel colorScheme section expandTabs ownProfile
                    }
                , expandedTabOption colorScheme "Takes" "/profile" (Takes == section)
                , expandedTabOption colorScheme "Notifications" "/profile#notifications" (Notifications == section)
                , expandedTabOption colorScheme "Settings" "/profile#settings" (Settings == section)
                ]


expandedTabOption : ColorScheme -> String -> String -> Bool -> Element Msg
expandedTabOption colorScheme label url current =
    let
        fontColor =
            if current then
                colorScheme.textOnSecondary

            else
                colorScheme.secondary
    in
    link
        [ Font.center
        , Font.size 20
        , width fill
        , paddingXY 0 12
        , Font.color fontColor
        ]
        { url = url
        , label = text label
        }


currentNavTabLabel : ColorScheme -> Section -> Bool -> Bool -> Element Msg
currentNavTabLabel colorScheme section expandTabs ownProfile =
    let
        sectionName =
            case section of
                Takes ->
                    "Takes"

                Notifications ->
                    "Notifications"

                Settings ->
                    "Settings"

        arrow =
            case ( expandTabs, ownProfile ) of
                ( _, False ) ->
                    Element.none

                ( False, True ) ->
                    el [ Font.alignRight, paddingXY 6 0 ] <| text "▾"

                ( True, True ) ->
                    el [ Font.alignRight, paddingXY 6 0 ] <| text "▴"

        ( fontColor, bgColor ) =
            if expandTabs then
                ( colorScheme.white, colorScheme.secondary )

            else
                ( colorScheme.secondary, colorScheme.white )

        border =
            if expandTabs then
                Border.roundEach { topLeft = 9, topRight = 9, bottomLeft = 0, bottomRight = 0 }

            else
                Border.rounded 12
    in
    row
        [ Font.color fontColor
        , Background.color bgColor
        , border
        , Font.size 20
        , padding 12
        , width fill
        ]
        [ el [ centerX ] <| text sectionName
        , arrow
        ]


ignore colorScheme section ownProfile userId =
    column
        [ Border.widthEach { top = 0, bottom = 2, left = 0, right = 0 }
        , width shrink
        , Border.color colorScheme.secondary
        ]
    <|
        if ownProfile then
            [ navTab colorScheme "Takes" "/profile" (Takes == section)
            , navTab colorScheme "Notifications" "/profile#notifications" (Notifications == section)
            , navTab colorScheme "Settings" "/profile#settings" (Settings == section)
            ]

        else
            [ navTab colorScheme
                "Takes"
                ("/profile?uid=" ++ String.fromInt userId)
                True
            ]
