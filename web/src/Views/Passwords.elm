module Views.Passwords exposing (Config, State, Msg, init, update, tasks, view, actionButton, finishEdit)

import Dict exposing (Dict)
import Set exposing (Set)
import Element exposing (..)
import Element.Background as Background
import Elements
import Styles
import Icons
import Data.RequestGroupPassword as RequestPassword exposing (Status(..), PasswordStatus(..))
import Data.Sync exposing (SyncData)
import Data.Settings exposing (Settings)
import Data exposing (..)
import Data.TaskList exposing (Task(..))
import Simple.Fuzzy as Fuzzy
import Helper
import Views.PasswordGenerator


{-
   - TODO: groups should have names:
       - when saving a new password, user can choose name.
       - by default the name is empty
       - The displayed name is level + name (+ parts of id if necessary)

-}


type alias Config msg =
    { toMsg : Msg -> msg
    , onDeletePassword : AccountId -> msg
    , onRequestPasswordPressed : List GroupId -> Maybe AccountId -> msg
    , onLockGroupsPressed : List GroupId -> msg
    , onTogglePassword : AccountId -> msg
    , onAddNewPassword : msg
    , onCopyToClipboard : msg
    , addPassword : GroupId -> AccountId -> String -> msg
    , onNewPasswordRequirements : Views.PasswordGenerator.State -> msg
    }


type alias State =
    { search : String
    , expandedSites : Set String
    , editPw : Maybe AccountId
    }


type Msg
    = UpdateSearch String
    | ToggleSite String
    | EditPassword AccountId
    | CancelEdit


update : Msg -> State -> State
update msg state =
    case msg of
        UpdateSearch s ->
            { state | search = s }

        ToggleSite s ->
            { state | expandedSites = Helper.addOrRemoveFromSet s state.expandedSites }

        EditPassword accountId ->
            { state | editPw = Just accountId }

        CancelEdit ->
            finishEdit state


finishEdit : State -> State
finishEdit state =
    { state | editPw = Nothing }


init : State
init =
    { search = "", expandedSites = Set.empty, editPw = Nothing }


view : Config msg -> { m | syncData : SyncData, passwordsView : State, requirementsState : Views.PasswordGenerator.State } -> Element msg
view config ({ syncData, passwordsView, requirementsState } as model) =
    let
        hasPasswords =
            Data.Sync.mapGroups (\_ _ _ _ -> 1) syncData |> (not << List.isEmpty)
    in
        column [ spacing (Styles.paddingScale 1) ]
            [ tasks config passwordsView (Data.Sync.getTasks syncData)
            , search config hasPasswords passwordsView.search
            , passwords config passwordsView requirementsState syncData
            , -- This is here such that we can scroll below the action button
              el [ height (px 30) ] empty
            ]


actionButton : Config msg -> { m | syncData : SyncData } -> Element msg
actionButton config model =
    if Data.Sync.numberOfKnownDevices model.syncData >= Data.Sync.minSecurityLevel model.syncData then
        Elements.floatingButton config.onAddNewPassword "Add new"
    else
        empty


search : Config msg -> Bool -> String -> Element msg
search config hasPasswords searchValue =
    if hasPasswords then
        -- TODO: not really a search, more like a filter ->
        -- add clear filter!
        el [ padding (Styles.paddingScale 1), width fill ] (Elements.search (config.toMsg << UpdateSearch) searchValue)
    else
        empty


tasks : Config msg -> State -> List Task -> Element msg
tasks config state ts =
    if List.isEmpty ts then
        empty
    else
        Elements.container
            [ Elements.h3 "Tasks"
            , List.map (viewTask config state) ts |> column [ spacing (Styles.paddingScale 0) ]
            ]


viewTask : Config msg -> State -> Task -> Element msg
viewTask config state task =
    let
        card =
            Elements.card 1 []
    in
        case task of
            MoveFromStashToGroup { accounts, group, status } ->
                card
                    [ row [ spacing (Styles.paddingScale 1) ]
                        [ viewGroupStatus config group False status, Elements.text "to save" ]
                    , viewSitesListSimple config state accounts
                    , row [] [ Elements.p "into", viewGroup group status ]
                    ]

            WaitForKeysDistributed { accounts, group, status, progress } ->
                let
                    ( level, _ ) =
                        group
                in
                    card
                        [ Elements.p ("Wait until enough (" ++ toString progress ++ "/" ++ toString level ++ ") keys are distributed to save")
                        , viewSitesListSimple config state accounts
                        , row [] [ Elements.p "into", viewGroup group status ]
                        ]

            CreateMoreShares { for, group, status } ->
                card
                    [ row [ spacing (Styles.paddingScale 1) ]
                        [ viewGroupStatus config group False status
                        , Elements.text "to create keys for:"
                        ]
                    , column [ Styles.paddingLeft (Styles.scaled 1) ] (List.map (Elements.avatar []) for)
                    ]


passwords : Config msg -> State -> Views.PasswordGenerator.State -> SyncData -> Element msg
passwords config state requirementsState sync =
    Elements.container (Data.Sync.mapGroups (viewSites config state requirementsState sync) sync)


viewSites : Config msg -> State -> Views.PasswordGenerator.State -> SyncData -> GroupId -> Int -> Status -> Dict String (Dict String PasswordStatus) -> Element msg
viewSites config state requirementsState sync groupId shares groupStatus accounts =
    [ viewGroupHeader config groupId (shares < Tuple.first groupId) groupStatus
    , viewSitesList config requirementsState sync groupId (shares < Tuple.first groupId) groupStatus state accounts
    ]
        |> column (spacing (Styles.paddingScale 2) :: height shrink :: Styles.grayedOutIf (shares < Tuple.first groupId))


viewSitesList : Config msg -> Views.PasswordGenerator.State -> SyncData -> GroupId -> Bool -> Status -> State -> Dict String (Dict String PasswordStatus) -> Element msg
viewSitesList config requirementsState sync groupId disabled groupStatus state accounts =
    Dict.foldl
        (\siteName userNames acc ->
            viewPw config requirementsState sync groupId disabled groupStatus state siteName userNames :: acc
        )
        []
        accounts
        |> Elements.stripedList (Styles.cardShadow 1) [ width fill ]


viewSitesListSimple : Config msg -> State -> Dict String (Dict String PasswordStatus) -> Element msg
viewSitesListSimple config state accounts =
    Dict.foldl
        (\siteName userNames acc ->
            viewSiteHeader siteName userNames :: acc
        )
        []
        accounts
        |> Elements.stripedList (Styles.cardShadow 1) [ width fill ]


viewGroupHeader : Config msg -> GroupId -> Bool -> Status -> Element msg
viewGroupHeader config groupId disabled groupStatus =
    row []
        [ el [ alignLeft ] (viewGroup groupId groupStatus)
        , el [ alignRight ] (viewGroupStatus config groupId disabled groupStatus)
        ]


viewPw : Config msg -> Views.PasswordGenerator.State -> SyncData -> GroupId -> Bool -> Status -> State -> String -> Dict String PasswordStatus -> Element msg
viewPw config requirementsState sync groupId disabled groupStatus state siteName userNames =
    let
        filterd =
            List.filterMap
                (\( userName, status ) ->
                    if Fuzzy.match state.search (siteName ++ userName) then
                        Just ( userName, status )
                    else
                        Nothing
                )
                (Dict.toList userNames)
    in
        if List.isEmpty filterd then
            empty
        else
            Elements.expandable (config.toMsg (ToggleSite siteName))
                (Set.member siteName state.expandedSites)
                [ width fill ]
                (viewSiteHeader siteName userNames)
                (viewSiteData config state requirementsState sync siteName userNames groupId disabled groupStatus)


viewSiteHeader : String -> Dict String a -> Element msg
viewSiteHeader siteName userNames =
    let
        names =
            Dict.keys userNames
                |> Helper.intersperseLastOneDifferent identity ", " " and "
                |> String.join ""
    in
        row [ padding (Styles.paddingScale 2), spacing (Styles.paddingScale 2), clipX ]
            [ el [ alignLeft ] (Elements.siteLogo siteName)
            , column [ width fill, clipX ] [ Elements.h3 siteName, Elements.textWithCustomOverflow ("... (" ++ toString (Dict.size userNames) ++ ")") names ]
            ]


viewSiteData : Config msg -> State -> Views.PasswordGenerator.State -> SyncData -> String -> Dict String PasswordStatus -> GroupId -> Bool -> Status -> Element msg
viewSiteData config state requirementsState sync siteName userNames groupId disabled groupStatus =
    column [ padding (Styles.paddingScale 2), spacing (Styles.paddingScale 4) ]
        (Dict.toList userNames
            |> List.map
                (\( login, status ) ->
                    column [ spacing (Styles.paddingScale 1) ]
                        [ column [ spacing (Styles.paddingScale 1) ]
                            [ Elements.inputText [] Nothing { label = "Login", placeholder = "" } login
                            , if state.editPw == Just ( siteName, login ) then
                                empty
                              else
                                viewStatus config sync ( siteName, login ) status
                            ]
                        , if RequestPassword.isUnlocked status then
                            if state.editPw == Just ( siteName, login ) then
                                column []
                                    [ Views.PasswordGenerator.view (config.addPassword groupId ( siteName, login ))
                                        True
                                        config.onNewPasswordRequirements
                                        requirementsState
                                    , Elements.button (Just (config.toMsg CancelEdit)) "Cancel Edit"
                                    ]
                            else
                                row []
                                    [ Elements.button (Just (config.toMsg (EditPassword ( siteName, login )))) "Edit"
                                    , Elements.delete (config.onDeletePassword ( siteName, login ))
                                    ]
                          else
                            viewGroupStatus config groupId disabled groupStatus
                        ]
                )
        )


unlockGroupsButton : (List GroupId -> Maybe AccountId -> msg) -> List ( GroupId, Status ) -> Element msg
unlockGroupsButton onRequestPasswordPressed groups =
    let
        groupIds =
            List.map Tuple.first groups
    in
        Elements.customButton (Just (onRequestPasswordPressed groupIds Nothing))
            (row [ spacing (Styles.paddingScale 0) ] [ Elements.text "Unlock", viewGroups groups ])


lockGroupsButton : (List GroupId -> msg) -> List ( GroupId, Status ) -> Element msg
lockGroupsButton onLockGroupsPressed groups =
    let
        groupIds =
            List.map Tuple.first groups
    in
        Elements.customButton (Just (onLockGroupsPressed groupIds))
            (row [ spacing (Styles.paddingScale 0) ] [ Elements.text "Lock", viewGroups groups ])


viewGroups : List ( GroupId, Status ) -> Element msg
viewGroups groups =
    row [] (Elements.enumeration (\( g, s ) -> viewGroup g s) groups)


viewGroup : GroupId -> Status -> Element msg
viewGroup groupId status =
    -- TODO: maybe show more info here, e.g. who has a key
    case status of
        Done _ _ ->
            Elements.groupIcon False groupId

        _ ->
            Elements.groupIcon True groupId


viewGroupStatus : Config msg -> GroupId -> Bool -> Status -> Element msg
viewGroupStatus config groupId disabled status =
    case status of
        Waiting n m ->
            row [ spacing (Styles.paddingScale 0) ]
                [ Elements.text "Unlocking"
                , Icons.loading
                , Elements.text <| toString n ++ "/" ++ toString m
                ]

        NotRequested ->
            if disabled then
                empty
            else
                unlockGroupsButton config.onRequestPasswordPressed [ ( groupId, status ) ]

        Done _ _ ->
            let
                ( level, _ ) =
                    groupId
            in
                if level == 1 then
                    empty
                else
                    lockGroupsButton config.onLockGroupsPressed [ ( groupId, status ) ]

        Error e ->
            Elements.text ("Error: " ++ e)


viewStatus : Config msg -> SyncData -> AccountId -> PasswordStatus -> Element msg
viewStatus config sync accountId status =
    let
        entry showPw pw =
            column [ spacing (Styles.paddingScale 1) ]
                [ Elements.password [] Nothing showPw pw
                , row [ spacing (Styles.paddingScale 0) ]
                    [ Elements.button (Just (config.onTogglePassword accountId))
                        (if showPw then
                            "Hide"
                         else
                            "Show"
                        )
                    , Elements.copyToClipboard config.onCopyToClipboard (\() -> Data.Sync.getPassword accountId sync |> Maybe.withDefault "")
                    ]
                ]
    in
        case status of
            Unlocked pw ->
                entry True pw

            UnlockedButHidden ->
                entry False "*****"

            _ ->
                Elements.password [] Nothing False "*****"
