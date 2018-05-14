module Views.Passwords exposing (Config, State, Msg, init, clear, update, tasks, view, actionButton, finishEdit)

import Dict exposing (Dict)
import Dict.Extra as Dict
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
    , movePw : AccountId -> GroupId -> GroupId -> msg
    , onNewPasswordRequirements : Views.PasswordGenerator.State -> msg
    , onCancelExportPassword : msg
    }


type alias State =
    { search : String
    , expandedSites : Set String
    , editPw : Maybe AccountId
    , movePw : Maybe AccountId
    , deletePressed : Maybe AccountId
    , selectedGroup : Maybe GroupId
    }


clear : State -> State
clear state =
    init


type Msg
    = UpdateSearch String
    | ToggleSite String
    | EditPassword AccountId
    | MovePw AccountId
    | SelectGroup (Maybe GroupId)
    | PressDelete AccountId
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

        MovePw accountId ->
            { state | movePw = Just accountId }

        SelectGroup groupId ->
            { state | selectedGroup = groupId }

        PressDelete accountId ->
            { state | deletePressed = Just accountId }

        CancelEdit ->
            finishEdit state


finishEdit : State -> State
finishEdit state =
    { state | editPw = Nothing, movePw = Nothing, selectedGroup = Nothing, deletePressed = Nothing }


init : State
init =
    { search = "", expandedSites = Set.empty, editPw = Nothing, movePw = Nothing, selectedGroup = Nothing, deletePressed = Nothing }


view : Config msg -> { m | syncData : SyncData, passwordsView : State, requirementsState : Views.PasswordGenerator.State } -> Element msg
view config ({ syncData, passwordsView, requirementsState } as model) =
    let
        hasPasswords =
            Data.Sync.mapGroups (\_ _ _ _ -> 1) syncData |> (not << List.isEmpty)
    in
        column [ spacing (Styles.paddingScale 1) ]
            [ tasks config syncData passwordsView (Data.Sync.getTasks syncData)
            , search config hasPasswords passwordsView.search
            , passwords config passwordsView requirementsState syncData
            , -- This is here such that we can scroll below the action button
              el [ height (px 30) ] none
            ]


actionButton : Config msg -> { m | syncData : SyncData } -> Element msg
actionButton config model =
    if Data.Sync.numberOfKnownDevices model.syncData >= Data.Sync.minSecurityLevel model.syncData then
        Elements.floatingButton [] config.onAddNewPassword "Add new"
    else
        none


search : Config msg -> Bool -> String -> Element msg
search config hasPasswords searchValue =
    if hasPasswords then
        -- TODO: not really a search, more like a filter ->
        -- add clear filter!
        el [ padding (Styles.paddingScale 1), width fill ] (Elements.search (config.toMsg << UpdateSearch) searchValue)
    else
        none


tasks : Config msg -> SyncData -> State -> List Task -> Element msg
tasks config sync state ts =
    if List.isEmpty ts then
        none
    else
        Elements.container
            [ Elements.h3 "Tasks"
            , List.map (viewTask config sync state) ts |> column [ spacing (Styles.paddingScale 0) ]
            ]


viewTask : Config msg -> SyncData -> State -> Task -> Element msg
viewTask config sync state task =
    let
        card =
            Elements.card 1 []

        niceRow =
            row [ spacing (Styles.paddingScale 0) ]
    in
        case task of
            MoveFromGroupToGroup { accounts, from, to, fromStatus, toStatus } ->
                card
                    [ row [ spacing (Styles.paddingScale 1) ]
                        [ viewGroupsStatus config [ ( from, fromStatus ), ( to, toStatus ) ] False, Elements.text "to move" ]
                    , viewSitesListSimple config state accounts
                    , niceRow [ Elements.p "from", viewGroup from fromStatus, Elements.p "to", viewGroup to toStatus ]
                    ]

            MoveFromStashToGroup { accounts, group, status } ->
                card
                    [ row [ spacing (Styles.paddingScale 1) ]
                        [ viewGroupStatus config [ group ] False status, Elements.text "to save" ]
                    , viewSitesListSimple config state accounts
                    , niceRow [ Elements.p "into", viewGroup group status ]
                    ]

            WaitForKeysDistributed { accounts, group, status, progress } ->
                card
                    [ Elements.paragraph []
                        [ Elements.text
                            ("Wait until enough ("
                                ++ toString progress
                                ++ "/"
                                ++ toString (getLevel group)
                                ++ ") keys are distributed to finish creating group "
                            )
                        , viewGroup group status
                        ]

                    -- , viewSitesListSimple config state accounts
                    ]

            CreateMoreShares { for, group, status } ->
                card
                    [ row [ spacing (Styles.paddingScale 1) ]
                        [ viewGroupStatus config [ group ] False status
                        , Elements.text "to create keys for:"
                        ]
                    , column [ Styles.paddingLeft (Styles.scaled 1) ] (List.map (Elements.avatar []) for)
                    ]

            ExportPws { done, toDo } ->
                card
                    [ if List.isEmpty toDo then
                        none
                      else
                        Elements.paragraph [ spacing (Styles.paddingScale 1) ]
                            [ el [ alignLeft ] (viewGroupsStatus config toDo False)
                            , Elements.text "to export passwords."
                            ]
                    , if List.isEmpty done then
                        Elements.button (Just config.onCancelExportPassword) "Cancel"
                      else
                        column []
                            [ Elements.paragraph []
                                [ Elements.text "Passwords in"
                                , el [ alignLeft ] (viewGroups done)
                                , Elements.text " are ready to export."
                                ]
                            , niceRow
                                [ Elements.button (Just config.onCancelExportPassword) "Cancel"
                                , Elements.downloadJsonButton config.onCancelExportPassword (Data.Sync.exportReadyPasswords sync) "Export ready passwords"
                                ]
                            ]
                    ]


passwords : Config msg -> State -> Views.PasswordGenerator.State -> SyncData -> Element msg
passwords config state requirementsState sync =
    Elements.container (Data.Sync.mapGroups (viewSites config state requirementsState sync) sync)


viewSites : Config msg -> State -> Views.PasswordGenerator.State -> SyncData -> Group -> Int -> Status -> Dict String (Dict String PasswordStatus) -> Element msg
viewSites config state requirementsState sync group shares groupStatus accounts =
    let
        notEnoughShares =
            shares < getLevel group
    in
        [ viewGroupHeader config group notEnoughShares groupStatus
        , viewSitesList config requirementsState sync group notEnoughShares groupStatus state accounts
        ]
            |> column (spacing (Styles.paddingScale 2) :: height shrink :: Styles.grayedOutIf notEnoughShares)


viewSitesList : Config msg -> Views.PasswordGenerator.State -> SyncData -> Group -> Bool -> Status -> State -> Dict String (Dict String PasswordStatus) -> Element msg
viewSitesList config requirementsState sync group disabled groupStatus state accounts =
    Dict.foldl
        (\siteName userNames acc ->
            viewPw config requirementsState sync group disabled groupStatus state siteName userNames :: acc
        )
        []
        accounts
        |> Elements.stripedList (Styles.cardShadow 1) [ width fill ]


viewSitesListSimple : Config msg -> State -> Dict String (Dict String a) -> Element msg
viewSitesListSimple config state accounts =
    Dict.foldl
        (\siteName userNames acc ->
            viewSiteHeader siteName userNames :: acc
        )
        []
        accounts
        |> Elements.stripedList (Styles.cardShadow 1) [ width fill ]


viewGroupHeader : Config msg -> Group -> Bool -> Status -> Element msg
viewGroupHeader config group disabled groupStatus =
    row []
        [ el [ alignLeft ] (viewGroup group groupStatus)
        , el [ alignRight ] (viewGroupStatus config [ group ] disabled groupStatus)
        ]


viewPw : Config msg -> Views.PasswordGenerator.State -> SyncData -> Group -> Bool -> Status -> State -> String -> Dict String PasswordStatus -> Element msg
viewPw config requirementsState sync group disabled groupStatus state siteName userNames =
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
            none
        else
            Elements.expandable (config.toMsg (ToggleSite siteName))
                (Set.member siteName state.expandedSites)
                [ width fill ]
                (viewSiteHeader siteName userNames)
                (viewSiteData config state requirementsState sync siteName userNames group disabled groupStatus)


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


viewSiteData : Config msg -> State -> Views.PasswordGenerator.State -> SyncData -> String -> Dict String PasswordStatus -> Group -> Bool -> Status -> Element msg
viewSiteData config state requirementsState sync siteName userNames group disabled groupStatus =
    let
        ( groupId, _ ) =
            group
    in
        column [ padding (Styles.paddingScale 2), spacing (Styles.paddingScale 4) ]
            (Dict.toList userNames
                |> List.map
                    (\( login, status ) ->
                        column [ spacing (Styles.paddingScale 1) ]
                            [ column [ spacing (Styles.paddingScale 1) ]
                                [ row [ spacing (Styles.paddingScale 0) ]
                                    [ Elements.inputText [ Styles.selectable ] Nothing { label = "Login", placeholder = "" } login
                                    , Elements.copyToClipboardIcon (Just config.onCopyToClipboard) login
                                    ]
                                , if state.editPw == Just ( siteName, login ) then
                                    none
                                  else
                                    viewStatus config sync ( siteName, login ) status
                                ]
                            , if RequestPassword.isUnlocked status then
                                if state.editPw == Just ( siteName, login ) then
                                    column [ spacing (Styles.paddingScale 1) ]
                                        [ Views.PasswordGenerator.view (config.addPassword groupId ( siteName, login ))
                                            True
                                            config.onNewPasswordRequirements
                                            requirementsState
                                        , Elements.button (Just (config.toMsg CancelEdit)) "Cancel Edit"
                                        ]
                                else if state.movePw == Just ( siteName, login ) then
                                    column [ spacing (Styles.paddingScale 1) ]
                                        [ Elements.p "Move to which group?"
                                        , row [ spacing (Styles.paddingScale 0) ]
                                            (Elements.customSelect (config.toMsg << SelectGroup << Maybe.map Tuple.first)
                                                (\isSelected group ->
                                                    Elements.groupIcon True group
                                                )
                                                (\( g, _ ) -> Just g == state.selectedGroup)
                                                (Data.Sync.namedGroups sync |> List.filter (\( g, _ ) -> groupId /= g))
                                            )
                                        , row [ spacing (Styles.paddingScale 0) ]
                                            [ Elements.button (Just (config.toMsg CancelEdit)) "Cancel Move"
                                            , case state.selectedGroup of
                                                Just g ->
                                                    Elements.primaryButton (Just (config.movePw ( siteName, login ) groupId g)) "Move"

                                                Nothing ->
                                                    none
                                            ]
                                        ]
                                else if state.deletePressed == Just ( siteName, login ) then
                                    column [ spacing (Styles.paddingScale 1) ]
                                        [ Elements.b "Are you sure?"
                                        , Elements.p "Do you really want to delete this password?"
                                        , row [ spacing (Styles.paddingScale 0) ]
                                            [ Elements.button (Just (config.toMsg CancelEdit)) "Cancel"
                                            , Elements.deleteDanger (config.onDeletePassword ( siteName, login ))
                                            ]
                                        ]
                                else
                                    row [ spacing (Styles.paddingScale 0) ]
                                        [ Elements.button (Just (config.toMsg (EditPassword ( siteName, login )))) "Edit"
                                        , if List.length (Data.Sync.groups sync) >= 2 then
                                            Elements.button (Just (config.toMsg (MovePw ( siteName, login )))) "Move"
                                          else
                                            none
                                        , Elements.delete (config.toMsg (PressDelete ( siteName, login )))
                                        ]
                              else
                                viewGroupStatus config [ group ] disabled groupStatus
                            ]
                    )
            )


unlockGroupsButton : (List GroupId -> Maybe AccountId -> msg) -> List ( Group, Status ) -> Element msg
unlockGroupsButton onRequestPasswordPressed =
    groupsButtonHelper "Unlock" (\ids -> onRequestPasswordPressed ids Nothing)


lockGroupsButton : (List GroupId -> msg) -> List ( Group, Status ) -> Element msg
lockGroupsButton =
    groupsButtonHelper "Lock"


groupsButtonHelper : String -> (List GroupId -> msg) -> List ( Group, Status ) -> Element msg
groupsButtonHelper txt onPress groups =
    let
        groupIds =
            List.map (Tuple.first << Tuple.first) groups
                |> List.filter (\( l, _ ) -> l /= 1)
    in
        if List.isEmpty groupIds then
            none
        else
            Elements.customButton []
                (Just (onPress groupIds))
                (row [ spacing (Styles.paddingScale 0) ] [ Elements.text txt, viewGroups groups ])


viewGroups : List ( Group, Status ) -> Element msg
viewGroups groups =
    row [ spacing (Styles.paddingScale 0) ] (Elements.enumeration (\( g, s ) -> viewGroup g s) groups)


viewGroup : Group -> Status -> Element msg
viewGroup group status =
    case status of
        Done _ _ ->
            Elements.groupIcon False group

        _ ->
            Elements.groupIcon True group


viewGroupsStatus : Config msg -> List ( Group, Status ) -> Bool -> Element msg
viewGroupsStatus config groups disabled =
    let
        groupedByStatus =
            Dict.groupBy (Tuple.second >> RequestPassword.statusToComparable) groups
                |> Dict.toList
                |> List.filter (\( status, _ ) -> status /= "Done")
    in
        if List.isEmpty groupedByStatus then
            none
        else
            groupedByStatus
                |> Elements.enumeration
                    (\( statusAsString, ids ) ->
                        case ids of
                            ( id, status ) :: _ ->
                                viewGroupStatus config (List.map Tuple.first ids) disabled status

                            _ ->
                                none
                    )
                |> row [ spacing (Styles.paddingScale 0), width shrink ]


viewGroupStatus : Config msg -> List Group -> Bool -> Status -> Element msg
viewGroupStatus config groupIds disabled status =
    let
        idsWithStatus =
            List.map (\id -> ( id, status )) groupIds
    in
        case status of
            Waiting n m ->
                row [ spacing (Styles.paddingScale 0) ]
                    [ Elements.text "Unlocking"
                    , Icons.loading
                    , Elements.text <| toString n ++ "/" ++ toString m
                    ]

            NotRequested ->
                if disabled then
                    none
                else
                    unlockGroupsButton config.onRequestPasswordPressed idsWithStatus

            Done _ _ ->
                lockGroupsButton config.onLockGroupsPressed idsWithStatus

            Error e ->
                Elements.text ("Error: " ++ e)


viewStatus : Config msg -> SyncData -> AccountId -> PasswordStatus -> Element msg
viewStatus config sync accountId status =
    let
        entry showPw pw =
            Elements.password []
                { onCopyToClipboard = Just config.onCopyToClipboard
                , onToggle = Just (config.onTogglePassword accountId)
                , shouldShow = showPw
                }
                pw
    in
        case status of
            Unlocked pw ->
                entry True (Just pw)

            UnlockedButHidden ->
                entry False (Data.Sync.getPassword accountId sync)

            _ ->
                Elements.password [] { onCopyToClipboard = Nothing, onToggle = Nothing, shouldShow = False } Nothing
