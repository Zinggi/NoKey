module Views.Passwords exposing (..)

import Dict exposing (Dict)
import Element exposing (..)
import Elements
import Styles
import Icons
import Data.RequestGroupPassword as RequestPassword exposing (Status(..), PasswordStatus(..))
import Data.Sync exposing (SyncData)
import Data exposing (..)
import Data.TaskList exposing (Task(..))
import Simple.Fuzzy as Fuzzy


{-
   - TODO: groups should have names:
       - when saving a new password, user can choose name.
       - by default the name is empty
       - The displayed name is level + name (+ parts of id if necessary)

   - TODO: collapse entries, e.g. collapsed only show site name, expanded show accounts + pws
-}


type alias Config msg =
    { toMsg : Msg -> msg
    , onDeletePassword : AccountId -> msg
    , onRequestPasswordPressed : List GroupId -> Maybe AccountId -> msg
    , onTogglePassword : AccountId -> msg
    }


type alias State =
    { search : String
    }


type Msg
    = UpdateSearch String


update : Msg -> State -> State
update msg state =
    case msg of
        UpdateSearch s ->
            { search = s }


init : State
init =
    { search = "" }



-- TODO: integrate here!
-- newSiteForm : Views.PasswordGenerator.State -> Bool -> PasswordMetaData -> String -> Int -> Element Msg
-- newSiteForm requirementsState expandSiteEntry entry currentGroupId maxSecurityLevel =
--     column []
--         [ column []
--             [ Elements.inputWithLabel (Just SiteNameChanged) "New Site: " "example.com" entry.siteName
--             ]
--         , if not expandSiteEntry then
--             empty
--           else
--             column []
--                 [ Elements.inputWithLabel (Just UserNameChanged) "Login name" "" entry.userName
--                 , Elements.text "Security Level: "
--                 , Elements.clampedNumberInput SecurityLevelChanged ( 2, 2, maxSecurityLevel ) entry.securityLevel
--                 , Views.PasswordGenerator.view (AddPassword currentGroupId) NewPasswordRequirements requirementsState
--                 ]
--         ]


view : Config msg -> State -> SyncData -> Element msg
view config state sync =
    -- TODO: display add button here!
    -- , if numberOfKnownDevices >= 2 then
    --     newSiteForm model.requirementsState
    --         model.expandSiteEntry
    --         model.newSiteEntry
    --         (Data.Sync.currentGroupId model.newSiteEntry.securityLevel model.syncData)
    --         numberOfKnownDevices
    --   else
    --     text "pair a device to save your first password"
    if Dict.size (Data.Sync.knownIds sync) <= 1 then
        Elements.p "pair a device to save your first password"
    else
        Elements.miniPage
            [ tasks config state.search (Data.Sync.getTasks sync)
            , search config state.search
            , passwords config state.search sync
            ]


search : Config msg -> String -> Element msg
search config searchValue =
    Elements.search (config.toMsg << UpdateSearch) searchValue


tasks : Config msg -> String -> List Task -> Element msg
tasks config search ts =
    if List.isEmpty ts then
        empty
    else
        Elements.container
            [ Elements.h3 "Tasks"
            , List.map (viewTask config search) ts |> column [ Styles.paddingLeft (Styles.scaled 1), spacing (Styles.paddingScale 0) ]
            ]


viewTask : Config msg -> String -> Task -> Element msg
viewTask config search task =
    case task of
        MoveFromStashToGroup { accounts, group, status } ->
            Elements.card []
                [ row [ spacing (Styles.paddingScale 1) ]
                    [ viewGroupStatus config group False status, Elements.text "to save" ]
                , viewSitesList config search accounts
                , row [] [ Elements.p "into", viewGroup group status ]
                ]

        WaitForKeysDistributed { accounts, group, status, progress } ->
            let
                ( level, _ ) =
                    group
            in
                Elements.card []
                    [ Elements.p ("Wait until enough (" ++ toString progress ++ "/" ++ toString level ++ ") keys are distributed to save")
                    , viewSitesList config search accounts
                    , row [] [ Elements.p "into", viewGroup group status ]
                    ]

        CreateMoreShares { for, group, status } ->
            Elements.card []
                [ row [ spacing (Styles.paddingScale 1) ]
                    [ viewGroupStatus config group False status
                    , Elements.text "to create keys for:"
                    ]
                , column [ Styles.paddingLeft (Styles.scaled 1) ] (List.map (Elements.avatar []) for)
                ]


passwords : Config msg -> String -> SyncData -> Element msg
passwords config search sync =
    Elements.container (Data.Sync.mapGroups (viewSites config search) sync)



-- {- I decided the tasks are enough to see what's in the stash.
--  - There is no need to show the stash seperately
--  -}
-- stash : Config msg -> String -> SyncData -> Element msg
-- stash config search sync =
--     let
--         stash =
--             (Tasks.getStash sync.tasks)
--     in
--         if Dict.isEmpty stash then
--             empty
--         else
--             [ Elements.h3 "Local stash"
--             , (Dict.foldl
--                 (\siteName userNames acc ->
--                     viewPw config search siteName userNames :: acc
--                 )
--                 []
--                 stash
--                 |> column [ Styles.paddingLeft (Styles.scaled 1) ]
--               )
--             ]
--                 |> column []


viewSites : Config msg -> String -> GroupId -> Int -> Status -> Dict String (Dict String PasswordStatus) -> Element msg
viewSites config search groupId shares groupStatus accounts =
    [ viewGroupHeader config groupId (shares < Tuple.first groupId) groupStatus
    , viewSitesList config search accounts
    ]
        |> column (Styles.grayedOutIf (shares < Tuple.first groupId))


viewSitesList : Config msg -> String -> Dict String (Dict String PasswordStatus) -> Element msg
viewSitesList config search accounts =
    Dict.foldl
        (\siteName userNames acc ->
            viewPw config search siteName userNames :: acc
        )
        []
        accounts
        |> column [ Styles.paddingLeft (Styles.scaled 1) ]


viewGroupHeader : Config msg -> GroupId -> Bool -> Status -> Element msg
viewGroupHeader config groupId disabled groupStatus =
    row []
        [ el [ alignLeft ] (viewGroup groupId groupStatus)
        , el [ alignRight ] (viewGroupStatus config groupId disabled groupStatus)
        ]


viewPw : Config msg -> String -> String -> Dict String PasswordStatus -> Element msg
viewPw config search siteName userNames =
    let
        filterd =
            List.filterMap
                (\( userName, status ) ->
                    if Fuzzy.match search (siteName ++ userName) then
                        Just ( userName, status )
                    else
                        Nothing
                )
                (Dict.toList userNames)
    in
        case filterd of
            [] ->
                empty

            [ ( userName, status ) ] ->
                pwRow config [ Elements.h4 siteName ] ( siteName, userName ) status

            other ->
                column []
                    [ Elements.h4 siteName
                    , List.map
                        (\( userName, status ) ->
                            pwRow config [] ( siteName, userName ) status
                        )
                        other
                        |> column [ Styles.paddingLeft (Styles.scaled 1) ]
                    ]


pwRow : Config msg -> List (Element msg) -> AccountId -> PasswordStatus -> Element msg
pwRow config pre (( _, userName ) as accountId) status =
    row []
        [ el [ alignLeft ]
            (row [ spacing (Styles.scaled 1) ]
                (pre ++ [ Elements.b userName, viewStatus config accountId status ])
            )
        , if RequestPassword.isUnlocked status then
            el [ alignRight ] (Elements.delete (config.onDeletePassword accountId))
          else
            empty
        ]


unlockGroupsButton : (List GroupId -> Maybe AccountId -> msg) -> List ( GroupId, Status ) -> Element msg
unlockGroupsButton onRequestPasswordPressed groups =
    let
        groupIds =
            List.map Tuple.first groups
    in
        Elements.customButton (Just (onRequestPasswordPressed groupIds Nothing))
            (row [ spacing (Styles.paddingScale 0) ] [ Elements.text "Unlock", viewGroups groups ])


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
            empty

        Error e ->
            Elements.text ("Error: " ++ e)


viewStatus : Config msg -> AccountId -> PasswordStatus -> Element msg
viewStatus config accountId status =
    case status of
        Unlocked pw ->
            row [] [ Elements.password [] Nothing True pw, Elements.button (Just (config.onTogglePassword accountId)) "Hide" ]

        UnlockedButHidden ->
            row [] [ Elements.password [] Nothing False "*****", Elements.button (Just (config.onTogglePassword accountId)) "Show" ]

        _ ->
            Elements.password [] Nothing False "*****"
