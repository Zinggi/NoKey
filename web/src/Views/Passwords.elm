module Views.Passwords exposing (..)

import Dict exposing (Dict)
import Element exposing (..)
import Elements
import Styles
import Icons
import Data.RequestGroupPassword as RequestPassword exposing (Status(..), PasswordStatus(..))
import Data.Sync exposing (SyncData)
import Data exposing (..)
import Simple.Fuzzy as Fuzzy


{-
   - TODO: groups should have names:
       - when saving a new password, user can choose name.
       - by default the name is empty
       - The displayed name is level + name (+ parts of id if necessary)

   - TODO: hide stash pw
   - TODO: collapse entries, e.g. collapsed only show site name, expanded show accounts + pws
-}


type alias Config msg =
    { toMsg : Msg -> msg
    , onDeletePassword : AccountId -> msg
    , onRequestPasswordPressed : GroupId -> Maybe AccountId -> msg
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


view : Config msg -> State -> SyncData -> Element msg
view config state sync =
    Elements.miniPage "Passwords"
        [ tasks sync.passwordStash
        , search config state.search
        , passwords config state.search sync
        ]


search : Config msg -> String -> Element msg
search config searchValue =
    Elements.search (config.toMsg << UpdateSearch) searchValue


tasks : a -> Element msg
tasks stash =
    Elements.container
        [ Elements.h3 "Tasks"
        , toString stash |> Elements.p
        ]


passwords : Config msg -> String -> SyncData -> Element msg
passwords config search sync =
    Elements.container <|
        -- TODO: use mapStash from SyncData
        stash search sync.passwordStash
            :: Data.Sync.mapGroups (viewGroups config search) sync


stash : String -> Dict AccountId ( GroupId, String ) -> Element msg
stash search pwStash =
    let
        filtered =
            Dict.foldl
                (\accountId ( groupId, pw ) acc ->
                    if Fuzzy.match search (toString accountId) then
                        Elements.text (toString accountId ++ " pw: " ++ pw) :: acc
                    else
                        acc
                )
                []
                pwStash
    in
        if List.isEmpty filtered then
            empty
        else
            (Elements.h4 "Local stash" :: filtered)
                |> column []


viewGroups : Config msg -> String -> GroupId -> Status -> Dict String (Dict String PasswordStatus) -> Element msg
viewGroups config search groupId groupStatus accounts =
    [ viewGroupHeader config groupId groupStatus
    , (Dict.foldl
        (\siteName userNames acc ->
            viewPw config search siteName userNames :: acc
        )
        []
        accounts
        |> column [ Styles.paddingLeft (Styles.scaled 1) ]
      )
    ]
        |> column []


viewGroupHeader : Config msg -> GroupId -> Status -> Element msg
viewGroupHeader config groupId groupStatus =
    row []
        [ el [ alignLeft ] (viewGroup groupId groupStatus)
        , el [ alignRight ] (viewGroupStatus config groupId groupStatus)
        ]


viewPw : Config msg -> String -> String -> Dict String PasswordStatus -> Element msg
viewPw config search siteName userNames =
    let
        filterd =
            List.filterMap
                (\( userName, status ) ->
                    if Fuzzy.match search (siteName ++ userName) then
                        Just ( siteName, userName, status )
                    else
                        Nothing
                )
                (Dict.toList userNames)
    in
        case filterd of
            [] ->
                empty

            [ ( siteName, userName, status ) ] ->
                pwRow config [ Elements.h4 siteName ] ( siteName, userName ) status

            other ->
                column []
                    [ Elements.h4 siteName
                    , (List.map
                        (\( siteName, userName, status ) ->
                            pwRow config [] ( siteName, userName ) status
                        )
                        other
                      )
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


viewGroup : GroupId -> Status -> Element msg
viewGroup ( level, _ ) status =
    case status of
        Done _ _ ->
            row [] [ Elements.h3 (toString level), Icons.unlocked ]

        _ ->
            row [] [ Elements.h3 (toString level), Icons.locked ]


viewGroupStatus : Config msg -> GroupId -> Status -> Element msg
viewGroupStatus config groupId status =
    case status of
        Waiting n m ->
            row [ spacing (Styles.paddingScale 0) ]
                [ Elements.text "Unlocking"
                , Icons.loading
                , Elements.text <| toString n ++ "/" ++ toString m
                ]

        NotRequested ->
            Elements.customButton (Just (config.onRequestPasswordPressed groupId Nothing))
                (row [ spacing (Styles.paddingScale 0) ] [ Elements.text "Unlock", viewGroup groupId status ])

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
