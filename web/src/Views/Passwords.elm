module Views.Passwords exposing (..)

import Dict exposing (Dict)
import Element exposing (..)
import Elements
import Styles
import Icons
import Data.RequestGroupPassword as RequestPassword exposing (Status(..), PasswordStatus(..))
import Data.Sync exposing (SyncData)
import Data exposing (..)


{- TODO: groups should have names:
   - when saving a new password, user can choose name.
   - by default the name is empty
   - The displayed name is level + name (+ parts of id if necessary)
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
        , passwords config sync
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


passwords : Config msg -> SyncData -> Element msg
passwords config sync =
    Elements.container <|
        stash sync.passwordStash
            :: Data.Sync.mapGroups (viewGroups config) sync


stash pwStash =
    Elements.h4 "Local stash"
        :: (Dict.foldl
                (\accountId ( groupId, pw ) acc ->
                    Elements.text (toString accountId ++ " pw: " ++ pw) :: acc
                )
                []
                pwStash
           )
        |> column []


viewGroups : Config msg -> GroupId -> Status -> Dict String (Dict String PasswordStatus) -> Element msg
viewGroups config groupId groupStatus accounts =
    [ viewGroupHeader config groupId groupStatus
    , (Dict.foldl
        (\siteName userNames acc ->
            viewPw config siteName userNames :: acc
        )
        []
        accounts
        |> column [ paddingXY (Styles.scaled 1) 0 ]
      )
    ]
        |> column []


viewGroupHeader config groupId groupStatus =
    row []
        [ el [ alignLeft ] (viewGroup groupId groupStatus)
        , el [ alignRight ] (viewGroupStatus config groupId groupStatus)
        ]


viewPw : Config msg -> String -> Dict String PasswordStatus -> Element msg
viewPw config siteName userNames =
    case Dict.toList userNames of
        [ ( userName, status ) ] ->
            pwRow config [ Elements.h4 siteName ] ( siteName, userName ) status

        other ->
            column []
                (Elements.h4 siteName
                    :: (List.map
                            (\( userName, status ) ->
                                pwRow config [] ( siteName, userName ) status
                            )
                            other
                       )
                )


pwRow config pre (( _, userName ) as accountId) status =
    row []
        [ el [ alignLeft ]
            (row [ padding (Styles.paddingScale 0) ]
                (pre ++ [ Elements.b userName, viewStatus config accountId status ])
            )
        , el [ alignRight ] (Elements.delete (config.onDeletePassword accountId))
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


viewStatus config accountId status =
    case status of
        Unlocked pw ->
            row [] [ Elements.password [] Nothing True pw, Elements.button (Just (config.onTogglePassword accountId)) "Hide" ]

        UnlockedButHidden ->
            row [] [ Elements.password [] Nothing False "*****", Elements.button (Just (config.onTogglePassword accountId)) "Show" ]

        _ ->
            Elements.password [] Nothing False "*****"
