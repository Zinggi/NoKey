module Views.Notifications exposing (view, init, Config, State)

import Set
import Element exposing (..)
import Elements
import Styles
import Data.Notifications as Notifications exposing (..)
import Data.Sync exposing (SyncData)


type alias State =
    ( Int, Bool )


type alias Config msg =
    { onRejectRequest : Id -> ShareRequest -> msg
    , onGrantRequest : Id -> ShareRequest -> msg
    , onDismiss : Id -> msg
    , onSaveEntry : Id -> String -> SiteEntry -> msg
    , onDeactivateForSite : Id -> String -> msg
    , toMsg : State -> msg
    }


init : State
init =
    ( 2, False )


view : Config msg -> SyncData -> Notifications -> ( Int, Int ) -> State -> Element msg
view config sync ns ( minSecLevel, numberOfAvailableDevices ) state =
    case Notifications.first ns of
        Just n ->
            viewEntry config sync ( minSecLevel, numberOfAvailableDevices ) n state

        Nothing ->
            none


viewEntry : Config msg -> SyncData -> ( Int, Int ) -> Notification -> State -> Element msg
viewEntry config sync ( minSecLevel, numberOfAvailableDevices ) n ( secLevel, shouldShow ) =
    let
        groups gs =
            let
                dict =
                    Data.Sync.namedGroupsDict sync
            in
                Set.toList gs
                    |> List.map (\g -> ( g, Data.Sync.getPostFixFromDict g dict ))
    in
        (case n.data of
            ShareRequestT req ->
                [ column []
                    [ Elements.avatar [] (Data.Sync.getDevice req.deviceId sync)
                    , Elements.p "wants to unlock"
                    , row [] (Elements.enumeration (Elements.groupIcon True) (groups req.keys))
                    ]
                , Elements.buttonRow []
                    [ Elements.button (Just (config.onRejectRequest n.id req)) "Disallow"
                    , Elements.primaryButton (Just (config.onGrantRequest n.id req)) "Allow"
                    ]
                ]

            ExternalSiteEntry entry isNew ->
                [ Elements.h3 <|
                    if isNew then
                        "Save this site?"
                    else
                        "Save updated password?"
                , Elements.inputText [] Nothing { label = "Site", placeholder = "" } entry.site
                , Elements.inputText [] Nothing { label = "Login", placeholder = "" } entry.login
                , Elements.password []
                    { onCopyToClipboard = Nothing
                    , onToggle = Just <| config.toMsg ( secLevel, not shouldShow )
                    , shouldShow = shouldShow
                    }
                    (Just entry.password)
                , Elements.clampedNumberInput (\l -> config.toMsg ( l, shouldShow ))
                    "Security Level"
                    ( minSecLevel, 2, min 5 numberOfAvailableDevices )
                    secLevel
                , Elements.buttonRow []
                    [ Elements.primaryButton
                        (if numberOfAvailableDevices < minSecLevel then
                            Nothing
                         else
                            Just
                                (config.onSaveEntry n.id
                                    (Data.Sync.currentGroupId secLevel sync)
                                    { entry | securityLevel = clamp minSecLevel 5 secLevel }
                                )
                        )
                        "Save"
                    , Elements.button (Just (config.onDismiss n.id)) "Forget"
                    , Elements.button (Just (config.onDeactivateForSite n.id entry.site)) "Deactivate NoKey for this site"
                    ]
                ]
        )
            |> column [ height shrink, width fill, padding (Styles.paddingScale 3), spacing (Styles.paddingScale 2) ]
