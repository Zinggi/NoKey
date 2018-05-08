module Views.Devices exposing (view, actionButton, Config, State, init)

import Dict exposing (Dict)
import Element exposing (..)
import Elements
import Styles
import Data.Sync exposing (SyncData)
import Route exposing (Page(..))


type alias State =
    { confirmDelete : Maybe String }


init : State
init =
    { confirmDelete = Nothing }


type alias Config msg =
    { toMsg : State -> msg
    , onSetDeviceName : String -> msg
    , onGoToPairing : msg
    , onRemoveDevice : String -> msg
    }


view : Config msg -> { m | syncData : SyncData, uniqueIdentifyier : String } -> State -> Element msg
view config { syncData, uniqueIdentifyier } state =
    let
        ( myId, knownIds ) =
            ( uniqueIdentifyier, Data.Sync.knownDevices syncData )
    in
        column [ spacing (Styles.paddingScale 1) ]
            (Elements.myAvatar config.onSetDeviceName myId (Dict.get myId knownIds |> Maybe.withDefault ( "", "" )) []
                :: devicesMap (viewDeviceEntry config syncData state myId) knownIds
                ++ [ el [ height (px 30) ] none ]
            )


actionButton : Config msg -> Element msg
actionButton config =
    Elements.floatingButton config.onGoToPairing "Pair new device"


{-| TODO: fix input lag on input fields. Workaround:

    https://github.com/elm-lang/html/issues/105#issuecomment-309524197
    https://ellie-app.com/3fPSxX6VHK7a1/0

-}
viewDeviceEntry : Config msg -> SyncData -> State -> String -> String -> ( String, String ) -> Element msg
viewDeviceEntry config sync state myId uuid ( name, idPart ) =
    if myId == uuid then
        none
    else
        column [ height shrink, spacing (Styles.paddingScale 1) ]
            [ row []
                [ Elements.avatar [ width fill ] { id = uuid, name = name, postFix = idPart }
                , if state.confirmDelete == Just uuid then
                    none
                  else
                    Elements.delete (config.toMsg { state | confirmDelete = Just uuid })
                ]
            , if state.confirmDelete == Just uuid then
                let
                    numDevAfter =
                        Data.Sync.numberOfKnownDevices sync - 1
                in
                    column [ spacing (Styles.paddingScale 1) ]
                        [ Elements.b "Are you sure?"
                        , Elements.p "Do you really want to remove this device?"

                        -- TODO: this is not true, as when a device gets removed, we delete all its shares.
                        -- Should we keep them?
                        -- , Elements.p "You can reverse this later by just pairing again."
                        , if numDevAfter < Data.Sync.maxUsedSecurityLevel sync then
                            paragraph []
                                [ Elements.b "WARNING"
                                , Elements.p "If you remove this device, the passwords saved in "
                                , Data.Sync.namedGroupsWithLevel (\l -> l > numDevAfter) sync
                                    |> Elements.enumeration (Elements.groupIcon True)
                                    |> row []
                                , Elements.p "will no longer be accessible. Better pair one more device and remove it then"
                                ]
                          else
                            none
                        , row [ spacing (Styles.paddingScale 0) ]
                            [ Elements.button (Just (config.toMsg { state | confirmDelete = Nothing })) "Cancel"
                            , Elements.deleteDanger (config.onRemoveDevice uuid)
                            ]
                        ]
              else
                none
            ]


devicesMap : (String -> ( String, String ) -> b) -> Dict String ( String, String ) -> List b
devicesMap f known_ids =
    Dict.foldl
        (\uuid name acc ->
            f uuid name :: acc
        )
        []
        known_ids
