module Views.Devices exposing (view, actionButton, Config, State, init, clear, wrongPassword, closeBox)

import Dict exposing (Dict)
import Set
import Element exposing (..)
import Elements
import Styles
import Data.Sync exposing (SyncData)
import Data.KeyBox exposing (KeyBoxId, Box)
import Route exposing (Page(..))
import Icons


type alias State =
    { confirmDelete : Maybe String, isActionButtonOpen : Bool, selectedBox : Maybe BoxState }


init : State
init =
    { confirmDelete = Nothing, isActionButtonOpen = False, selectedBox = Nothing }


clear : State -> State
clear state =
    init


type alias BoxState =
    { id : KeyBoxId, pw : String, error : Maybe String }


initBox : KeyBoxId -> BoxState
initBox id =
    { id = id, pw = "", error = Nothing }


setPw : String -> Maybe BoxState -> Maybe BoxState
setPw pw =
    Maybe.map (\b -> { b | pw = pw })


closeBox state =
    { state | selectedBox = Nothing }


wrongPassword : String -> State -> State
wrongPassword err state =
    { state | selectedBox = Maybe.map (\b -> { b | error = Just err }) state.selectedBox }


type alias Config msg =
    { toMsg : State -> msg
    , onSetDeviceName : String -> msg
    , onGoToPairing : msg
    , onRemoveDevice : String -> msg
    , onCreateKeyBox : msg
    , onOpenBox : Box -> String -> msg
    , onCloseBox : KeyBoxId -> msg
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
                ++ viewKeyBoxes config syncData state
                ++ [ el [ height (px 45) ] none ]
            )


actionButton : Config msg -> State -> Element msg
actionButton config state =
    -- Elements.floatingButton [ alignRight ] config.onGoToPairing "Pair new device"
    if state.isActionButtonOpen then
        column [ spacing (Styles.paddingScale 3) ]
            [ Elements.floatingButton [ alignRight ] config.onCreateKeyBox "Create key box"
            , Elements.floatingButton [ alignRight ] config.onGoToPairing "Pair new device"
            , Elements.floatingIconButton [ alignRight ] (config.toMsg { state | isActionButtonOpen = False }) Icons.close
            ]
    else
        Elements.floatingIconButton [] (config.toMsg { state | isActionButtonOpen = True }) Icons.more


viewKeyBoxes : Config msg -> SyncData -> State -> List (Element msg)
viewKeyBoxes config syncData state =
    let
        boxes =
            Data.Sync.getKeyBoxes syncData

        bs =
            Data.KeyBox.mapBoxes
                (\box ->
                    column [ height shrink ]
                        [ Elements.keyBox
                            (config.toMsg { state | selectedBox = Just (initBox box.id) })
                            (config.onCloseBox box.id)
                            box
                        , if not box.isOpen then
                            case state.selectedBox of
                                Just b ->
                                    if b.id == box.id then
                                        column []
                                            [ Elements.newPasswordInput []
                                                (Just (\p -> config.toMsg { state | selectedBox = setPw p state.selectedBox }))
                                                { label = "Password", placeholder = "" }
                                                b.pw
                                            , case b.error of
                                                Just err ->
                                                    Elements.text "Wrong password"

                                                Nothing ->
                                                    none
                                            , Elements.buttonRow []
                                                [ Elements.button (Just (config.toMsg { state | selectedBox = Nothing })) "Cancel"
                                                , if String.length b.pw >= 8 then
                                                    Elements.primaryButton (Just (config.onOpenBox box b.pw)) "Open"
                                                  else
                                                    none
                                                ]
                                            ]
                                    else
                                        none

                                Nothing ->
                                    none
                          else
                            let
                                gs =
                                    Elements.groupIcons syncData (Set.toList box.hasShares)
                            in
                                Elements.paragraph []
                                    (if List.isEmpty gs then
                                        [ Elements.text "No Keys in here yet." ]
                                     else
                                        [ Elements.text "Has keys for "
                                        , row [] gs
                                        ]
                                    )
                        ]
                )
                boxes
    in
        if List.isEmpty bs then
            []
        else
            el [ paddingXY 0 (Styles.paddingScale 4) ] (Elements.h3 "Key Boxes") :: bs


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
                        , if numDevAfter < Data.Sync.maxUsedSecurityLevel sync then
                            Elements.paragraph []
                                [ Elements.b "WARNING"
                                , Elements.text "If you remove this device, the passwords saved in "
                                , Data.Sync.namedGroupsWithLevel (\l -> l > numDevAfter) sync
                                    |> Elements.enumeration (Elements.groupIcon True)
                                    |> row []
                                , Elements.text "will no longer be accessible. Better pair one more device and remove it then"
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
