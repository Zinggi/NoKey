module Views.Settings exposing (view, State, Config, init, parseFileError, clear)

import Element exposing (..)
import Elements
import Route exposing (Page(..))
import Data.Sync exposing (SyncData)
import Data.Settings exposing (Settings, setAllowLevel1)
import Styles


type alias State =
    { showConfirmReset : Bool, parseFileError : Maybe String }


init : State
init =
    { showConfirmReset = False, parseFileError = Nothing }


clear : State -> State
clear state =
    init


parseFileError : String -> State -> State
parseFileError txt state =
    { state | parseFileError = Just txt }


type alias Config msg =
    { toMsg : State -> msg
    , onSetSettings : Settings -> msg
    , onShowTutorial : msg
    , onShowReleaseLog : msg
    , onReset : msg
    , onExportPasswords : msg
    , onOpenExtensionInTab : msg
    }


view : Config msg -> { m | syncData : SyncData } -> State -> Element msg
view config { syncData } state =
    let
        options =
            Data.Sync.getSettings syncData
    in
        column [ spacing (Styles.paddingScale 3) ]
            [ Elements.button (Just config.onShowTutorial) "Show Tutorial"
            , Elements.line
            , Elements.button (Just config.onShowReleaseLog) "Show release log"
            , Elements.line
            , Elements.button (Just config.onExportPasswords) "Export Passwords"
            , Elements.line
            , if Data.Sync.numberOfKnownDevices syncData >= 2 then
                column [ spacing (Styles.paddingScale 3), height shrink ]
                    [ Elements.b "Import Passwords"
                    , column [ spacing (Styles.paddingScale 3) ] <|
                        (if Data.Sync.isExtension syncData then
                            [ Elements.p "File upload only works if the extension is shown in it's own tab."
                            , Elements.button (Just config.onOpenExtensionInTab) "Open in seperate tab"
                            ]
                         else
                            []
                        )
                            ++ [ Elements.fileUpload
                               , case state.parseFileError of
                                    Just e ->
                                        Elements.p ("Error:\n" ++ e)

                                    Nothing ->
                                        none
                               ]
                    , Elements.line
                    ]
              else
                none

            -- Version
            , Elements.text ("Version: " ++ Data.Sync.appVersion)
            , Elements.line

            -- Dangerous settings
            , el [ paddingXY 0 (Styles.paddingScale 3) ] (Elements.h3 "Dangerous")
            , Elements.checkBox (\b -> setAllowLevel1 b options |> config.onSetSettings) False "Allow security level 1" options.allowLevel1
            , Elements.p allowLevel1Txt
            , Elements.line
            , if state.showConfirmReset then
                let
                    numDevAfter =
                        Data.Sync.numberOfKnownDevices syncData - 1
                in
                    column [ spacing (Styles.paddingScale 1) ]
                        [ Elements.b "Are you sure?"
                        , Elements.p "This will delete all saved passwords and group keys on this device."
                        , if numDevAfter < Data.Sync.maxUsedSecurityLevel syncData then
                            Elements.paragraph []
                                [ Elements.b "WARNING"
                                , Elements.text "If you reset this device, the passwords saved in "
                                , Data.Sync.namedGroupsWithLevel (\l -> l > numDevAfter) syncData
                                    |> Elements.enumeration (Elements.groupIcon True)
                                    |> row []
                                , Elements.text "will no longer be accessible. Better pair one more device and then reset!"
                                ]
                          else
                            none
                        , row [ spacing (Styles.paddingScale 0) ]
                            [ Elements.button (Just (config.toMsg { state | showConfirmReset = False })) "Cancel"
                            , Elements.dangerButton (Just config.onReset) "Reset"
                            ]
                        ]
              else
                Elements.button (Just (config.toMsg { state | showConfirmReset = True })) "Reset Device"
            , el [ height (px 30) ] none
            ]


allowLevel1Txt : String
allowLevel1Txt =
    """
If you select this option, you will be able to save passwords in a group with security level 1.
This is equivalent to no security at all! If someone steals your device, they will be able to read these passwords.
So only use this for storing throwaway accounts, not for anything you care about!
"""
