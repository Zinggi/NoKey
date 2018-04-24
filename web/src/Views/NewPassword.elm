module Views.NewPassword exposing (view)

import Dict
import Elements
import Element exposing (..)
import Styles
import Data.Sync
import Views.PasswordGenerator
import Data.PasswordMeta exposing (PasswordMetaData)
import Model exposing (Msg(..))


view model =
    newSiteForm model.requirementsState
        model.newSiteEntry
        (Data.Sync.currentGroupId model.newSiteEntry.securityLevel model.syncData)
        (Data.Sync.knownIds model.syncData |> Dict.size)


newSiteForm : Views.PasswordGenerator.State -> PasswordMetaData -> String -> Int -> Element Msg
newSiteForm requirementsState entry currentGroupId maxSecurityLevel =
    column [ spacing (Styles.paddingScale 3) ]
        [ Elements.inputText [] (Just SiteNameChanged) { placeholder = "example.com", label = "Site" } entry.siteName
        , Elements.inputText [] (Just UserNameChanged) { placeholder = "", label = "Login" } entry.userName

        -- TODO: offer to create a named group
        , Elements.clampedNumberInput SecurityLevelChanged "Security Level" ( 2, 2, maxSecurityLevel ) entry.securityLevel
        , el [ height shrink, width fill ] (Views.PasswordGenerator.view (AddPassword currentGroupId) NewPasswordRequirements requirementsState)
        ]
