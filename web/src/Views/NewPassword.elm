module Views.NewPassword exposing (view)

import Dict
import Elements
import Element exposing (..)
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
    column []
        [ Elements.inputWithLabel (Just SiteNameChanged) "Site" "example.com" entry.siteName
        , Elements.inputWithLabel (Just UserNameChanged) "Login" "" entry.userName
        , Elements.text "Security Level"
        , -- TODO: replace with a 'radio button', showing existing groups and offer to create a new group
          Elements.clampedNumberInput SecurityLevelChanged ( 2, 2, maxSecurityLevel ) entry.securityLevel
        , Elements.text "Password"
        , Views.PasswordGenerator.view (AddPassword currentGroupId) NewPasswordRequirements requirementsState
        ]
