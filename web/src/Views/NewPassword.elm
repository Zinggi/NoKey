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
        ( Data.Sync.minSecurityLevel model.syncData, Data.Sync.numberOfAvailableDevices model.syncData )


newSiteForm : Views.PasswordGenerator.State -> PasswordMetaData -> ( Int, Int ) -> Element Msg
newSiteForm requirementsState entry ( minSecLevel, maxSecurityLevel ) =
    column [ spacing (Styles.paddingScale 3) ]
        [ Elements.keyedInputText (toString entry.counter) [] (Just SiteNameChanged) { placeholder = "example.com", label = "Site" } entry.siteName
        , Elements.keyedInputText ("a" ++ toString entry.counter) [] (Just UserNameChanged) { placeholder = "", label = "Login" } entry.userName

        -- TODO: offer to create a named group
        , Elements.clampedNumberInput SecurityLevelChanged "Security Level" ( minSecLevel, 2, min 5 maxSecurityLevel ) entry.securityLevel
        , el [ height shrink, width fill ]
            (Views.PasswordGenerator.view AddPassword
                (entry.siteName /= "" && entry.userName /= "")
                NewPasswordRequirements
                requirementsState
            )
        ]
