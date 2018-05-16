module Route exposing (Page(..), fromLocation, modifyUrl, newUrl, href, pageToTitle, hasBackButton)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)


type Page
    = Home
    | Passwords
    | Devices
    | Options
    | Pairing
    | NewPassword
    | Tutorial
      -- | CreateKeyBox
    | ReleaseLog String


route : Parser (Page -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Passwords (s "passwords")
        , Url.map Devices (s "devices")
        , Url.map Options (s "options")
        , Url.map Pairing (s "pairing")
        , Url.map Tutorial (s "tutorial")

        -- , Url.map CreateKeyBox (s "createkeybox")
        , Url.map NewPassword (s "newpassword")
        , Url.map ReleaseLog (s "releaselog" </> string)
        , Url.map (ReleaseLog "") (s "releaselog")
        ]


pageToString : Page -> String
pageToString page =
    let
        end =
            case page of
                Home ->
                    ""

                ReleaseLog s ->
                    if s == "" then
                        "releaselog"
                    else
                        "releaselog/" ++ s

                other ->
                    String.toLower (toString other)
    in
        "#/" ++ end



-- PUBLIC HELPERS --


hasBackButton : Page -> Bool
hasBackButton page =
    case page of
        Tutorial ->
            True

        Pairing ->
            True

        NewPassword ->
            True

        -- CreateKeyBox ->
        --     True
        ReleaseLog _ ->
            True

        _ ->
            False


pageToTitle : Page -> String
pageToTitle page =
    case page of
        Home ->
            "Home"

        NewPassword ->
            "New Password"

        Options ->
            "More"

        Passwords ->
            "Vault"

        -- CreateKeyBox ->
        --     "Create Key Box"
        ReleaseLog s ->
            if s == "" then
                "Release Log"
            else
                s

        other ->
            toString other


href : Page -> Attribute msg
href route =
    Attr.href (pageToString route)


modifyUrl : Page -> Cmd msg
modifyUrl =
    pageToString >> Navigation.modifyUrl


newUrl : Page -> Cmd msg
newUrl =
    pageToString >> Navigation.newUrl


fromLocation : Location -> Page
fromLocation location =
    parseHash route location
        |> Maybe.withDefault Home
