module Route exposing (Page(..), fromLocation, modifyUrl, newUrl, href, pageToTitle)

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


route : Parser (Page -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Passwords (s "passwords")
        , Url.map Devices (s "devices")
        , Url.map Options (s "options")
        , Url.map Pairing (s "pairing")
        ]


pageToString : Page -> String
pageToString page =
    let
        end =
            case page of
                Home ->
                    ""

                Passwords ->
                    "passwords"

                Devices ->
                    "devices"

                Options ->
                    "options"

                Pairing ->
                    "pairing"
    in
        "#/" ++ end



-- PUBLIC HELPERS --


pageToTitle : Page -> String
pageToTitle page =
    case page of
        Home ->
            "Dashboard"

        Passwords ->
            "Passwords"

        Devices ->
            "Devices"

        Options ->
            "Options"

        Pairing ->
            "Pairing"


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
