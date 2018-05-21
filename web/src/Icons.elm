module Icons exposing (..)

import Element exposing (..)
import Styles
import FeatherIcons
import Loader
import Icons.Svg


type alias Icon =
    FeatherIcons.Icon


home =
    -- .home, .grid
    FeatherIcons.home


passwords =
    -- TODO: make better icon
    FeatherIcons.lock


devices =
    Icons.Svg.devices


chestClose =
    Icons.Svg.chestClose


chestOpen =
    Icons.Svg.chestOpen


options =
    -- ,.sliders , .menu, .list, .alignJustify
    FeatherIcons.settings


locked =
    FeatherIcons.lock


unlocked =
    FeatherIcons.unlock


delete =
    FeatherIcons.trash2


back =
    FeatherIcons.chevronLeft


arrowUp =
    FeatherIcons.chevronsUp


arrowDown =
    FeatherIcons.chevronsDown


eye =
    FeatherIcons.eye


eyeOff =
    FeatherIcons.eyeOff


clipboard =
    FeatherIcons.clipboard


more =
    FeatherIcons.plus


offline =
    FeatherIcons.cloudOff


close =
    FeatherIcons.x


loading =
    el [] (Element.html <| Loader.loaderWithOptions { loaderOptions | color = Styles.black })


loaderOptions =
    Loader.defaultOptions


small icon =
    icon
        |> FeatherIcons.withSize 18
        |> toElement


normal icon =
    icon
        |> FeatherIcons.withSize 24
        |> toElement


svgIcon description path =
    image [] { src = path, description = description }


toElement : FeatherIcons.Icon -> Element msg
toElement icon =
    icon
        |> FeatherIcons.toHtml []
        |> html
        |> el []
