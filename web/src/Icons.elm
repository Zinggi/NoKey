module Icons exposing (..)

import Element exposing (..)
import Styles
import FeatherIcons
import Loader
import Icons.Svg


home =
    -- .home, .grid
    FeatherIcons.home


passwords =
    -- TODO: make better icon
    FeatherIcons.lock


devices =
    Icons.Svg.devices


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
