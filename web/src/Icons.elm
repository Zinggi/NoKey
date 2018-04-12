module Icons exposing (..)

import Element exposing (..)
import Styles
import FeatherIcons
import Loader


dashBoard =
    -- TODO: maybe .home is better
    FeatherIcons.grid |> toElement


passwords =
    -- TODO: make better icon
    FeatherIcons.lock |> toElement


devices =
    -- TODO: make better icon, .smartphone, .tablet
    FeatherIcons.monitor |> toElement


options =
    -- ,.sliders , .menu, .list, .alignJustify
    FeatherIcons.settings |> toElement


locked =
    FeatherIcons.lock |> toElement


unlocked =
    FeatherIcons.unlock |> toElement


delete =
    FeatherIcons.trash2 |> toElement


back =
    FeatherIcons.chevronLeft |> toElement


arrowUp =
    FeatherIcons.chevronsUp |> toElement


arrowDown =
    FeatherIcons.chevronsDown |> toElement


loading =
    el [] (Element.html <| Loader.loaderWithOptions { loaderOptions | color = Styles.black })


loaderOptions =
    Loader.defaultOptions


toElement : FeatherIcons.Icon -> Element msg
toElement icon =
    icon
        |> FeatherIcons.toHtml []
        |> html
        |> el []
