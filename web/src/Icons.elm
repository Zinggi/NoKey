module Icons exposing (..)

import Element exposing (..)
import Styles
import FeatherIcons
import Loader


locked =
    FeatherIcons.lock |> toElement


unlocked =
    FeatherIcons.unlock |> toElement


delete =
    FeatherIcons.trash2 |> toElement


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
