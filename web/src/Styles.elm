module Styles exposing (..)

import Color
import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import Element.Border as Border
import Html.Attributes as Attr
import Scale


scaled =
    Scale.modular 14 1.414 >> round


paddingScale =
    Scale.modular 6 1.414 >> round



-- Colors


backgroundColor =
    Color.rgb 255 241 222


disabledColor =
    Color.rgb 150 150 150


white =
    Color.rgb 255 255 255


black =
    Color.rgb 0 0 0


thinLineColor =
    Color.rgba 0 0 0 0.25


errorColor =
    Color.rgb 191 36 25


foregroundColor =
    Color.rgb 191 119 25


accentColor =
    Color.rgb 25 152 194


textColor =
    Color.rgb 1 58 76


borderStyle =
    [ Border.width 1
    , Border.color thinLineColor
    , Border.mouseOverColor black
    , Border.rounded 4
    ]


background =
    [ Background.color backgroundColor
    , Font.color textColor
    , normalFont
    , padding (paddingScale 3)
    ]


groupHeading =
    [ normalFont, Font.bold, alignLeft, Font.size (scaled 1) ]


normalFont =
    Font.family
        [ Font.external
            { name = "FiraSans"
            , url = "https://fonts.googleapis.com/css?family=Fira+Sans"
            }
        , Font.sansSerif
        ]


altFont =
    Font.family
        [ Font.external
            { name = "OpenSans"
            , url = "https://fonts.googleapis.com/css?family=Open+Sans"
            }
        , Font.sansSerif
        ]