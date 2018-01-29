module Styles exposing (..)

import Color
import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import Scale


scaled =
    Scale.modular 16 1.618 >> round


backgroundColor =
    Color.rgb 255 241 222


white =
    Color.rgb 255 255 255


errorColor =
    Color.rgb 191 36 25


foregroundColor =
    Color.rgb 191 119 25


accentColor =
    Color.rgb 25 152 194


textColor =
    Color.rgb 1 58 76


background =
    [ Background.color backgroundColor, Font.color textColor, normalFont ]


groupHeading =
    [ normalFont, Font.bold, alignLeft ]


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
