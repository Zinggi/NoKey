module Styles exposing (..)

import Color
import Element.Font as Font
import Element.Background as Background


backgroundColor =
    Color.rgb 255 241 222


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
