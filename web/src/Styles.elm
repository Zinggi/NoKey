module Styles exposing (..)

import Style exposing (..)
import Style.Color as Color
import Color exposing (..)


type Style
    = Example
    | None


stylesheet : StyleSheet Style c
stylesheet =
    styleSheet
        [ style Example
            [ Color.background blue
            , Color.text white
            ]
        ]
