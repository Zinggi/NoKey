module Styles exposing (..)

import Array
import Color
import Html.Attributes as Attr
import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import Element.Border as Border


scaled =
    modular 14 1.414 >> round


paddingScale =
    modular 6 1.414 >> round


paddingLeft x =
    paddingEach { bottom = 0, left = x, right = 0, top = 0 }



-- Colors


shadowColor =
    Color.rgba 0 0 0 0.37


backgroundColor =
    Color.rgb 255 241 222


altBackgroundColor =
    Color.rgb 255 252 247


alt2BackgroundColor =
    Color.rgb 231 225 218


disabledColor =
    Color.rgb 150 150 150


white =
    Color.rgb 255 255 255


black =
    Color.rgb 0 0 0


transparent =
    Color.rgba 0 0 0 0


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
    , mouseOver [ Border.color black ]
    , Border.rounded 4
    ]


dangerStyle =
    [ Font.color altBackgroundColor
    , Background.color errorColor
    ]


titleHighlight =
    [ Background.color accentColor, Font.color white ]



-- cardShadow =
--     Border.shadow { offset = ( 1, 1 ), blur = 3, size = 0, color = shadowColor }


cardShadow depth =
    -- Source: https://css-tricks.com/snippets/sass/material-shadows-mixin/
    let
        get l =
            Array.get (depth - 1) (Array.fromList l) |> Maybe.withDefault 0

        ( off1, alpha1 ) =
            ( get [ 1.5, 3, 10, 14, 19 ]
            , get [ 0.12, 0.16, 0.19, 0.25, 0.3 ]
            )

        blur1 =
            off1 * 4

        ( off2, blur2, alpha2 ) =
            ( get [ 1.5, 3, 6, 10, 15 ]
            , get [ 1, 3, 3, 5, 6 ]
            , get [ 0.24, 0.23, 0.23, 0.22, 0.22 ]
            )
    in
        [ Border.shadow { offset = ( 0, off1 ), blur = blur1, size = 0, color = Color.rgba 0 0 0 alpha1 }
        , Border.shadow { offset = ( 0, off2 ), blur = blur2, size = 0, color = Color.rgba 0 0 0 alpha2 }
        , htmlAttribute (Attr.style [ ( "z-index", toString depth ) ])
        ]


disabledBorder =
    [ Border.width 1
    , Border.color disabledColor
    , Border.rounded 4
    ]


background =
    [ Background.color backgroundColor
    , Font.color textColor
    , normalFont
    ]


grayedOutIf b =
    if b then
        [ Font.color disabledColor ]
    else
        []


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


unselectable : Attribute msg
unselectable =
    htmlAttribute
        (Attr.style
            [ ( "-webkit-touch-callout", "none" )
            , ( "-webkit-user-select", "none" )
            , ( "-khtml-user-select", "none" )
            , ( "-moz-user-select", "none" )
            , ( "-ms-user-select", "none" )
            , ( "user-select", "none" )
            ]
        )


selectable : Attribute msg
selectable =
    htmlAttribute
        (Attr.style
            [ ( "-webkit-touch-callout", "all" )
            , ( "-webkit-user-select", "all" )
            , ( "-khtml-user-select", "all" )
            , ( "-moz-user-select", "all" )
            , ( "-ms-user-select", "all" )
            , ( "user-select", "all" )
            ]
        )
