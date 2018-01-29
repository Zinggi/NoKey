module Elements exposing (..)

import Color
import Element exposing (..)
import Element.Input as Input
import Element.Background as Background
import Element.Font as Font
import Styles


checkBox onChange label checked =
    Input.checkbox []
        { onChange = Just onChange
        , icon =
            Just <|
                el
                    (List.concat
                        [ box
                        , [ (if checked then
                                Styles.accentColor
                             else
                                Styles.white
                            )
                                |> Background.color
                          , inFront checked (el box (text "✔"))
                          ]
                        ]
                    )
                    empty
        , checked = checked
        , label = Input.labelRight [] (text label)
        , notice = Nothing
        }


box =
    [ width (px (Styles.scaled 1)), height (px (Styles.scaled 1)) ]


inputGroup heading contents =
    column [ padding (Styles.scaled 1), spacing (Styles.scaled -2), alignLeft ]
        ((el Styles.groupHeading (text (heading ++ ":"))) :: contents)


input onChange label value =
    Input.text
        []
        { onChange = Just onChange
        , text = value
        , label = Input.labelLeft [] (el [ width (px 200) ] (text label))
        , placeholder = Nothing
        , notice = Nothing
        }
