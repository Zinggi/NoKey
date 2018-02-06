module Elements exposing (..)

import Color
import Html.Attributes as Attr
import Element exposing (..)
import Element.Input as Input
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Styles


checkBox onChange isDisabled label checked =
    Input.checkbox []
        { onChange =
            if isDisabled then
                Nothing
            else
                Just onChange
        , icon =
            Just <|
                el
                    (List.concat
                        [ box
                        , [ padding (Styles.paddingScale 2) ]
                        , Styles.borderStyle
                        , [ (if isDisabled then
                                Styles.disabledColor
                             else if checked then
                                Styles.accentColor
                             else
                                Styles.white
                            )
                                |> Background.color
                          , inFront checked (el (paddingXY (Styles.paddingScale -6) (Styles.paddingScale 2) :: box) (text "✔"))
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


text txt =
    el [ Font.size (Styles.scaled 1), alignLeft ] (Element.text txt)


italicText txt =
    el [ Font.size (Styles.scaled 1), alignLeft, Font.italic ] (Element.text txt)


h1 txt =
    el [ Font.size (Styles.scaled 4), Font.bold, alignLeft ] (Element.text txt)


h2 txt =
    el [ Font.size (Styles.scaled 3), Font.bold, alignLeft ] (Element.text txt)


h3 txt =
    el [ Font.size (Styles.scaled 2), Font.bold, alignLeft ] (Element.text txt)


h4 txt =
    el [ Font.size (Styles.scaled 1), Font.bold, alignLeft ] (Element.text txt)


button onPress txt =
    Input.button []
        { label =
            (el
                (padding (Styles.paddingScale 1)
                    :: (Background.color <|
                            case onPress of
                                Nothing ->
                                    Styles.disabledColor

                                Just _ ->
                                    Styles.accentColor
                       )
                    :: Styles.borderStyle
                )
                (text txt)
            )
        , onPress = onPress
        }


inputGroup heading contents =
    column [ padding (Styles.scaled 1), spacing (Styles.paddingScale 1), alignLeft ]
        ((el Styles.groupHeading (h4 (heading))) :: contents)


container contents =
    column [ padding (Styles.paddingScale 2), spacing (Styles.paddingScale 1) ] contents


toggleMoreButton onOpen labelClosed labelOpen isOpen =
    Input.checkbox []
        { onChange = Just onOpen
        , icon =
            Just <|
                text
                    (if isOpen then
                        "⌃"
                     else
                        "⌄"
                    )
        , checked = isOpen
        , label =
            Input.labelRight []
                (text
                    (if isOpen then
                        labelOpen
                     else
                        labelClosed
                    )
                )
        , notice = Nothing
        }


clampedNumberInput : (Int -> msg) -> ( Int, Int, Int ) -> Int -> Element msg
clampedNumberInput onChange ( min, default, max ) n =
    let
        m =
            clamp min max n

        toNumber s =
            String.toInt s |> Result.map (clamp min max) |> Result.withDefault default |> onChange

        inp length t =
            Input.text
                [ attribute (Attr.type_ t)
                , attribute (Attr.min (toString min))
                , attribute (Attr.max (toString max))
                , attribute (Attr.style [ ( "background", "none" ), ( "border", "none" ) ])
                , alignLeft
                , width (px (Styles.scaled length))
                , height (px (Styles.scaled 2))
                , padding 0
                , Font.size (Styles.scaled 1)
                , attribute (Attr.disabled (min == max))
                ]
                { onChange = Just toNumber
                , text = toString m
                , label = Input.labelLeft [] (empty)
                , placeholder = Nothing
                , notice = Nothing
                }
    in
        row [ alignLeft ]
            [ inp 8 "range", inp 4 "number" ]


line =
    el [ height (px 1), Background.color Styles.black, width fill ] empty


wrapInBorder ele =
    el [ Border.width 1, Border.rounded 4, padding (Styles.paddingScale 1) ] ele


withLabel label ele =
    row []
        [ el [ width (fillPortion 1) ] (text label)
        , el [ width (fillPortion 3) ] ele
        ]


buttonRow attrs btns =
    row ([ padding (Styles.paddingScale 3) ] ++ attrs) (List.intersperse spacer btns)


spacer =
    el [ width fill ] empty


inputWithLabel onChange label placeholder value =
    row []
        [ el [ width (fillPortion 1) ] (text label)
        , inputHelper Input.text [ width (fillPortion 3) ] onChange placeholder value
        ]


textInput onChange placeholder value =
    inputHelper Input.text [] onChange placeholder value


passwordEntry onChange label value =
    row []
        [ el [ width (fillPortion 1), attribute (Attr.type_ "password"), attribute (Attr.name "password") ] (text label)
        , inputHelper Input.currentPassword [ width (fillPortion 3) ] onChange "********" value
        ]


inputHelper fn attr onChange placeholder value =
    fn
        (attr ++ [ padding 0, attribute (Attr.disabled (onChange == Nothing)) ])
        { onChange = onChange
        , text = value
        , label = Input.labelLeft [ padding 0 ] (empty)
        , placeholder =
            if placeholder == "" then
                Nothing
            else
                Just <| Input.placeholder [ padding (Styles.paddingScale 1) ] (text placeholder)
        , notice = Nothing
        }


table : List ( String, a -> Element msg ) -> List a -> Element msg
table headers data =
    Element.table [ spacing (Styles.paddingScale 1) ]
        { data = data
        , columns =
            List.map
                (\( h, fn ) ->
                    { header = h4 h
                    , view = fn
                    }
                )
                headers
        }


miniPage title children =
    column [ spacing (Styles.paddingScale 1) ] (h2 title :: children)
