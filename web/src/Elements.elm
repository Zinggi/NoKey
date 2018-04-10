module Elements exposing (..)

import Html.Attributes as Attr
import Html.Events
import Json.Decode as JD
import Element exposing (..)
import Element.Input as Input
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Styles
import HashIcon
import Icons
import Data exposing (..)
import Route exposing (Page(..))


-- attributes


onEnter : msg -> Attribute msg
onEnter =
    onKey 13


onKey : Int -> msg -> Element.Attribute msg
onKey desiredCode msg =
    let
        decode code =
            if code == desiredCode then
                JD.succeed msg
            else
                JD.fail "Not the key"

        isKey =
            JD.field "which" JD.int
                |> JD.andThen decode
    in
        htmlAttribute <|
            Html.Events.onWithOptions "keyup"
                { stopPropagation = False
                , preventDefault = True
                }
                isKey



-- Wrappers


inputGroup : String -> List (Element msg) -> Element msg
inputGroup heading contents =
    column [ padding (Styles.scaled 1), spacing (Styles.paddingScale 1), alignLeft ]
        (el Styles.groupHeading (h4 heading) :: contents)


container : List (Element msg) -> Element msg
container contents =
    column [ padding (Styles.paddingScale 2), spacing (Styles.paddingScale 1) ] contents


miniPage : List (Element msg) -> Element msg
miniPage children =
    column [ spacing (Styles.paddingScale 1) ] children


buttonRow : List (Attribute msg) -> List (Element msg) -> Element msg
buttonRow attrs btns =
    row (padding (Styles.paddingScale 3) :: attrs) (List.intersperse spacer btns)


card : List (Attribute msg) -> List (Element msg) -> Element msg
card attr children =
    column
        ([ Background.color Styles.white
         , padding (Styles.scaled 1)
         , Border.shadow { offset = ( 1, 1 ), blur = 3, size = 0, color = Styles.shadowColor }

         -- 1px 1px 3px 0px ;
         ]
            ++ attr
        )
        children



-- Element wrappers


wrapInBorder : Element msg -> Element msg
wrapInBorder ele =
    el [ Border.width 1, Border.rounded 4, padding (Styles.paddingScale 1) ] ele


withLabel : String -> Element msg -> Element msg
withLabel label ele =
    row []
        [ el [ width (fillPortion 1) ] (text label)
        , el [ width (fillPortion 3) ] ele
        ]


{-| Take a list [a,b,c] and add commas and 'and's in between.

    enumeration text ["carrot", "apple", "tomato"] ->
        List.map text ["carrot", ",", "apple", "and", "tomato"]

-}
enumeration : (a -> Element msg) -> List a -> List (Element msg)
enumeration f xs =
    case xs of
        [] ->
            []

        [ x ] ->
            [ f x ]

        [ x, y ] ->
            [ f x, text "and", f y ]

        x :: other ->
            f x :: text "," :: enumeration f other



-- Elements


hashIcon : String -> Element msg
hashIcon =
    html << HashIcon.iconFromStringWithBrands 48 2.5


checkBox : (Bool -> msg) -> Bool -> String -> Bool -> Element msg
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
                          ]
                        , if checked then
                            [ inFront (el (paddingXY (Styles.paddingScale -6) (Styles.paddingScale 2) :: box) (text "✔")) ]
                          else
                            []
                        ]
                    )
                    empty
        , checked = checked
        , label = Input.labelRight [] (text label)
        }


box : List (Attribute msg)
box =
    [ width (px (Styles.scaled 1)), height (px (Styles.scaled 1)) ]


text : String -> Element msg
text txt =
    el [ Font.size (Styles.scaled 1), alignLeft ] (Element.text txt)


p : String -> Element msg
p txt =
    paragraph [] [ text txt ]


italicText : String -> Element msg
italicText txt =
    el [ Font.size (Styles.scaled 1), alignLeft, Font.italic ] (Element.text txt)


h1 : String -> Element msg
h1 txt =
    el [ Font.size (Styles.scaled 4), Font.bold, alignLeft ] (Element.text txt)


h2 : String -> Element msg
h2 txt =
    el [ Font.size (Styles.scaled 3), Font.bold, alignLeft ] (Element.text txt)


h3 : String -> Element msg
h3 txt =
    el [ Font.size (Styles.scaled 2), Font.bold, alignLeft ] (Element.text txt)


h4 : String -> Element msg
h4 txt =
    el [ Font.size (Styles.scaled 1), Font.bold, alignLeft ] (Element.text txt)


b : String -> Element msg
b txt =
    el [ Font.bold ] (text txt)


pageButton : msg -> Bool -> Page -> Element msg
pageButton onPress isActive page =
    Input.button [ width fill ]
        { onPress = Just onPress
        , label =
            column
                (if isActive then
                    [ Font.color Styles.accentColor

                    {- TODO: make this a button -}
                    ]
                 else
                    []
                )
                [ el [ centerX ] (pageToIcon page)
                , el [ centerX ] (Route.pageToTitle page |> h4)
                ]
        }


pageToIcon : Page -> Element msg
pageToIcon page =
    case page of
        Home ->
            Icons.dashBoard

        Passwords ->
            Icons.passwords

        Devices ->
            Icons.devices

        Options ->
            Icons.options

        _ ->
            empty


primaryButton : Maybe msg -> String -> Element msg
primaryButton onPress txt =
    Input.button []
        { label =
            el
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
        , onPress = onPress
        }


button : Maybe msg -> String -> Element msg
button onPress txt =
    Input.button []
        { label =
            el
                (padding (Styles.paddingScale 1)
                    :: (case onPress of
                            Nothing ->
                                Styles.disabledBorder

                            Just _ ->
                                Styles.borderStyle
                       )
                )
                (text txt)
        , onPress = onPress
        }


delete onPress =
    Input.button []
        { label =
            el (padding (Styles.paddingScale 1) :: Styles.borderStyle)
                (row [ spacing (Styles.paddingScale 0) ] [ Icons.delete, text "Delete" ])
        , onPress = Just onPress
        }


customButton : Maybe msg -> Element msg -> Element msg
customButton onPress inner =
    Input.button []
        { label =
            el
                (padding (Styles.paddingScale 1)
                    :: (case onPress of
                            Nothing ->
                                Styles.disabledBorder

                            Just _ ->
                                Styles.borderStyle
                       )
                )
                inner
        , onPress = onPress
        }


toggleMoreButton : (Bool -> msg) -> String -> String -> Bool -> Element msg
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
        }


groupIcon : Bool -> GroupId -> Element msg
groupIcon isLocked ( level, _ ) =
    row [ width shrink ]
        [ h3 (toString level)
        , if isLocked then
            Icons.locked
          else
            Icons.unlocked
        ]


avatar : List (Attribute msg) -> Data.Device -> Element msg
avatar attrs { id, name, postFix } =
    row (spacing (Styles.scaled 1) :: attrs)
        [ hashIcon id
        , paragraph [ alignLeft, width fill, spacing (Styles.scaled -2) ]
            [ helperMayName name, helperMayIdPart postFix ]
        ]


helperMayName name =
    if name == "" then
        italicText "Unnamed"
    else
        text name


helperMayIdPart idPart =
    if idPart /= "" then
        italicText ("(" ++ idPart ++ ")")
    else
        empty


myAvatar : (String -> msg) -> String -> ( String, String ) -> List (Attribute msg) -> Element msg
myAvatar onSetDeviceName id ( name, idPart ) attrs =
    row (spacing (Styles.scaled 1) :: attrs)
        [ hashIcon id
        , textInput (Just onSetDeviceName) "Name your device.." name
        , helperMayIdPart idPart
        ]


clampedNumberInput : (Int -> msg) -> ( Int, Int, Int ) -> Int -> Element msg
clampedNumberInput onChange ( min, default, max ) n =
    let
        m =
            clamp min max n

        toNumber s =
            String.toInt s |> Result.map (clamp min max) |> Result.withDefault default |> onChange

        inp length t =
            Input.text
                [ htmlAttribute (Attr.type_ t)
                , htmlAttribute (Attr.min (toString min))
                , htmlAttribute (Attr.max (toString max))
                , htmlAttribute (Attr.style [ ( "background", "none" ), ( "border", "none" ) ])
                , alignLeft
                , width (px (Styles.scaled length))
                , height (px (Styles.scaled 2))
                , padding 0
                , Font.size (Styles.scaled 1)
                , htmlAttribute (Attr.disabled (min == max))
                ]
                { onChange = Just toNumber
                , text = toString m
                , label = Input.labelLeft [] empty
                , placeholder = Nothing
                }
    in
        row [ alignLeft ] [ inp 8 "range", inp 4 "number" ]


line : Element msg
line =
    el [ height (px 1), Background.color Styles.black, width fill ] empty


spacer : Element msg
spacer =
    el [ width fill ] empty


inputWithLabel : Maybe (String -> msg) -> String -> String -> String -> Element msg
inputWithLabel onChange label placeholder value =
    row []
        [ el [ width (fillPortion 1) ] (text label)
        , inputHelper Input.text [ width (fillPortion 3) ] onChange placeholder value
        ]


search : (String -> msg) -> String -> Element msg
search onChange value =
    inputHelper Input.search [] (Just onChange) "search" value


textInput : Maybe (String -> msg) -> String -> String -> Element msg
textInput onChange placeholder value =
    inputHelper Input.text [] onChange placeholder value


passwordEntry : Maybe (String -> msg) -> String -> Bool -> String -> Element msg
passwordEntry onChange label shouldShow value =
    row []
        [ el [ width (fillPortion 1), htmlAttribute (Attr.type_ "password"), htmlAttribute (Attr.name "password") ] (text label)
        , password [ width (fillPortion 3) ] onChange shouldShow value
        ]


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


password attr onChange shouldShow value =
    Input.currentPassword
        (attr ++ [ padding 0, width shrink ])
        { onChange = onChange
        , text = value
        , label = Input.labelLeft [ padding 0 ] empty
        , placeholder =
            Just <| Input.placeholder [ padding (Styles.paddingScale 1) ] (text "********")
        , show = shouldShow
        }


inputHelper fn attr onChange placeholder value =
    fn
        (attr
            ++ [ padding 0
               , htmlAttribute (Attr.disabled (onChange == Nothing))
               ]
        )
        { onChange = onChange
        , text = value
        , label = Input.labelLeft [ padding 0 ] empty
        , placeholder =
            if placeholder == "" then
                Nothing
            else
                Just <| Input.placeholder [ padding (Styles.paddingScale 1) ] (text placeholder)
        }
