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
import Helper


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


buttonRow : List (Attribute msg) -> List (Element msg) -> Element msg
buttonRow attrs btns =
    row (padding (Styles.paddingScale 3) :: attrs) (List.intersperse spacer btns)


card : Int -> List (Attribute msg) -> List (Element msg) -> Element msg
card depth attr children =
    column
        ([ -- Background.color Styles.white
           height shrink
         , Background.color Styles.altBackgroundColor
         , spacing (Styles.paddingScale 1)
         , padding (Styles.scaled 1)

         -- 1px 1px 3px 0px ;
         ]
            ++ attr
            ++ Styles.cardShadow depth
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
    Helper.intersperseLastOneDifferent f (text ",") (text "and") xs


stripedList : List (Attribute msg) -> List (Attribute msg) -> List (Element msg) -> Element msg
stripedList attrCo attrCh children =
    column attrCo
        (List.indexedMap
            (\i ->
                el
                    (Background.color
                        (if i % 2 == 0 then
                            Styles.altBackgroundColor
                         else
                            Styles.alt2BackgroundColor
                        )
                        :: attrCh
                    )
            )
            children
        )


expandable : msg -> Bool -> List (Attribute msg) -> Element msg -> Element msg -> Element msg
expandable onExpand isExpanded attrs header content =
    let
        title isExpanded =
            Input.button attrs
                { label =
                    row
                        (attrs
                            ++ if isExpanded then
                                Styles.cardShadow 2
                               else
                                []
                        )
                        [ header
                        , if isExpanded then
                            Icons.arrowUp
                          else
                            Icons.arrowDown
                        ]
                , onPress = Just onExpand
                }
    in
        if isExpanded then
            column attrs [ title isExpanded, content ]
        else
            title isExpanded



-- Elements


siteLogo : String -> Element msg
siteLogo siteName =
    let
        letter =
            String.left 1 siteName
                |> String.toUpper
    in
        -- TODO: maybe use a service to get nice logos.
        -- For now just use the first letter on a nice background
        el
            [ Border.rounded 20
            , width (px 28)
            , height (px 28)
            , Background.color Styles.foregroundColor
            , Font.size 24
            , Font.center
            ]
            (Element.text letter)


hashIcon : String -> Element msg
hashIcon txt =
    el [ width (px 48) ] (html <| HashIcon.iconFromStringWithBrands 48 2.5 txt)


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


copyToClipboard : msg -> (() -> String) -> Element msg
copyToClipboard msg txt =
    Input.button [ htmlAttribute (Attr.attribute "onClick" (copyToClipboardHack ++ ";copyToClipboard(" ++ toString (txt ()) ++ ");")) ]
        { label =
            el
                (padding (Styles.paddingScale 1)
                    :: Styles.borderStyle
                )
                (text "Copy to clipboard")
        , onPress = Just msg
        }


copyToClipboardHack =
    """
function copyToClipboard(text) {
    if (window.clipboardData && window.clipboardData.setData) {
        // IE specific code path to prevent textarea being shown while dialog is visible.
        return clipboardData.setData("Text", text);

    } else if (document.queryCommandSupported && document.queryCommandSupported("copy")) {
        var textarea = document.createElement("textarea");
        textarea.textContent = text;
        textarea.style.position = "fixed";  // Prevent scrolling to bottom of page in MS Edge.
        document.body.appendChild(textarea);
        textarea.select();
        try {
            return document.execCommand("copy");  // Security exception may be thrown by some browsers.
        } catch (ex) {
            console.warn("Copy to clipboard failed.", ex);
            return false;
        } finally {
            document.body.removeChild(textarea);
        }
    }
}
"""


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
                    ]
                 else
                    []
                )
                [ el [ centerX ] (pageToIcon page)
                , if True || isActive then
                    el [ centerX ] (Route.pageToTitle page |> h4)
                  else
                    empty
                ]
        }


backButton : msg -> Element msg
backButton msg =
    Input.button [ width shrink ]
        { onPress = Just msg
        , label = row [] [ Icons.back, text "Back" ]
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
        , row []
            [ textInput (Just onSetDeviceName) "Name your device.." name
            , helperMayIdPart idPart
            ]
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
        row [ alignLeft ]
            [ inp 8 "range"
            , inp 4 "number"
            ]


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
        (attr
            ++ [ padding 0
               , width shrink
               , htmlAttribute
                    (Attr.size
                        (if shouldShow then
                            String.length value
                         else
                            5
                        )
                    )
               ]
        )
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
               , htmlAttribute (Attr.size (max 15 (String.length value)))
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
