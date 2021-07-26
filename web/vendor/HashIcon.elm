module HashIcon
    exposing
        ( estimateNumberOfPossibleIcons
        , iconWithColor
        , iconFromString
        , allColorCombinations
        , estimateEntropy
        , iconsFromString
        , iconFromStringWithSeed
        , iconsFromStringWithSeed
        , randomIcon
        , CutOffRatio
        , Size
        , iconFromStringWithBrands
        , iconsFromStringWithBrands
        , iconWithColorWithBrands
        , estimateNumberOfPossibleIconsWithBrands
        , estimateEntropyWithBrands
        , randomIconWithBrands
        , iconFromStringWithSeedWithBrands
        , iconsFromStringWithSeedWithBrands
        )

{-|


# Icons

All functions are available in two variants, normal or with brands.
The ones with brands add a bit more entropy, but you might not want to use icons of other brands in your product.

@docs CutOffRatio, Size

@docs iconFromString, iconsFromString, iconWithColor


## Explore

@docs estimateNumberOfPossibleIcons, estimateEntropy, allColorCombinations


## Other

@docs randomIcon, iconFromStringWithSeed, iconsFromStringWithSeed


## With Brands

@docs iconFromStringWithBrands, iconsFromStringWithBrands, iconWithColorWithBrands
@docs estimateNumberOfPossibleIconsWithBrands, estimateEntropyWithBrands
@docs randomIconWithBrands, iconFromStringWithSeedWithBrands, iconsFromStringWithSeedWithBrands

-}

import Color exposing (Color)
import Html exposing (Html, input, div)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random.Pcg as Random exposing (Generator)
import Color.Accessibility exposing (contrastRatio)
import Color.Convert exposing (colorToHex)
import Murmur3
import HashIcon.Constants as Constants exposing (icons, brands, colors, fallbackIcon)


{-| The ratio controls which color contrast is acceptable for us.
A smaller ratio allows for more combinations, but many of them might be ugly.
These values seem to provide a quite good trade-off between good icons and still pretty high collision resistance:

  - 1.4 -> An ok chance of getting a good icon, but there are quite a few ugly ones as well.
    We get about 21 bits of entropy with this.

  - 2.1 -> Mostly good looking icons. Ca 20 bits.

  - 3.4 -> Very good looking icons. Ca. 19 bits.

  - 8.4 -> Almost no bad one. Ca. 16 bits

-}
type alias CutOffRatio =
    Float


{-| -}
type alias Size =
    Int


{-| Can be used to generate random icons
-}
randomIcon : Size -> CutOffRatio -> Generator (Html msg)
randomIcon size ratio =
    Random.map2 (,) (goodColors ratio) randomFontIcon
        |> Random.map (\( c1c2c3, icon ) -> constantIcon size c1c2c3 icon)


{-| -}
randomIconWithBrands : Size -> CutOffRatio -> Generator (Html msg)
randomIconWithBrands size ratio =
    Random.map2 (,) (goodColors ratio) randomFontIconBrands
        |> Random.map (\( c1c2c3, icon ) -> constantIcon size c1c2c3 icon)


{-| Turns some text into an icon.
Given the same string, the icon will always be the same.

    iconFromString 120 1.8 "hash icon"

![](https://github.com/Zinggi/elm-hash-icon/raw/master/examples/imgs/hashIcon.svg?sanitize=true)

-}
iconFromString : Size -> CutOffRatio -> String -> Html msg
iconFromString =
    iconFromStringWithSeed 42


{-| -}
iconFromStringWithBrands : Size -> CutOffRatio -> String -> Html msg
iconFromStringWithBrands =
    iconFromStringWithSeedWithBrands 42


{-| Same as `iconFromString`, but allows you to adjust the seed used in the internal Murmur3 hash function.
You probably don't need this.
-}
iconFromStringWithSeed : Int -> Size -> CutOffRatio -> String -> Html msg
iconFromStringWithSeed seed size ratio string =
    Random.step (randomIcon size ratio) (Random.initialSeed (Murmur3.hashString seed string))
        |> Tuple.first


{-| -}
iconFromStringWithSeedWithBrands : Int -> Size -> CutOffRatio -> String -> Html msg
iconFromStringWithSeedWithBrands seed size ratio string =
    Random.step (randomIconWithBrands size ratio) (Random.initialSeed (Murmur3.hashString seed string))
        |> Tuple.first


{-| Same as `iconFromString`, but gives you a list of icons.
Useful to get more entropy if you need higher collision resistance.

    iconsFromString 120 9.5 4 "hash icon"

![](https://github.com/Zinggi/elm-hash-icon/raw/master/examples/imgs/hashIcons.svg?sanitize=true)

-}
iconsFromString : Size -> CutOffRatio -> Int -> String -> List (Html msg)
iconsFromString size ratio num string =
    List.map (\i -> iconFromStringWithSeed i size ratio string) (List.range 1 num)


{-| -}
iconsFromStringWithBrands : Size -> CutOffRatio -> Int -> String -> List (Html msg)
iconsFromStringWithBrands size ratio num string =
    List.map (\i -> iconFromStringWithSeedWithBrands i size ratio string) (List.range 1 num)


{-| Same as `iconsFromString`, but allows you to specify an offset to the seed used
in the internal Murmur3 hash function. You probably don't need this.
-}
iconsFromStringWithSeed : Int -> Size -> CutOffRatio -> Int -> String -> List (Html msg)
iconsFromStringWithSeed seedOffset size ratio num string =
    List.map (\i -> iconFromStringWithSeed (i + seedOffset) size ratio string) (List.range 1 num)


{-| -}
iconsFromStringWithSeedWithBrands : Int -> Size -> CutOffRatio -> Int -> String -> List (Html msg)
iconsFromStringWithSeedWithBrands seedOffset size ratio num string =
    List.map (\i -> iconFromStringWithSeedWithBrands (i + seedOffset) size ratio string) (List.range 1 num)


{-| Shows you the number of possible icons for a given ratio.
The higher the more collision resistant.

Note: Since I wasn't sure if I got the combinatorics right,
I called it estimate, but it might also be the correct number \U0001f937

-}
estimateNumberOfPossibleIcons : CutOffRatio -> Int
estimateNumberOfPossibleIcons ratio =
    List.length (allColorCombinations ratio) * (465 + 115)


{-| -}
estimateNumberOfPossibleIconsWithBrands : CutOffRatio -> Int
estimateNumberOfPossibleIconsWithBrands ratio =
    List.length (allColorCombinations ratio) * (465 + 115 + 324)


{-| The same as `estimateNumberOfPossibleIcons`, but in bits.
-}
estimateEntropy : CutOffRatio -> Float
estimateEntropy ratio =
    logBase 2 (toFloat <| estimateNumberOfPossibleIcons ratio)


{-| -}
estimateEntropyWithBrands : CutOffRatio -> Float
estimateEntropyWithBrands ratio =
    logBase 2 (toFloat <| estimateNumberOfPossibleIconsWithBrands ratio)


{-| Display an icon with a fixed color scheme. Very useful in combination with allColorCombinations.
-}
iconWithColor : Size -> ( Color, Color, Color ) -> String -> Html msg
iconWithColor size c1c2c3 string =
    let
        ( icon, _ ) =
            Random.step randomFontIcon (Random.initialSeed (Murmur3.hashString 42 string))
    in
        constantIcon size c1c2c3 icon


{-| -}
iconWithColorWithBrands : Size -> ( Color, Color, Color ) -> String -> Html msg
iconWithColorWithBrands size c1c2c3 string =
    let
        ( icon, _ ) =
            Random.step randomFontIconBrands (Random.initialSeed (Murmur3.hashString 42 string))
    in
        constantIcon size c1c2c3 icon


{-| Gives you all possible colors combinations that can occur for a given ratio, sorted by contrast ratio,
e.g from worst combination to best.
Peeking at the top values of this list allows you to find a nice value for the ratio.
Combine this with `iconWithColor` to look at the resulting icons.
-}
allColorCombinations : CutOffRatio -> List ( Color, Color, Color )
allColorCombinations ratio =
    List.concatMap (\c1 -> List.concatMap (\c2 -> List.map (\c3 -> ( c1, c2, c3 )) (white :: colors)) colors) (white :: colors)
        |> List.filter
            (\( c1, c2, c3 ) ->
                contrastRatio c1 c3 > Basics.max 1 ratio && (contrastRatio c1 c2 > Basics.max 1 ratio || c1 == c2)
            )
        |> List.sortBy
            (\( c1, c2, c3 ) ->
                Basics.min (contrastRatio c1 c3)
                    (contrastRatio c1 c2
                        |> (\r ->
                                if r == 1 then
                                    100
                                else
                                    r
                           )
                    )
            )


{-| tested with svg width=120 height=120
rect x=10 y=10 width=100 height=100 rx=12 ry=12 strokeWidth=12
foreignObject x=20 y=20 width=80 height=80

    -> 120 -> (120, 120*1/12, 120*10/12, 120/10, 120*2/12, 120*8/12)

-}
constantIcon : Size -> ( Color, Color, Color ) -> Html msg -> Html msg
constantIcon size ( c1, c2, c3 ) icon =
    svg
        [ width (toString size), height (toString size), viewBox "0 0 120 120" ]
        [ rect
            [ x "10"
            , y "10"
            , width "100"
            , height "100"
            , rx "12"
            , ry "12"
            , fill <| colorToHex c1
            , stroke <| colorToHex c2
            , strokeWidth "12"
            ]
            []
        , foreignObject
            [ x "20"
            , y "20"
            , width "80"
            , height "80"
            , Svg.Attributes.style ("color: " ++ colorToHex c3)
            ]
            [ icon ]
        ]


randomFontIcon : Generator (Html msg)
randomFontIcon =
    Random.sample icons |> Random.map (Maybe.withDefault fallbackIcon)


randomFontIconBrands : Generator (Html msg)
randomFontIconBrands =
    Random.sample (icons ++ brands) |> Random.map (Maybe.withDefault fallbackIcon)


randomColor : Color -> List Color -> Generator Color
randomColor fallback colors =
    Random.sample colors |> Random.map (Maybe.withDefault fallback)


{-| Here we make sure the icon is readable.
For this we use the contrast ratio between the background and icon color.
-}
readableColors : Float -> Color -> List Color -> List Color
readableColors r backgroundColor colors =
    List.filter (\col -> contrastRatio backgroundColor col > (Basics.max 1 r)) colors


goodColors : Float -> Generator ( Color, Color, Color )
goodColors r =
    (randomColor white (white :: colors))
        |> Random.andThen
            (\cBack ->
                Random.map2 (\cRing cIcon -> ( cBack, cRing, cIcon ))
                    (randomColor black (cBack :: readableColors r cBack colors))
                    (randomColor white (readableColors r cBack (white :: colors)))
            )


white =
    Color.rgb 255 255 255


black =
    Color.rgb 0 0 0
