module UTF8 exposing (foldl)

import Bitwise exposing (and, or, shiftLeftBy, shiftRightZfBy)
import Char


foldl : (Int -> a -> a) -> a -> String -> a
foldl op acc input =
    let
        helper char acc =
            accumulate op (Char.toCode char) acc
    in
        String.foldl helper ( acc, Nothing ) input
            |> Tuple.first


type alias Accumulator a =
    ( a, Maybe Int )


accumulate : (Int -> a -> a) -> Int -> Accumulator a -> Accumulator a
accumulate add char ( acc, combine ) =
    case combine of
        Nothing ->
            if char < 0x80 then
                ( acc |> add char
                , Nothing
                )
            else if char < 0x0800 then
                ( acc
                    |> add (or 0xC0 <| shiftRightZfBy 6 char)
                    |> add (or 0x80 <| and 0x3F char)
                , Nothing
                )
            else if char < 0xD800 || char >= 0xE000 then
                ( acc
                    |> add (or 0xE0 <| shiftRightZfBy 12 char)
                    |> add (or 0x80 <| and 0x3F <| shiftRightZfBy 6 char)
                    |> add (or 0x80 <| and 0x3F char)
                , Nothing
                )
            else
                ( acc, Just char )

        Just prev ->
            let
                combined : Int
                combined =
                    (prev
                        |> and 0x03FF
                        |> shiftLeftBy 10
                        |> or (and 0x03FF char)
                    )
                        + 0x00010000
            in
                ( acc
                    |> add (or 0xF0 <| shiftRightZfBy 18 combined)
                    |> add (or 0x80 <| and 0x3F <| shiftRightZfBy 12 combined)
                    |> add (or 0x80 <| and 0x3F <| shiftRightZfBy 6 combined)
                    |> add (or 0x80 <| and 0x3F combined)
                , Nothing
                )