module Views.Devices exposing (view)

import Dict exposing (Dict)
import Element exposing (..)
import Elements
import Styles
import Model exposing (Msg(..))


view : String -> Dict String String -> Element Msg
view myId knownIds =
    Elements.miniPage "Devices"
        (Elements.textInput (Just SetDeviceName) ("Name your device.. " ++ "(" ++ String.left 4 myId ++ ")") (Dict.get myId knownIds |> Maybe.withDefault "")
            :: devicesMap (viewDeviceEntry myId) knownIds
        )



-- Elements.table [ ( "name", .name ), ( "uuid", .uuid ) ]
--     (devicesMap (viewDeviceEntry myId) knownIds)


{-| TODO: fix input lag on input fields. Workaround:

    https://github.com/elm-lang/html/issues/105#issuecomment-309524197
    https://ellie-app.com/3fPSxX6VHK7a1/0

-}
viewDeviceEntry : String -> String -> String -> Element Msg
viewDeviceEntry myId uuid name =
    if myId == uuid then
        empty
    else
        row []
            [ el [ width (fillPortion 5) ]
                (paragraph [ alignLeft, spacing (Styles.scaled 1) ]
                    (if name == "" then
                        [ Elements.italicText "Unnamed ", Elements.text ("(" ++ String.left 4 uuid ++ ")") ]
                     else
                        [ Elements.text name ]
                    )
                )
            , el [ width fill ]
                -- TODO: add nice icons, e.g. trash-2 from:
                -- http://package.elm-lang.org/packages/1602/elm-feather/2.2.0
                -- https://feathericons.com/
                (Elements.button (Just (RemoveDevice uuid)) "Remove!")
            ]


devicesMap : (String -> String -> b) -> Dict String String -> List b
devicesMap f known_ids =
    Dict.foldl
        (\uuid name acc ->
            f uuid name :: acc
        )
        []
        known_ids
