module Views.Devices exposing (view)

import Dict exposing (Dict)
import Element exposing (..)
import Elements
import Model exposing (Msg(..))
import Route exposing (Page(..))


view : String -> Dict String ( String, String ) -> Element Msg
view myId knownIds =
    Elements.miniPage
        (Elements.myAvatar SetDeviceName myId (Dict.get myId knownIds |> Maybe.withDefault ( "", "" )) []
            :: devicesMap (viewDeviceEntry myId) knownIds
            -- TODO: add pairing link here, e.g. via floating add button
            ++ [ Elements.button (Just (NavigateTo Pairing)) "Pair new device" ]
        )


{-| TODO: fix input lag on input fields. Workaround:

    https://github.com/elm-lang/html/issues/105#issuecomment-309524197
    https://ellie-app.com/3fPSxX6VHK7a1/0

-}
viewDeviceEntry : String -> String -> ( String, String ) -> Element Msg
viewDeviceEntry myId uuid ( name, idPart ) =
    if myId == uuid then
        empty
    else
        row []
            [ Elements.avatar [ width fill ] { id = uuid, name = name, postFix = idPart }
            , Elements.delete (RemoveDevice uuid)
            ]


devicesMap : (String -> ( String, String ) -> b) -> Dict String ( String, String ) -> List b
devicesMap f known_ids =
    Dict.foldl
        (\uuid name acc ->
            f uuid name :: acc
        )
        []
        known_ids
