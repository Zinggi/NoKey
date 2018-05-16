module Views.ReleaseLog exposing (view)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Font as Font
import Elements
import Styles
import Route exposing (Page(..))
import Model exposing (Msg(..))


log : Dict String (Element Msg)
log =
    Dict.fromList
        [ ( "0.0.0", text "lol 1" )
        , ( "0.0.1", text "lol 2" )
        ]


sortedLog : List String
sortedLog =
    Dict.keys log
        |> List.sort


view : String -> Element Msg
view s =
    case s of
        "" ->
            overview

        other ->
            Dict.get other log
                |> Maybe.withDefault (Elements.text ("No log for version: " ++ other))


overview : Element Msg
overview =
    List.map
        (\v ->
            Elements.linkButton [ Font.bold ] (NavigateTo (ReleaseLog v)) v
        )
        sortedLog
        |> column [ spacing (Styles.paddingScale 3), padding (Styles.paddingScale 4) ]
