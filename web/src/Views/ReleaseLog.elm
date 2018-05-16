module Views.ReleaseLog exposing (view, allNews, summery)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Font as Font
import Elements
import Styles
import Route exposing (Page(..))
import Model exposing (Msg(..))


allNews : Dict String ( Element Msg, Element Msg )
allNews =
    -- TODO!: on new big release, write log
    Dict.fromList
        [ ( "0.3", ( s0_3, n0_3 ) )
        ]


s0_3 =
    Elements.p "This update requires some work on your part, please click below for more."


n0_3 =
    column [ spacing (Styles.paddingScale 3), height shrink ]
        [ Elements.h3 "Note"
        , Elements.paragraph []
            [ Elements.text "Due to some bugs in previous versions, it is recommended that you "
            , Elements.b "start from scratch"
            , Element.text "."
            , Elements.br
            , Elements.text "To do that and keep all your saved passwords, follow the instructions below:"
            ]
        , Elements.list
            [ Elements.text "Export your passwords"
            , Elements.text "Reset all your devices"
            , Elements.text "Pair your devices again"
            , Elements.text "Import passwords"
            ]
        , Elements.p "You'll find those features in the settings"
        , Elements.button (Just <| NavigateTo Options) "Go to Settings"
        , el [ paddingXY 0 (Styles.paddingScale 3) ] (Elements.h3 "New Features")
        , Elements.list
            [ Elements.text "Export passwords"
            , Elements.text "Import passwords"
            ]
        ]


sortedLog : List String
sortedLog =
    Dict.keys allNews
        |> List.sort


summery : List String -> Element Msg
summery versions =
    List.foldl
        (\v acc ->
            case Dict.get v allNews of
                Just ( s, _ ) ->
                    column [ spacing (Styles.paddingScale 1) ]
                        [ Elements.h4 v
                        , s
                        , Elements.button (Just (NavigateTo (ReleaseLog v))) "Read more"
                        ]
                        :: acc

                Nothing ->
                    acc
        )
        []
        versions
        |> column [ spacing (Styles.paddingScale 5), height shrink ]


view : String -> Element Msg
view s =
    case s of
        "" ->
            overview

        other ->
            Dict.get other allNews
                |> Maybe.map Tuple.second
                |> Maybe.withDefault (Elements.text ("No log for version: " ++ other))


overview : Element Msg
overview =
    summery sortedLog
