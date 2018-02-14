module FillLogin exposing (main)

import Html exposing (Html)
import Dict
import Element exposing (..)
import Elements
import Loader
import Styles
import Background exposing (Model, Msg(..))
import ExternalStateView
import Data.RequestPassword exposing (Status(..))


view : Model -> Html Msg
view model =
    (case model.currentSite of
        Nothing ->
            empty

        Just site ->
            List.map (viewStatus site) (sortedLogins site model.syncData model.sitesState)
                |> Elements.inputGroup "Choose login"
    )
        |> Element.layout Styles.background


sortedLogins site sync sitesState =
    -- Well this isn't sorted, but sorting might be confusing...
    Data.RequestPassword.getStatusForSite site sync sitesState
        |> Dict.toList


viewStatus : String -> ( String, Status ) -> Element Msg
viewStatus site ( login, status ) =
    case status of
        Done fill pw ->
            labled login
                [ Elements.button (Just (FillForm { login = login, site = site, password = pw })) "Fill"
                ]

        Waiting n m ->
            labled login
                [ row [ width fill ]
                    [ Element.html <| Loader.loaderWithOptions { loaderOptions | color = Styles.black }
                    , Elements.text <| toString n ++ "/" ++ toString m
                    ]
                , el [ alignRight ] (Elements.button (Just (RequestPasswordPressed ( site, login ) True)) "Retry")
                ]

        Error error ->
            labled login
                [ el [ width fill ] (Elements.text ("Error:\n" ++ error))
                , el [ alignRight ] (Elements.button (Just (RequestPasswordPressed ( site, login ) True)) "retry")
                ]

        NotRequested ->
            -- We aren't waiting on any shares yet
            labled login
                [ el [ alignRight ] (Elements.button (Just (RequestPasswordPressed ( site, login ) True)) "Request")
                ]


loaderOptions =
    Loader.defaultOptions


labled l rest =
    row [] (el [ width fill ] (Elements.text l) :: rest)


main =
    ExternalStateView.program
        { view = view
        , subs = Background.subs
        , decodeModel = Background.decodeModel
        , encodeMsg = Background.encodeMsg
        }
