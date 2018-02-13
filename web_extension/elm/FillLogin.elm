module FillLogin exposing (main)

import Html exposing (Html)
import Dict
import Element exposing (..)
import Elements
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
            List.map (viewStatus site) (Data.RequestPassword.getStatusForSite site model.syncData model.sitesState |> Dict.toList)
                |> Elements.inputGroup "Choose login"
    )
        |> Element.layout Styles.background


viewStatus : String -> ( String, Status ) -> Element Msg
viewStatus site ( login, status ) =
    case status of
        Done fill pw ->
            Elements.button (Just (FillForm { login = login, site = site, password = pw })) ("Fill password for " ++ login)

        Waiting n m ->
            Elements.text <| "Received " ++ toString n ++ "/" ++ toString m ++ " shares of " ++ login

        Error error ->
            Elements.text ("Couldn't recover password for " ++ login ++ ", reason:\n" ++ error)

        NotRequested ->
            -- We aren't waiting on any shares yet
            Elements.button (Just (RequestPasswordPressed ( site, login ) True)) login


main =
    ExternalStateView.program
        { view = view
        , subs = Background.subs
        , decodeModel = Background.decodeModel
        , encodeMsg = Background.encodeMsg
        }
