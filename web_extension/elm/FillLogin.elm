module FillLogin exposing (main)

import Html exposing (Html)
import Dict
import Element exposing (..)
import Elements
import Styles
import Background exposing (Model, Msg(..))
import ExternalStateView
import Data.Sync
import Data.RequestPassword exposing (Status(..))
import Helper exposing (maybeToList)


view : Model -> Html Msg
view model =
    (case model.currentSite of
        Nothing ->
            empty

        Just site ->
            case Data.RequestPassword.getStatusForSite site model.sitesState |> Dict.toList of
                ( _, Done fill pw ) :: _ ->
                    -- empty
                    Elements.text "Done"

                ( _, Waiting n m ) :: _ ->
                    Elements.text <| "Received " ++ toString n ++ "/" ++ toString m ++ " shares"

                ( _, Error error ) :: _ ->
                    Elements.text ("Couldn't recover password, reason:\n" ++ error)

                _ :: _ ->
                    -- TODO: what does this mean???
                    Debug.crash
                        ("getSavedSite was Nothing, but we are waiting for shares "
                            ++ "(getSavedSite returned Just ..).\nThis doesn't make any sense, so we crash."
                        )

                [] ->
                    -- We aren't waiting on any shares yet
                    Elements.inputGroup "Choose login"
                        (chooseAccount site (Data.Sync.getAccountsForSite site model.syncData))
    )
        |> Element.layout Styles.background


chooseAccount : String -> List String -> List (Element Msg)
chooseAccount site logins =
    List.map (\login -> Elements.button (Just (RequestPasswordPressed ( site, login ) True)) login) logins


main =
    ExternalStateView.program
        { view = view
        , subs = Background.subs
        , decodeModel = Background.decodeModel
        , encodeMsg = Background.encodeMsg
        }
