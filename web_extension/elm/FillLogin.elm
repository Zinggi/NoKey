module FillLogin exposing (main)

import Html exposing (Html)
import Element exposing (..)
import Elements
import Loader
import Styles
import Background
import Model exposing (Model, Msg(..), ModelState(..))
import ExternalStateView
import Data.RequestGroupPassword exposing (Status(..))
import Data.Sync
import Data exposing (..)


view : ModelState -> Html Msg
view model =
    (case model of
        Loaded m ->
            case m.currentSite of
                Nothing ->
                    empty

                Just site ->
                    Data.Sync.mapAccountsForSite site viewStatus m.syncData
                        |> Elements.inputGroup "Choose login"

        _ ->
            empty
    )
        |> Element.layout (padding (Styles.paddingScale 3) :: Styles.background)


viewStatus : GroupId -> AccountId -> Status -> Element Msg
viewStatus groupId (( siteName, userName ) as accountId) status =
    case status of
        Done fill pw ->
            labled userName
                [ Elements.button (Just (FillForm accountId)) "Fill"
                ]

        Waiting n m ->
            labled userName
                [ row [ width fill ]
                    [ Element.html <| Loader.loaderWithOptions { loaderOptions | color = Styles.black }
                    , Elements.text <| toString n ++ "/" ++ toString m
                    ]
                , el [ alignRight ] (Elements.button (Just (RequestPasswordPressed [ groupId ] (Just accountId))) "Retry")
                ]

        Error error ->
            labled userName
                [ el [ width fill ] (Elements.text ("Error:\n" ++ error))
                , el [ alignRight ] (Elements.button (Just (RequestPasswordPressed [ groupId ] (Just accountId))) "retry")
                ]

        NotRequested ->
            -- We aren't waiting on any shares yet
            labled userName
                [ el [ alignRight ] (Elements.button (Just (RequestPasswordPressed [ groupId ] (Just accountId))) "Request")
                ]


loaderOptions =
    Loader.defaultOptions


labled l rest =
    row [] (el [ width fill ] (Elements.text l) :: rest)


viewError : String -> Html msg
viewError err =
    (case err of
        "not loaded yet" ->
            Element.empty

        other ->
            Elements.p other
    )
        |> Element.layout Styles.background


main =
    ExternalStateView.program
        { view = view
        , viewError = viewError
        , subs = Background.subs
        , decodeModel = Model.decode
        , encodeMsg = Model.encodeMsg
        }
