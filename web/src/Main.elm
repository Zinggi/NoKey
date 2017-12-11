module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onInput, onSubmit, onClick)
import Json.Encode as JE
import Json.Decode as JD
import Dict exposing (Dict)
import Set exposing (Set)
import Time


-- TODO: change random to a higher bit version
-- also adapt the UUID package
-- see:
--  https://github.com/danyx23/elm-uuid/issues/10
--  https://github.com/mgold/elm-random-pcg/issues/18

import Random.Pcg as Random exposing (Generator, Seed)
import Http
import Uuid
import RemoteData exposing (WebData)
import Debounce exposing (Debounce)


--

import PasswordGenerator exposing (PasswordRequirements)
import PasswordGenerator.View as PW
import Pairing
import Api
import Crdt.ORDict as ORDict exposing (ORDict)


type alias Model =
    { newSiteEntry : PasswordMetaData
    , expandSiteEntry : Bool
    , requirementsState : PW.State
    , pairingDialogue : Pairing.State
    , showPairingDialogue : Bool
    , seed : Random.Seed
    , onlineDevices : Devices
    , debounce : Debounce ()

    -- TODO: The ones below should be serialized:
    , uniqueIdentifyier : String

    -- TODO: these two should be in a state objects and inside a CRDT for synchronisation
    , syncData : Api.SyncData
    , sites : List PasswordPart
    }


type alias PasswordMetaData =
    { securityLevel : Int
    , length : Int
    , siteName : String
    , userName : String
    }


type alias Devices =
    Set String


devicesMap : (String -> String -> DeviceStatus -> b) -> ORDict String ( Int, String ) -> Devices -> List b
devicesMap f known_ids devs =
    Dict.foldl
        (\uuid ( _, name ) acc ->
            if Set.member uuid devs then
                f uuid name Online :: acc
            else
                f uuid name Offline :: acc
        )
        []
        (ORDict.get known_ids)


type DeviceStatus
    = Online
    | Offline


defaultMetaData : PasswordMetaData
defaultMetaData =
    { securityLevel = 2, length = 16, siteName = "", userName = "" }


resetMeta : PasswordMetaData -> PasswordMetaData
resetMeta meta =
    { meta | siteName = "" }


type alias PasswordPart =
    { pw : Random.Seed, meta : PasswordMetaData, requirements : PasswordRequirements }


splitPassword : PasswordMetaData -> PW.State -> Random.Seed -> PasswordPart
splitPassword meta req seed =
    -- TODO: the seed is the actual password!
    -- since the seed IS the password, it should have at least as many bytes of randomness as the desired password length!
    -- Use the seed that was used to generate the password and split it into parts.
    -- Use Shamir's secret sharing algorithm
    PasswordPart (seed) meta (PW.getRequirements req)


randomUUID : Generator String
randomUUID =
    Random.map Uuid.toString Uuid.uuidGenerator


initModel : Int -> Model
initModel randInt =
    let
        ( uuid, seed2 ) =
            Random.step randomUUID (Random.initialSeed randInt)

        ( indepSeed, seed3 ) =
            Random.step Random.independentSeed seed2
    in
        { sites = []
        , newSiteEntry = defaultMetaData
        , expandSiteEntry = False
        , requirementsState = PW.init
        , seed = seed3
        , debounce = Debounce.init
        , uniqueIdentifyier = uuid
        , syncData = Api.init indepSeed uuid
        , onlineDevices = Set.empty
        , pairingDialogue = Pairing.init
        , showPairingDialogue = True
        }


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.soon (1 * Time.second)
    , transform = SyncToOthers
    }


type alias Flags =
    { initialSeed : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel flags.initialSeed, Cmd.none )


type Msg
    = AddPassword
    | SiteNameChanged String
    | PasswordLengthChanged Int
    | SecurityLevelChanged Int
    | NewPasswordRequirements PW.State
    | GenerateNewPassword
    | UserNameChanged String
    | ReceiveMessage JE.Value
    | ReceiveToken (WebData String)
    | PairDeviceClicked
    | GetTokenClicked
    | UpdatePairing Pairing.State
    | TokenSubmitted
    | PairedWith (Result Http.Error ( String, Api.SyncData ))
    | RejoinChannel JE.Value
    | RemoveDevice String
    | SetDeviceName String
    | SyncToOthers Debounce.Msg
    | NoOp


noCmd : a -> ( a, Cmd msg )
noCmd a =
    ( a, Cmd.none )


withCmd : Cmd msg -> a -> ( a, Cmd msg )
withCmd cmd a =
    ( a, cmd )


addCmd : List (Cmd msg) -> ( a, Cmd msg ) -> ( a, Cmd msg )
addCmd cmds ( a, cmd ) =
    ( a, Cmd.batch (cmd :: cmds) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model |> noCmd

        AddPassword ->
            let
                pwPart =
                    splitPassword model.newSiteEntry model.requirementsState model.seed
            in
                { model | sites = pwPart :: model.sites, newSiteEntry = resetMeta model.newSiteEntry, expandSiteEntry = False }
                    |> updateSeed
                    |> noCmd

        SiteNameChanged s ->
            { model | newSiteEntry = (\e -> { e | siteName = s }) model.newSiteEntry, expandSiteEntry = not <| String.isEmpty s }
                |> noCmd

        SecurityLevelChanged n ->
            { model | newSiteEntry = (\e -> { e | securityLevel = n }) model.newSiteEntry }
                |> noCmd

        GenerateNewPassword ->
            updateSeed model
                |> noCmd

        NewPasswordRequirements state ->
            { model | requirementsState = state }
                |> updateSeed
                |> noCmd

        PasswordLengthChanged l ->
            { model | newSiteEntry = (\e -> { e | length = l }) model.newSiteEntry }
                |> updateSeed
                |> noCmd

        UserNameChanged n ->
            { model | newSiteEntry = (\e -> { e | userName = n }) model.newSiteEntry }
                |> noCmd

        ReceiveMessage msg ->
            model
                |> (\m ->
                        -- TODO: include senderID in message + reject messages from unknown sources
                        case Debug.log "received msg" <| JD.decodeValue Api.serverResponseDecoder msg of
                            Ok (Api.PairedWith id syncData) ->
                                pairedWith id syncData m

                            Ok (Api.SyncUpdate syncData) ->
                                syncUpdate syncData m

                            Ok Api.GotRemoved ->
                                { m | syncData = Api.gotRemoved model.syncData } |> noCmd

                            Err e ->
                                m |> noCmd
                   )

        ReceiveToken token ->
            ( { model | pairingDialogue = Pairing.receivedToken token model.pairingDialogue }, Cmd.none )

        PairDeviceClicked ->
            { model | showPairingDialogue = not model.showPairingDialogue }
                |> noCmd

        GetTokenClicked ->
            model
                |> (\m -> { m | pairingDialogue = Pairing.getTockenClicked model.pairingDialogue })
                |> withCmd (Api.initPairing ReceiveToken model.uniqueIdentifyier model.syncData)

        UpdatePairing s ->
            { model | pairingDialogue = s }
                |> noCmd

        TokenSubmitted ->
            model
                |> (\m -> { m | pairingDialogue = Pairing.tokenSubmitted m.pairingDialogue })
                |> withCmd (Api.pairWith PairedWith model.uniqueIdentifyier model.pairingDialogue.inputToken model.syncData)

        PairedWith res ->
            case res of
                Ok ( id, known_ids ) ->
                    model
                        |> (\m -> { m | pairingDialogue = Pairing.pairingCompleted (Ok id) m.pairingDialogue })
                        |> pairedWith id known_ids

                Err e ->
                    model
                        |> (\m -> { m | pairingDialogue = Pairing.pairingCompleted (Err e) m.pairingDialogue })
                        |> noCmd

        RejoinChannel v ->
            let
                _ =
                    Debug.log "RejoinChannel" ()
            in
                model
                    |> syncToOthers

        RemoveDevice uuid ->
            let
                ( sync, removeCmd ) =
                    Api.removeDevice NoOp uuid model.syncData
            in
                { model | syncData = sync }
                    |> syncToOthers
                    |> addCmd [ removeCmd ]

        SetDeviceName newName ->
            -- TODO: set page title to quickly see which tab is which
            let
                newSync =
                    -- do not sync immediately to reduce #of messages.
                    Api.renameDevice newName model.syncData
            in
                { model | syncData = newSync }
                    |> syncToOthers

        SyncToOthers msg ->
            -- delay the sync update to others, as we might get multiple updates in a short time, so wait until it settled.
            let
                ( newSync, cmdToDebounce ) =
                    Api.syncToOthers NoOp model.syncData

                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast (always cmdToDebounce))
                        msg
                        model.debounce
            in
                { model | debounce = debounce, syncData = newSync } ! [ cmd ]


pairedWith : String -> Api.SyncData -> Model -> ( Model, Cmd Msg )
pairedWith id syncData model =
    { model | onlineDevices = Set.insert id model.onlineDevices, syncData = Api.pairedWith id syncData model.syncData }
        |> syncUpdate syncData


syncToOthers : Model -> ( Model, Cmd Msg )
syncToOthers model =
    let
        ( debounce, cmd ) =
            Debounce.push debounceConfig () model.debounce
    in
        ( { model | debounce = debounce }, cmd )


syncUpdate : Api.SyncData -> Model -> ( Model, Cmd Msg )
syncUpdate syncData model =
    let
        newSync =
            Api.merge syncData model.syncData

        ( debounce, cmd ) =
            Debounce.push debounceConfig () model.debounce
    in
        ( { model | syncData = newSync, debounce = debounce }
        , cmd
        )


updateSeed : Model -> Model
updateSeed model =
    { model
        | seed =
            Tuple.second <| PW.getNextPassword model.requirementsState model.newSiteEntry.length model.seed
    }


view : Model -> Html Msg
view model =
    Html.div []
        [ viewDevices model.uniqueIdentifyier model.syncData.knownIds model.onlineDevices
        , Pairing.view (pairingConfig model.showPairingDialogue) model.pairingDialogue
        , Html.hr [] []
        , newSiteForm model.requirementsState model.expandSiteEntry model.newSiteEntry model.seed
        , Html.hr [] []
        , viewSavedSites model.sites

        -- , Html.div [] [ Html.button [ onClick PairDeviceClicked ] [ Html.text "Pair device..." ] ]
        ]


pairingConfig : Bool -> Pairing.Config Msg
pairingConfig doShow =
    { doShow = doShow, onSubmitToken = TokenSubmitted, onGetTokenClicked = GetTokenClicked, toMsg = UpdatePairing }


viewDevices : String -> ORDict String ( Int, String ) -> Devices -> Html Msg
viewDevices myId knownIds devs =
    Html.table []
        (Html.tr [] [ Html.th [] [ Html.text "name" ], Html.th [] [ Html.text "uuid" ] ]
            -- TODO. see if there is a reliable method for detecting online devices
            -- , Html.th [] [ Html.text "status" ] ]
            :: devicesMap (viewDeviceEntry myId) knownIds devs
        )


viewDeviceEntry : String -> String -> String -> DeviceStatus -> Html Msg
viewDeviceEntry myId uuid name status =
    Html.tr []
        ([ Html.td []
            [ if myId == uuid then
                Html.input [ Attr.value name, onInput SetDeviceName, Attr.placeholder "Name your device.." ] []
              else
                Html.text name
            ]
         , Html.td [] [ Html.text uuid ]

         -- , Html.td [] [ Html.text (toString status) ]
         ]
            ++ (if myId /= uuid then
                    [ Html.button [ onClick (RemoveDevice uuid) ] [ Html.text "Remove!" ] ]
                else
                    []
               )
        )


viewSavedSites : List PasswordPart -> Html Msg
viewSavedSites sites =
    Html.div []
        (List.map
            (\({ meta } as spw) ->
                Html.div [] [ Html.h3 [] [ Html.text meta.siteName ], Html.text (toString spw) ]
            )
            sites
        )


clampedNumberInput : (Int -> msg) -> ( Int, Int, Int ) -> Int -> Html msg
clampedNumberInput toMsg ( min, default, max ) n =
    let
        m =
            clamp min max n
    in
        Html.input
            [ Attr.type_ "number"
            , Attr.min (toString min)
            , Attr.max (toString max)
            , Attr.value (toString m)
            , onInput (\s -> String.toInt s |> Result.map (clamp min max) |> Result.withDefault default |> toMsg)
            ]
            []


newSiteForm : PW.State -> Bool -> PasswordMetaData -> Seed -> Html Msg
newSiteForm requirementsState expandSiteEntry entry seed =
    let
        pw =
            Tuple.first (PW.getNextPassword requirementsState entry.length seed)
    in
        Html.div []
            [ Html.form [ onSubmit GenerateNewPassword ]
                [ Html.text "New Site: "
                , Html.input [ Attr.placeholder "example.com", Attr.value entry.siteName, onInput SiteNameChanged ] []
                ]
            , (if not expandSiteEntry then
                Html.text ""
               else
                Html.div []
                    ([ Html.text "Login name: "
                     , Html.input [ Attr.value entry.userName, onInput UserNameChanged ] []
                     , Html.text "Security Level: "

                     -- TODO: limit max by number of available devices.
                     , clampedNumberInput SecurityLevelChanged ( 2, 2, 5 ) entry.securityLevel
                     , Html.text "Password length: "
                     , clampedNumberInput PasswordLengthChanged ( 4, 16, 512 ) entry.length
                     , PW.view NewPasswordRequirements requirementsState
                     ]
                        ++ case pw of
                            Ok thePw ->
                                [ Html.text "your new password: "
                                , Html.text thePw
                                , Html.div
                                    []
                                    [ Html.button [ onClick AddPassword ] [ Html.text "OK" ]
                                    , Html.button [ onClick GenerateNewPassword ] [ Html.text "Generate another one!" ]
                                    ]
                                ]

                            Err e ->
                                [ Html.text e ]
                    )
              )
            ]


subs : Model -> Sub Msg
subs model =
    Api.connectPrivateSocket ReceiveMessage RejoinChannel model.uniqueIdentifyier


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , subscriptions = subs
        , view = view
        , update = update
        }
