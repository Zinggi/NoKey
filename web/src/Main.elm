module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onInput, onSubmit, onClick)
import Json.Encode as JE


-- TODO: change random to a higher bit version
-- also adapt the UUID package
-- see:
--  https://github.com/danyx23/elm-uuid/issues/10
--  https://github.com/mgold/elm-random-pcg/issues/18

import Random.Pcg as Random exposing (Generator, Seed)
import Http
import Uuid
import RemoteData exposing (WebData)


--

import PasswordGenerator exposing (PasswordRequirements)
import PasswordGenerator.View as PW
import Pairing
import Api


type alias Model =
    { newSiteEntry : PasswordMetaData
    , expandSiteEntry : Bool
    , requirementsState : PW.State
    , pairingDialogue : Pairing.State
    , showPairingDialogue : Bool
    , seed : Random.Seed
    , messages : List JE.Value

    -- TODO: The ones below should be serialized:
    , uniqueIdentifyier : String

    -- TODO: these two should be in a state objects and inside a CRDT for synchronisation
    , devices : List Device
    , sites : List PasswordPart
    }


type alias PasswordMetaData =
    { securityLevel : Int
    , length : Int
    , siteName : String
    , userName : String
    }


type alias Device =
    { name : String
    , uuid : String
    , status : DeviceStatus
    }


type DeviceStatus
    = Online
    | Offline
      -- local means the device that is actually running the code
    | Local


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
    in
        { sites = []
        , newSiteEntry = defaultMetaData
        , expandSiteEntry = False
        , requirementsState = PW.init
        , seed = seed2
        , uniqueIdentifyier = uuid
        , devices = [ { name = "Local PC", uuid = uuid, status = Local } ]
        , messages = []
        , pairingDialogue = Pairing.init
        , showPairingDialogue = False
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
    | PairedWith (Result Http.Error String)


noCmd : a -> ( a, Cmd msg )
noCmd a =
    ( a, Cmd.none )


withCmd : Cmd msg -> a -> ( a, Cmd msg )
withCmd cmd a =
    ( a, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            ( { model | messages = msg :: model.messages }
            , Cmd.none
            )

        ReceiveToken token ->
            ( { model | pairingDialogue = Pairing.receivedToken token model.pairingDialogue }, Cmd.none )

        PairDeviceClicked ->
            { model | showPairingDialogue = not model.showPairingDialogue }
                |> noCmd

        GetTokenClicked ->
            model
                |> withCmd (Api.initPairing ReceiveToken model.uniqueIdentifyier)

        UpdatePairing s ->
            { model | pairingDialogue = s }
                |> noCmd

        TokenSubmitted ->
            model
                |> withCmd (Api.pairWith PairedWith model.uniqueIdentifyier model.pairingDialogue.inputToken)

        PairedWith mayOtherId ->
            (case mayOtherId of
                Ok id ->
                    { model | devices = { name = "", uuid = id, status = Online } :: model.devices }

                Err e ->
                    Debug.crash ("TODO: deal with no internet, token expiry, etc...:\n" ++ toString e)
            )
                |> noCmd


updateSeed : Model -> Model
updateSeed model =
    { model
        | seed =
            Tuple.second <| PW.getNextPassword model.requirementsState model.newSiteEntry.length model.seed
    }


view : Model -> Html Msg
view model =
    Html.div []
        [ viewDevices model.devices
        , newSiteForm model.requirementsState model.expandSiteEntry model.newSiteEntry model.seed
        , viewSavedSites model.sites
        , Html.div [] [ Html.button [ onClick PairDeviceClicked ] [ Html.text "Pair device..." ] ]
        , Pairing.view (pairingConfig model.showPairingDialogue) model.pairingDialogue
        , Html.div [] [ Html.text (toString model.messages) ]
        ]


pairingConfig : Bool -> Pairing.Config Msg
pairingConfig doShow =
    { doShow = doShow, onSubmitToken = TokenSubmitted, onGetTokenClicked = GetTokenClicked, toMsg = UpdatePairing }


viewDevices : List Device -> Html Msg
viewDevices devs =
    Html.table []
        (Html.tr [] [ Html.th [] [ Html.text "name" ], Html.th [] [ Html.text "uuid" ], Html.th [] [ Html.text "status" ] ]
            :: List.map viewDeviceEntry devs
        )


viewDeviceEntry : Device -> Html Msg
viewDeviceEntry dev =
    Html.tr []
        [ Html.td [] [ Html.text dev.name ]
        , Html.td [] [ Html.text dev.uuid ]
        , Html.td [] [ Html.text (toString dev.status) ]
        ]


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
    Api.connectPrivateSocket ReceiveMessage model.uniqueIdentifyier


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , subscriptions = subs
        , view = view
        , update = update
        }
