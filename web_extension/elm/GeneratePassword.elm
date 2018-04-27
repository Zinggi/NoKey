port module GeneratePassword exposing (main)

import Html
import Views.PasswordGenerator as PW
import Element exposing (padding)
import Random.Pcg.Extended as RandomE
import Styles


type Msg
    = OnAcceptPw String
    | Pw PW.State


init : { initialSeed : ( Int, List Int ) } -> ( PW.State, Cmd Msg )
init { initialSeed } =
    let
        ( base, ext ) =
            initialSeed
    in
        ( PW.init (RandomE.initialSeed base ext), Cmd.none )


port onAcceptPw : String -> Cmd msg


update msg model =
    case msg of
        Pw s ->
            ( s, Cmd.none )

        OnAcceptPw pw ->
            ( PW.nextPassword model, onAcceptPw pw )


view model =
    PW.view OnAcceptPw True Pw model
        |> Element.layout (padding (Styles.paddingScale 3) :: Styles.background)


main =
    Html.programWithFlags
        { view = view
        , update = update
        , subscriptions = \model -> Sub.none
        , init = init
        }
