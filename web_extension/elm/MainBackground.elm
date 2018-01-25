module MainBackground exposing (main)

import Background exposing (Flags, Model, Msg)


main : Program Flags Model Msg
main =
    Platform.programWithFlags
        { init = Background.init
        , subscriptions = Background.subs
        , update = Background.update
        }
