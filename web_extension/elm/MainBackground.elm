module MainBackground exposing (main)

import Background
import Model exposing (ModelState, Msg, Flags)


main : Program Flags ModelState Msg
main =
    Platform.programWithFlags
        { init = Background.init
        , subscriptions = Background.subs
        , update = Background.updateModelState
        }
