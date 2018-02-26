module Timer exposing (Timer, init, Msg, Config, startTimer, update)

import Time exposing (Time)
import Dict exposing (Dict)
import Task
import Process


-- TODO: document


type Timer id
    = Timer
        { timers : Dict String ( id, Maybe Time, Time )
        , isActive : Bool
        }


type Msg
    = Tick Time


type alias Config id msg =
    { onInterval : id -> Time -> msg
    , onFinish : id -> Time -> msg
    , toMsg : Msg -> msg
    , frequency : Time
    }


init : Timer id
init =
    Timer { timers = Dict.empty, isActive = False }


startTimer : Config id msg -> id -> Time -> Timer id -> ( Timer id, Cmd msg )
startTimer config id totalTime (Timer timer) =
    ( Timer { timers = Dict.insert (toString id) ( id, Nothing, totalTime ) timer.timers, isActive = True }
    , if timer.isActive then
        Cmd.none
      else
        Task.perform (config.toMsg << Tick) Time.now
    )


update : Config id msg -> Msg -> Timer id -> ( Timer id, Cmd msg )
update config (Tick currentTime) (Timer timer) =
    let
        ( msgs, newTimers ) =
            Dict.foldl
                (\strId ( id, mayStart, total ) ( msgs, t ) ->
                    case mayStart of
                        Just start ->
                            if currentTime - start < total then
                                ( config.onInterval id currentTime :: msgs, t )
                            else
                                ( config.onFinish id currentTime :: msgs, Dict.remove strId t )

                        Nothing ->
                            ( msgs, Dict.insert strId ( id, Just currentTime, total ) t )
                )
                ( [], timer.timers )
                timer.timers

        needsUpdate =
            not (Dict.isEmpty newTimers)

        cmds =
            List.map (\msg -> Task.perform identity (Task.succeed msg)) msgs

        nextTick =
            Process.sleep config.frequency
                |> Task.andThen
                    (\_ ->
                        Time.now
                    )
                |> Task.perform (config.toMsg << Tick)
    in
        if needsUpdate then
            ( Timer { timer | timers = newTimers }, Cmd.batch (nextTick :: cmds) )
        else
            ( Timer { timer | timers = newTimers, isActive = False }, Cmd.batch cmds )
