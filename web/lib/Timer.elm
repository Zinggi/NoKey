module Timer exposing (Timer)

import Time exposing (Time)
import Dict exposing (Dict)
import Task
import Process


-- TODO: test, document and WAS IT WORTH IT?


type Timer
    = Timer
        { timers : Dict String ( Maybe Time, Time )
        , isActive : Bool
        }


type Msg
    = Tick Time


type alias Config msg =
    { onInterval : Time -> String -> msg
    , onFinish : Time -> String -> msg
    , toMsg : Msg -> msg
    , frequency : Time
    }


init : Timer
init =
    Timer { timers = Dict.empty, isActive = False }


startTimer : Config msg -> String -> Time -> Timer -> ( Timer, Cmd msg )
startTimer config id totalTime (Timer timer) =
    ( Timer { timers = Dict.insert id ( Nothing, totalTime ) timer.timers, isActive = True }
    , if timer.isActive then
        Cmd.none
      else
        Task.perform (config.toMsg << Tick) Time.now
    )


update : Config msg -> Msg -> Timer -> ( Timer, Cmd msg )
update config (Tick currentTime) (Timer timer) =
    let
        ( msgs, newTimers ) =
            Dict.foldl
                (\id ( mayStart, total ) ( msgs, t ) ->
                    case mayStart of
                        Just start ->
                            if currentTime - start < total then
                                ( config.onInterval currentTime id :: msgs, t )
                            else
                                ( config.onFinish currentTime id :: msgs, Dict.remove id t )

                        Nothing ->
                            ( msgs, Dict.insert id ( Just currentTime, total ) t )
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
            ( Timer { timer | timers = newTimers }, Cmd.batch cmds )
        else
            ( Timer { timer | timers = newTimers, isActive = False }, Cmd.batch (nextTick :: cmds) )
