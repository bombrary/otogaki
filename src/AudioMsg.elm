module AudioMsg exposing (AudioEvent(..), decode)

import Array exposing (Array)
import Json.Decode as Decode


type AudioEvent
    = Playhead Int
    | PlaybackStopped
    | AudioReady
    | InstrumentLoaded String
    | InstrumentLoadFailed String
    | RefAudioReady { name : String, peaks : Array Float, peakDt : Float, durationSecs : Float }
    | WavRenderStarted
    | WavRenderDone
    | WavRenderFailed String
    | Unknown


decode : Decode.Value -> AudioEvent
decode value =
    Decode.decodeValue decoder value
        |> Result.withDefault Unknown


decoder : Decode.Decoder AudioEvent
decoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "playhead" ->
                        Decode.map Playhead (Decode.at [ "payload", "ticks" ] Decode.int)

                    "stopped" ->
                        Decode.succeed PlaybackStopped

                    "audioReady" ->
                        Decode.succeed AudioReady

                    "instrumentLoaded" ->
                        Decode.map InstrumentLoaded (Decode.at [ "payload", "instrument" ] Decode.string)

                    "instrumentLoadFailed" ->
                        Decode.map InstrumentLoadFailed (Decode.at [ "payload", "instrument" ] Decode.string)

                    "refAudioReady" ->
                        Decode.map4
                            (\name peaks peakDt durationSecs -> RefAudioReady { name = name, peaks = peaks, peakDt = peakDt, durationSecs = durationSecs })
                            (Decode.at [ "payload", "name" ] Decode.string)
                            (Decode.at [ "payload", "peaks" ] (Decode.array Decode.float))
                            (Decode.at [ "payload", "peakDt" ] Decode.float)
                            (Decode.at [ "payload", "duration" ] Decode.float)

                    "wavRenderStarted" ->
                        Decode.succeed WavRenderStarted

                    "wavRenderDone" ->
                        Decode.succeed WavRenderDone

                    "wavRenderFailed" ->
                        Decode.map WavRenderFailed (Decode.at [ "payload", "message" ] Decode.string)

                    _ ->
                        Decode.succeed Unknown
            )
