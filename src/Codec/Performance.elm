module Codec.Performance exposing
    ( Event
    , Loop
    , encodeLoadInstruments
    , encodePlay
    , encodePreviewNote
    , encodeRenderWav
    , encodeSeek
    , encodeSetBpm
    , encodeSetMute
    , encodeSetVolume
    , encodeStop
    , encodeUpdateEvents
    , metronomeEvents
    , metronomeTrackId
    , toEvents
    , usedInstrumentNames
    )

import Array
import Data.ChordTrack exposing (ChordTrack)
import Data.Meter
import Data.Project exposing (Project)
import Data.ReferenceAudio exposing (ReferenceAudio)
import Data.StrumExpand
import Data.StrumPattern
import Data.Time
import Data.Timeline
import Data.Track exposing (Track, TrackKind(..))
import Data.Voicing
import Json.Encode as Encode


type alias Loop =
    { startTicks : Int
    , endTicks : Int
    }


encodePlay : { loop : Maybe Loop, startTicks : Int } -> Project -> Encode.Value
encodePlay opts project =
    command "play"
        (Encode.object
            [ ( "bpm", Encode.float project.bpm )
            , ( "ppq", Encode.int Data.Time.ppq )
            , ( "events", Encode.list encodeEvent (toEvents project) )
            , ( "tracks", trackMetas project )
            , ( "loop", encodeLoop opts.loop )
            , ( "startTicks", Encode.int opts.startTicks )
            , ( "refAudio", encodeRefAudio project.referenceAudio )
            ]
        )


encodeLoop : Maybe Loop -> Encode.Value
encodeLoop maybeLoop =
    case maybeLoop of
        Just l ->
            Encode.object
                [ ( "startTicks", Encode.int l.startTicks )
                , ( "endTicks", Encode.int l.endTicks )
                ]

        Nothing ->
            Encode.null


{-| プロジェクト全体 or ループ範囲を WAV に書き出す。refAudio はミックス対象外。
-}
encodeRenderWav : { loop : Maybe Loop, fileName : String } -> Project -> Encode.Value
encodeRenderWav opts project =
    command "renderWav"
        (Encode.object
            [ ( "bpm", Encode.float project.bpm )
            , ( "ppq", Encode.int Data.Time.ppq )
            , ( "events", Encode.list encodeEvent (toEvents project) )
            , ( "tracks", trackMetas project )
            , ( "loop", encodeLoop opts.loop )
            , ( "fileName", Encode.string opts.fileName )
            ]
        )


{-| 再生中にイベント列だけ差し替える（トランスポートは止めない）。
-}
encodeUpdateEvents : Project -> Encode.Value
encodeUpdateEvents project =
    command "updateEvents"
        (Encode.object
            [ ( "ppq", Encode.int Data.Time.ppq )
            , ( "events", Encode.list encodeEvent (toEvents project) )
            , ( "tracks", trackMetas project )
            , ( "refAudio", encodeRefAudio project.referenceAudio )
            ]
        )


encodeRefAudio : ReferenceAudio -> Encode.Value
encodeRefAudio ra =
    Encode.object
        [ ( "offsetMs", Encode.int ra.offsetMs )
        , ( "volume", Encode.int ra.volume )
        , ( "muted", Encode.bool ra.muted )
        ]


trackMetas : Project -> Encode.Value
trackMetas project =
    Encode.list identity (List.map encodeTrackMeta project.tracks ++ [ chordTrackMeta project.chordTrack ])


encodeStop : Encode.Value
encodeStop =
    command "stop" (Encode.object [])


encodeSetBpm : Float -> Encode.Value
encodeSetBpm bpm =
    command "setBpm" (Encode.object [ ( "bpm", Encode.float bpm ) ])


encodeSeek : Int -> Encode.Value
encodeSeek ticks =
    command "seek" (Encode.object [ ( "ticks", Encode.int ticks ) ])


encodeSetMute : Int -> Bool -> Encode.Value
encodeSetMute trackId muted =
    command "setMute"
        (Encode.object
            [ ( "trackId", Encode.int trackId )
            , ( "muted", Encode.bool muted )
            ]
        )


encodeSetVolume : Int -> Int -> Encode.Value
encodeSetVolume trackId volume =
    command "setVolume"
        (Encode.object
            [ ( "trackId", Encode.int trackId )
            , ( "volume", Encode.int volume )
            ]
        )


encodeLoadInstruments : List String -> Encode.Value
encodeLoadInstruments names =
    command "loadInstruments"
        (Encode.object [ ( "instruments", Encode.list Encode.string names ) ])


encodePreviewNote : String -> Int -> Encode.Value
encodePreviewNote instrument pitch =
    command "previewNote"
        (Encode.object
            [ ( "instrument", Encode.string instrument )
            , ( "pitch", Encode.int pitch )
            ]
        )


usedInstrumentNames : Project -> List String
usedInstrumentNames project =
    (project.tracks |> List.map (\t -> Data.Track.instrumentToString t.instrument))
        ++ [ Data.Track.instrumentToString project.chordTrack.instrument ]
        |> List.foldl
            (\name acc ->
                if List.member name acc then
                    acc

                else
                    acc ++ [ name ]
            )
            []


command : String -> Encode.Value -> Encode.Value
command tag payload =
    Encode.object [ ( "tag", Encode.string tag ), ( "payload", payload ) ]


type alias Event =
    { ticks : Int
    , durationTicks : Int
    , pitch : Int
    , velocity : Int
    , instrument : String
    , trackId : Int
    }


toEvents : Project -> List Event
toEvents project =
    let
        voicings =
            if project.voicingEnabled then
                project.voicings

            else
                []
    in
    List.concatMap trackEvents project.tracks ++ chordEvents project.guitarFormEnabled voicings (Data.Project.timeline project) project.chordTrack


{-| メトロノームのクリックイベントに予約している trackId。コードトラックの -1 の隣。
`js/audio.js` の `METRONOME_TRACK_ID` と値を一致させること（自然停止計算から除外するため）。
-}
metronomeTrackId : Int
metronomeTrackId =
    -2


{-| timeline 全体（`Data.Timeline.totalTicks` まで）にわたって小節頭・拍のクリックイベントを生成する。
拍子は小節ごとに異なってよい。小節頭（拍1）は cowbell、その他の拍は rimshot。
`toEvents` には混ざず、`encodePlay` / `encodeUpdateEvents` の呼び出し側でオプトインで追加する（WAV/MIDI 書き出しには混入しない）。
-}
metronomeEvents : Data.Timeline.Timeline -> List Event
metronomeEvents timeline =
    Array.toList timeline.bars
        |> List.concatMap barClicks


barClicks : Data.Timeline.Bar -> List Event
barClicks bar =
    let
        beatTicks =
            Data.Meter.ticksPerBeat bar.meter

        beatCount =
            Basics.max 1 (bar.lengthTicks // beatTicks)

        clickDuration =
            Basics.max 1 (Data.Time.ppq // 4)

        click i =
            { ticks = bar.startTicks + i * beatTicks
            , durationTicks = clickDuration
            , pitch =
                if i == 0 then
                    56

                else
                    37
            , velocity =
                if i == 0 then
                    115

                else
                    95
            , instrument = "drumKit"
            , trackId = metronomeTrackId
            }
    in
    List.range 0 (beatCount - 1) |> List.map click


chordEvents : Bool -> List Data.Voicing.Voicing -> Data.Timeline.Timeline -> ChordTrack -> List Event
chordEvents guitarFormEnabled voicings timeline chordTrack =
    let
        instrument =
            Data.Track.instrumentToString chordTrack.instrument

        resolvedChords =
            Data.ChordTrack.resolved timeline chordTrack
    in
    case chordTrack.rhythm |> Maybe.andThen Data.StrumPattern.byName of
        Just pattern ->
            case ( List.map .startTicks resolvedChords |> List.minimum, resolvedChords |> List.map (\rc -> rc.startTicks + rc.durationTicks) |> List.maximum ) of
                ( Just startTicks, Just endTicks ) ->
                    Data.StrumExpand.expand { startTicks = startTicks, endTicks = endTicks } guitarFormEnabled voicings pattern resolvedChords
                        |> List.map
                            (\n ->
                                { ticks = n.start
                                , durationTicks = n.duration
                                , pitch = n.pitch
                                , velocity = n.velocity
                                , instrument = instrument
                                , trackId = -1
                                }
                            )

                _ ->
                    []

        Nothing ->
            resolvedChords
                |> List.concatMap
                    (\ev ->
                        Data.StrumExpand.soundingPitches guitarFormEnabled voicings ev.chord
                            |> List.map
                                (\pitch ->
                                    { ticks = ev.startTicks
                                    , durationTicks = ev.durationTicks
                                    , pitch = pitch
                                    , velocity = 90
                                    , instrument = instrument
                                    , trackId = -1
                                    }
                                )
                    )


trackEvents : Track -> List Event
trackEvents track =
    let
        instrument =
            Data.Track.instrumentToString track.instrument

        fromNote note =
            { ticks = note.start
            , durationTicks = note.duration
            , pitch = note.pitch
            , velocity = note.velocity
            , instrument = instrument
            , trackId = track.id
            }
    in
    case track.kind of
        NoteTrack notes ->
            List.map fromNote notes

        DrumTrack notes ->
            List.map fromNote notes


encodeEvent : Event -> Encode.Value
encodeEvent e =
    Encode.object
        [ ( "ticks", Encode.int e.ticks )
        , ( "durationTicks", Encode.int e.durationTicks )
        , ( "pitch", Encode.int e.pitch )
        , ( "velocity", Encode.int e.velocity )
        , ( "instrument", Encode.string e.instrument )
        , ( "trackId", Encode.int e.trackId )
        ]


chordTrackMeta : ChordTrack -> Encode.Value
chordTrackMeta ct =
    Encode.object
        [ ( "id", Encode.int -1 )
        , ( "instrument", Encode.string (Data.Track.instrumentToString ct.instrument) )
        , ( "muted", Encode.bool ct.muted )
        , ( "volume", Encode.int ct.volume )
        ]


encodeTrackMeta : Track -> Encode.Value
encodeTrackMeta t =
    Encode.object
        [ ( "id", Encode.int t.id )
        , ( "instrument", Encode.string (Data.Track.instrumentToString t.instrument) )
        , ( "muted", Encode.bool t.muted )
        , ( "volume", Encode.int t.volume )
        ]
