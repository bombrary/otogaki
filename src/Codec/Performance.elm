module Codec.Performance exposing
    ( Event
    , Loop
    , encodeLoadInstruments
    , encodePlay
    , encodePreviewNote
    , encodeSeek
    , encodeSetBpm
    , encodeSetMute
    , encodeSetVolume
    , encodeStop
    , encodeUpdateEvents
    , toEvents
    , usedInstrumentNames
    )

import Data.Chord
import Data.ChordTrack exposing (ChordTrack)
import Data.Project exposing (Project)
import Data.ReferenceAudio exposing (ReferenceAudio)
import Data.Time
import Data.Timeline
import Data.Track exposing (Track, TrackKind(..))
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
            , ( "loop"
              , case opts.loop of
                    Just l ->
                        Encode.object
                            [ ( "startTicks", Encode.int l.startTicks )
                            , ( "endTicks", Encode.int l.endTicks )
                            ]

                    Nothing ->
                        Encode.null
              )
            , ( "startTicks", Encode.int opts.startTicks )
            , ( "refAudio", encodeRefAudio project.referenceAudio )
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
    project.tracks
        |> List.map (\t -> Data.Track.instrumentToString t.instrument)
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
    List.concatMap trackEvents project.tracks ++ chordEvents (Data.Project.timeline project) project.chordTrack


chordEvents : Data.Timeline.Timeline -> ChordTrack -> List Event
chordEvents timeline chordTrack =
    let
        instrument =
            Data.Track.instrumentToString chordTrack.instrument
    in
    Data.ChordTrack.resolved timeline chordTrack
        |> List.concatMap
            (\ev ->
                Data.Chord.toPitches ev.chord
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
