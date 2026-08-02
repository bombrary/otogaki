module Codec.ProjectJson exposing (decoder, encode)

import Data.ChordTrack exposing (ChordTrack)
import Data.Note exposing (Note)
import Data.Project exposing (Project)
import Data.ReferenceAudio exposing (ReferenceAudio)
import Data.Scrap exposing (Scrap)
import Data.Section exposing (Section)
import Data.Track exposing (Track, TrackKind(..))
import Json.Decode as Decode
import Json.Encode as Encode


currentVersion : Int
currentVersion =
    1


encode : Project -> Encode.Value
encode project =
    Encode.object
        [ ( "version", Encode.int currentVersion )
        , ( "name", Encode.string project.name )
        , ( "bpm", Encode.int project.bpm )
        , ( "tracks", Encode.list encodeTrack project.tracks )
        , ( "chordTrack", encodeChordTrack project.chordTrack )
        , ( "sections", Encode.list encodeSection project.sections )
        , ( "scraps", Encode.list encodeScrap project.scraps )
        , ( "referenceAudio", encodeReferenceAudio project.referenceAudio )
        , ( "nextId", Encode.int project.nextId )
        ]


encodeReferenceAudio : ReferenceAudio -> Encode.Value
encodeReferenceAudio ra =
    Encode.object
        [ ( "fileName"
          , case ra.fileName of
                Just name ->
                    Encode.string name

                Nothing ->
                    Encode.null
          )
        , ( "offsetMs", Encode.int ra.offsetMs )
        , ( "volume", Encode.int ra.volume )
        , ( "muted", Encode.bool ra.muted )
        ]


encodeSection : Section -> Encode.Value
encodeSection section =
    Encode.object
        [ ( "id", Encode.int section.id )
        , ( "name", Encode.string section.name )
        , ( "lengthBars", Encode.int section.lengthBars )
        , ( "memo", Encode.string section.memo )
        ]


encodeScrap : Scrap -> Encode.Value
encodeScrap scrap =
    Encode.object
        [ ( "id", Encode.int scrap.id )
        , ( "name", Encode.string scrap.name )
        , ( "notes", Encode.list encodeNote scrap.notes )
        ]


encodeChordTrack : ChordTrack -> Encode.Value
encodeChordTrack ct =
    Encode.object
        [ ( "text", Encode.string ct.text )
        , ( "instrument", Encode.string (Data.Track.instrumentToString ct.instrument) )
        , ( "muted", Encode.bool ct.muted )
        , ( "volume", Encode.int ct.volume )
        ]


encodeTrack : Track -> Encode.Value
encodeTrack track =
    let
        ( kindType, notes ) =
            case track.kind of
                NoteTrack ns ->
                    ( "note", ns )

                DrumTrack ns ->
                    ( "drum", ns )
    in
    Encode.object
        [ ( "id", Encode.int track.id )
        , ( "name", Encode.string track.name )
        , ( "instrument", Encode.string (Data.Track.instrumentToString track.instrument) )
        , ( "muted", Encode.bool track.muted )
        , ( "volume", Encode.int track.volume )
        , ( "kindType", Encode.string kindType )
        , ( "notes", Encode.list encodeNote notes )
        ]


encodeNote : Note -> Encode.Value
encodeNote note =
    Encode.object
        [ ( "id", Encode.int note.id )
        , ( "pitch", Encode.int note.pitch )
        , ( "start", Encode.int note.start )
        , ( "duration", Encode.int note.duration )
        , ( "velocity", Encode.int note.velocity )
        ]


decoder : Decode.Decoder Project
decoder =
    Decode.field "version" Decode.int
        |> Decode.andThen
            (\version ->
                if version == currentVersion then
                    projectDecoder

                else
                    Decode.fail ("未対応のバージョン: " ++ String.fromInt version)
            )


projectDecoder : Decode.Decoder Project
projectDecoder =
    Decode.map8 Project
        (Decode.field "name" Decode.string)
        (Decode.field "bpm" Decode.int)
        (Decode.field "tracks" (Decode.list trackDecoder))
        (Decode.oneOf
            [ Decode.field "chordTrack" chordTrackDecoder
            , Decode.succeed Data.ChordTrack.empty
            ]
        )
        (Decode.oneOf
            [ Decode.field "sections" (Decode.list sectionDecoder)
            , Decode.succeed []
            ]
        )
        (Decode.oneOf
            [ Decode.field "scraps" (Decode.list scrapDecoder)
            , Decode.succeed []
            ]
        )
        (Decode.oneOf
            [ Decode.field "referenceAudio" referenceAudioDecoder
            , Decode.succeed Data.ReferenceAudio.empty
            ]
        )
        (Decode.field "nextId" Decode.int)


referenceAudioDecoder : Decode.Decoder ReferenceAudio
referenceAudioDecoder =
    Decode.map4 ReferenceAudio
        (Decode.field "fileName" (Decode.nullable Decode.string))
        (Decode.field "offsetMs" Decode.int)
        (Decode.field "volume" Decode.int)
        (Decode.field "muted" Decode.bool)


sectionDecoder : Decode.Decoder Section
sectionDecoder =
    Decode.map4 Section
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "lengthBars" Decode.int)
        (Decode.field "memo" Decode.string)


scrapDecoder : Decode.Decoder Scrap
scrapDecoder =
    Decode.map3 Scrap
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "notes" (Decode.list noteDecoder))


chordTrackDecoder : Decode.Decoder ChordTrack
chordTrackDecoder =
    Decode.map4 ChordTrack
        (Decode.field "text" Decode.string)
        (Decode.field "instrument" instrumentDecoder)
        (Decode.field "muted" Decode.bool)
        (Decode.oneOf
            [ Decode.field "volume" Decode.int
            , Decode.succeed 100
            ]
        )


trackDecoder : Decode.Decoder Track
trackDecoder =
    Decode.map6 Track
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "instrument" instrumentDecoder)
        (Decode.field "muted" Decode.bool)
        (Decode.oneOf
            [ Decode.field "volume" Decode.int
            , Decode.succeed 100
            ]
        )
        kindDecoder


instrumentDecoder : Decode.Decoder Data.Track.Instrument
instrumentDecoder =
    Decode.string
        |> Decode.andThen
            (\raw ->
                case Data.Track.instrumentFromString raw of
                    Just inst ->
                        Decode.succeed inst

                    Nothing ->
                        Decode.fail ("未知の楽器: " ++ raw)
            )


kindDecoder : Decode.Decoder TrackKind
kindDecoder =
    Decode.field "kindType" Decode.string
        |> Decode.andThen
            (\kindType ->
                case kindType of
                    "note" ->
                        Decode.map NoteTrack (Decode.field "notes" (Decode.list noteDecoder))

                    "drum" ->
                        Decode.map DrumTrack (Decode.field "notes" (Decode.list noteDecoder))

                    _ ->
                        Decode.fail ("未知のトラック種別: " ++ kindType)
            )


noteDecoder : Decode.Decoder Note
noteDecoder =
    Decode.map5 Note
        (Decode.field "id" Decode.int)
        (Decode.field "pitch" Decode.int)
        (Decode.field "start" Decode.int)
        (Decode.field "duration" Decode.int)
        (Decode.field "velocity" Decode.int)
