module Codec.ProjectJson exposing (decoder, encode)

import Data.ChordTrack exposing (ChordTrack)
import Data.Key exposing (Key)
import Data.Meter exposing (Meter)
import Data.Note exposing (Note)
import Data.Project exposing (Project)
import Data.ReferenceAudio exposing (ReferenceAudio)
import Data.Scrap exposing (Scrap)
import Data.Section exposing (Section)
import Data.Track exposing (Track, TrackKind(..))
import Data.Voicing exposing (Voicing)
import Json.Decode as Decode
import Json.Encode as Encode
import Set


currentVersion : Int
currentVersion =
    1


encode : Project -> Encode.Value
encode project =
    Encode.object
        [ ( "version", Encode.int currentVersion )
        , ( "name", Encode.string project.name )
        , ( "bpm", Encode.float project.bpm )
        , ( "tracks", Encode.list encodeTrack project.tracks )
        , ( "chordTrack", encodeChordTrack project.chordTrack )
        , ( "sections", Encode.list encodeSection project.sections )
        , ( "scraps", Encode.list encodeScrap project.scraps )
        , ( "referenceAudio", encodeReferenceAudio project.referenceAudio )
        , ( "nextId", Encode.int project.nextId )
        , ( "memo", Encode.string project.memo )
        , ( "voicings", Encode.list encodeVoicing project.voicings )
        , ( "voicingEnabled", Encode.bool project.voicingEnabled )
        , ( "guitarFormEnabled", Encode.bool project.guitarFormEnabled )
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
        , ( "durationMs"
          , case ra.durationMs of
                Just ms ->
                    Encode.int ms

                Nothing ->
                    Encode.null
          )
        ]


encodeKey : Key -> Encode.Value
encodeKey key =
    Encode.object
        [ ( "tonic", Encode.int key.tonic )
        , ( "mode", Encode.string (Data.Key.modeToString key.mode) )
        ]


encodeMeter : Meter -> Encode.Value
encodeMeter meter =
    Encode.object
        [ ( "numerator", Encode.int meter.numerator )
        , ( "denominator", Encode.int meter.denominator )
        ]


encodeSection : Section -> Encode.Value
encodeSection section =
    Encode.object
        [ ( "id", Encode.int section.id )
        , ( "name", Encode.string section.name )
        , ( "lengthBars", Encode.int section.lengthBars )
        , ( "memo", Encode.string section.memo )
        , ( "key", encodeKey section.key )
        , ( "meter", encodeMeter section.meter )
        ]


encodeScrap : Scrap -> Encode.Value
encodeScrap scrap =
    Encode.object
        [ ( "id", Encode.int scrap.id )
        , ( "name", Encode.string scrap.name )
        , ( "notes", Encode.list encodeNote scrap.notes )
        ]


encodeVoicing : Voicing -> Encode.Value
encodeVoicing v =
    Encode.object
        [ ( "name", Encode.string v.name )
        , ( "offsets", Encode.list Encode.int v.offsets )
        , ( "stringPicks", Encode.list encodePick (Set.toList v.stringPicks) )
        ]


encodePick : ( Int, Int ) -> Encode.Value
encodePick ( offset, stringIndex ) =
    Encode.list Encode.int [ offset, stringIndex ]


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


{-| `Project` は9フィールドあり Json.Decode の map 関数は map8 が上限なので、2つに分けて合成する。
今後さらにフィールドを足す場合は PartB 側に足せばよい。
-}
type alias PartA =
    { name : String
    , bpm : Float
    , tracks : List Track
    , chordTrack : ChordTrack
    , sections : List Section
    }


type alias PartB =
    { scraps : List Scrap
    , referenceAudio : ReferenceAudio
    , nextId : Int
    , memo : String
    , voicings : List Voicing
    , voicingEnabled : Bool
    , guitarFormEnabled : Bool
    }


projectDecoder : Decode.Decoder Project
projectDecoder =
    Decode.map2 buildProject partADecoder partBDecoder


buildProject : PartA -> PartB -> Project
buildProject a b =
    { name = a.name
    , bpm = a.bpm
    , tracks = a.tracks
    , chordTrack = a.chordTrack
    , sections = a.sections
    , scraps = b.scraps
    , referenceAudio = b.referenceAudio
    , nextId = b.nextId
    , memo = b.memo
    , voicings = b.voicings
    , voicingEnabled = b.voicingEnabled
    , guitarFormEnabled = b.guitarFormEnabled
    }


partADecoder : Decode.Decoder PartA
partADecoder =
    Decode.map5 PartA
        (Decode.field "name" Decode.string)
        (Decode.oneOf
            [ Decode.field "bpm" Decode.float
            , Decode.field "bpm" Decode.int |> Decode.map toFloat
            ]
        )
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


partBDecoder : Decode.Decoder PartB
partBDecoder =
    Decode.map7 PartB
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
        (Decode.oneOf
            [ Decode.field "memo" Decode.string
            , Decode.succeed ""
            ]
        )
        (Decode.oneOf
            [ Decode.field "voicings" (Decode.list voicingDecoder)
            , Decode.succeed []
            ]
        )
        (Decode.oneOf
            [ Decode.field "voicingEnabled" Decode.bool
            , Decode.succeed True
            ]
        )
        (Decode.oneOf
            [ Decode.field "guitarFormEnabled" Decode.bool
            , Decode.succeed True
            ]
        )


voicingDecoder : Decode.Decoder Voicing
voicingDecoder =
    Decode.map3 Voicing
        (Decode.field "name" Decode.string)
        (Decode.field "offsets" (Decode.list Decode.int))
        (Decode.oneOf
            [ Decode.field "stringPicks" (Decode.list pickDecoder) |> Decode.map Set.fromList
            , Decode.succeed Set.empty
            ]
        )


pickDecoder : Decode.Decoder ( Int, Int )
pickDecoder =
    Decode.map2 Tuple.pair
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.int)


referenceAudioDecoder : Decode.Decoder ReferenceAudio
referenceAudioDecoder =
    Decode.map5 ReferenceAudio
        (Decode.field "fileName" (Decode.nullable Decode.string))
        (Decode.field "offsetMs" Decode.int)
        (Decode.field "volume" Decode.int)
        (Decode.field "muted" Decode.bool)
        (Decode.oneOf
            [ Decode.field "durationMs" (Decode.nullable Decode.int)
            , Decode.succeed Nothing
            ]
        )


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map2 Key
        (Decode.field "tonic" Decode.int)
        (Decode.field "mode" Decode.string
            |> Decode.andThen
                (\raw ->
                    case Data.Key.modeFromString raw of
                        Just mode ->
                            Decode.succeed mode

                        Nothing ->
                            Decode.fail ("未知の mode: " ++ raw)
                )
        )


meterDecoder : Decode.Decoder Meter
meterDecoder =
    Decode.map2 Meter
        (Decode.field "numerator" Decode.int)
        (Decode.field "denominator" Decode.int)


sectionDecoder : Decode.Decoder Section
sectionDecoder =
    Decode.map6 Section
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "lengthBars" Decode.int)
        (Decode.field "memo" Decode.string)
        (Decode.oneOf
            [ Decode.field "key" keyDecoder
            , Decode.succeed Data.Key.default
            ]
        )
        (Decode.oneOf
            [ Decode.field "meter" meterDecoder
            , Decode.succeed Data.Meter.default
            ]
        )


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
