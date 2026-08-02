module Midi.Encode exposing (fromProject)

import Bytes exposing (Bytes)
import Bytes.Encode as E
import Codec.Performance as Performance
import Data.Project exposing (Project)
import Midi.VarLen as VarLen


{-| Project を SMF Format 1 に展開する。再生と同じ Performance.toEvents を使うので、
聴こえる内容と .mid の内容は定義上一致する。
-}
fromProject : Project -> Bytes
fromProject project =
    let
        groups =
            eventGroups project

        chunks =
            headerChunk (1 + List.length groups)
                :: tempoTrackChunk project.bpm
                :: List.indexedMap noteTrackChunk groups
    in
    E.encode (E.sequence chunks)


eventGroups : Project -> List ( Int, List Performance.Event )
eventGroups project =
    let
        all =
            Performance.toEvents project

        ids =
            List.map .id project.tracks ++ [ -1 ]
    in
    ids
        |> List.map (\tid -> ( tid, List.filter (\e -> e.trackId == tid) all ))
        |> List.filter (\( _, evs ) -> not (List.isEmpty evs))


headerChunk : Int -> E.Encoder
headerChunk ntracks =
    E.sequence
        [ E.string "MThd"
        , E.unsignedInt32 Bytes.BE 6
        , E.unsignedInt16 Bytes.BE 1
        , E.unsignedInt16 Bytes.BE ntracks
        , E.unsignedInt16 Bytes.BE 480
        ]


trackChunk : List E.Encoder -> E.Encoder
trackChunk body =
    let
        bodyBytes =
            E.encode (E.sequence body)
    in
    E.sequence
        [ E.string "MTrk"
        , E.unsignedInt32 Bytes.BE (Bytes.width bodyBytes)
        , E.bytes bodyBytes
        ]


tempoTrackChunk : Int -> E.Encoder
tempoTrackChunk bpm =
    let
        usPerQuarter =
            60000000 // Basics.max 1 bpm
    in
    trackChunk
        [ VarLen.encode 0
        , E.sequence (List.map E.unsignedInt8 [ 0xFF, 0x51, 0x03 ])
        , E.unsignedInt8 (usPerQuarter // 65536)
        , E.unsignedInt8 (modBy 256 (usPerQuarter // 256))
        , E.unsignedInt8 (modBy 256 usPerQuarter)
        , VarLen.encode 0
        , E.sequence (List.map E.unsignedInt8 [ 0xFF, 0x58, 0x04, 0x04, 0x02, 0x18, 0x08 ])
        , endOfTrack
        ]


endOfTrack : E.Encoder
endOfTrack =
    E.sequence (VarLen.encode 0 :: List.map E.unsignedInt8 [ 0xFF, 0x2F, 0x00 ])


type alias MidiMessage =
    { tick : Int
    , isOn : Bool
    , pitch : Int
    , velocity : Int
    }


noteTrackChunk : Int -> ( Int, List Performance.Event ) -> E.Encoder
noteTrackChunk index ( _, events ) =
    let
        isDrum =
            events
                |> List.head
                |> Maybe.map (\e -> e.instrument == "drumKit")
                |> Maybe.withDefault False

        channel =
            if isDrum then
                9

            else
                channelFor index

        messages : List MidiMessage
        messages =
            events
                |> List.concatMap
                    (\e ->
                        [ { tick = e.ticks, isOn = True, pitch = clamp 0 127 e.pitch, velocity = clamp 1 127 e.velocity }
                        , { tick = e.ticks + e.durationTicks, isOn = False, pitch = clamp 0 127 e.pitch, velocity = 64 }
                        ]
                    )
                |> List.sortBy
                    (\m ->
                        ( m.tick
                        , if m.isOn then
                            1

                          else
                            0
                        )
                    )

        ( encoders, _ ) =
            List.foldl
                (\m ( acc, prevTick ) ->
                    ( acc
                        ++ [ VarLen.encode (m.tick - prevTick)
                           , E.unsignedInt8
                                ((if m.isOn then
                                    0x90

                                  else
                                    0x80
                                 )
                                    + channel
                                )
                           , E.unsignedInt8 m.pitch
                           , E.unsignedInt8 m.velocity
                           ]
                    , m.tick
                    )
                )
                ( [], 0 )
                messages
    in
    trackChunk (encoders ++ [ endOfTrack ])


channelFor : Int -> Int
channelFor index =
    let
        available =
            [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15 ]
    in
    available
        |> List.drop (modBy 15 index)
        |> List.head
        |> Maybe.withDefault 0
