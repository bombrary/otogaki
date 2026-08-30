module NoteMergeTest exposing (suite)

import Data.Key as Key
import Data.Meter as Meter
import Data.Note as Note
import Data.Project as Project
import Data.ReferenceAudio as ReferenceAudio
import Data.Track exposing (Instrument(..), TrackKind(..))
import Expect
import Set
import Test exposing (Test, describe, test)


note : Int -> Int -> Int -> Int -> { id : Int, pitch : Int, start : Int, duration : Int, velocity : Int }
note id start duration pitch =
    { id = id, pitch = pitch, start = start, duration = duration, velocity = 100 }


baseProject : List { id : Int, pitch : Int, start : Int, duration : Int, velocity : Int } -> Project.Project
baseProject notes =
    { name = "test"
    , bpm = 120.0
    , tracks =
        [ { id = 1, name = "track", instrument = Piano, muted = False, volume = 100, kind = NoteTrack notes } ]
    , chordTrack = { text = "", instrument = Piano, muted = False, volume = 100, rhythm = Nothing }
    , sections = [ { id = 2, name = "s", lengthBars = 4, memo = "", key = Key.default, meter = Meter.default } ]
    , scraps = []
    , referenceAudio = ReferenceAudio.empty
    , nextId = 1000
    , memo = ""
    , voicings = []
    , voicingEnabled = True
    , guitarFormEnabled = True
    , ghostTrackIds = Set.empty
    }


firstTrackNotes : Project.Project -> List { id : Int, pitch : Int, start : Int, duration : Int, velocity : Int }
firstTrackNotes project =
    project.tracks
        |> List.head
        |> Maybe.map
            (\t ->
                case t.kind of
                    NoteTrack ns ->
                        ns

                    DrumTrack ns ->
                        ns
            )
        |> Maybe.withDefault []


suite : Test
suite =
    describe "Note merge"
        [ test "Note.mergeAdjacent: 同 pitch隣接は結合しduration合計・前半id/velocityを維持" <|
            \_ ->
                let
                    a =
                        note 1 0 80 60

                    b =
                        note 2 80 80 60
                in
                case Note.mergeAdjacent a b of
                    Just merged ->
                        Expect.equal
                            { id = a.id, pitch = a.pitch, start = a.start, duration = a.duration + b.duration, velocity = a.velocity }
                            merged

                    Nothing ->
                        Expect.fail "mergeAdjacent should merge two contiguous same-pitch notes"
        , test "Note.mergeAdjacent: pitch不一致は結合しない" <|
            \_ ->
                let
                    a =
                        note 1 0 80 60

                    b =
                        note 2 80 80 61
                in
                Expect.equal Nothing (Note.mergeAdjacent a b)
        , test "Note.mergeAdjacent: 隔たりがある場合は結合しない" <|
            \_ ->
                let
                    a =
                        note 1 0 80 60

                    b =
                        note 2 100 80 60
                in
                Expect.equal Nothing (Note.mergeAdjacent a b)
        , test "Project.mergeNotes: 隣接2件選択 → 1件に結合しduration合計・id/velocityは先頭を維持" <|
            \_ ->
                let
                    a =
                        note 1 0 80 60

                    b =
                        note 2 80 80 60

                    project =
                        baseProject [ a, b ]

                    result =
                        Project.mergeNotes { trackId = 1, targetIds = Set.fromList [ 1, 2 ] } project

                    notes =
                        firstTrackNotes result.project
                in
                Expect.equal
                    ( [ { id = 1, pitch = 60, start = 0, duration = 160, velocity = 100 } ], Set.fromList [ 1 ] )
                    ( notes, result.newSelection )
        , test "Project.mergeNotes: 3連鎖(A-B-C全選択・隣接)は1本にまとまる" <|
            \_ ->
                let
                    a =
                        note 1 0 40 60

                    b =
                        note 2 40 40 60

                    c =
                        note 3 80 40 60

                    project =
                        baseProject [ a, b, c ]

                    result =
                        Project.mergeNotes { trackId = 1, targetIds = Set.fromList [ 1, 2, 3 ] } project

                    notes =
                        firstTrackNotes result.project
                in
                Expect.equal
                    ( [ { id = 1, pitch = 60, start = 0, duration = 120, velocity = 100 } ], Set.fromList [ 1 ] )
                    ( notes, result.newSelection )
        , test "Project.mergeNotes: 選択に非隣接ノートが含まれても隣接ペアだけが影響を受ける" <|
            \_ ->
                let
                    a =
                        note 1 0 40 60

                    b =
                        note 2 40 40 60

                    -- c は a・b と離れた(隔たりのある)同pitchノート。選択に含まれるがペアは組めない
                    c =
                        note 3 200 40 60

                    project =
                        baseProject [ a, b, c ]

                    result =
                        Project.mergeNotes { trackId = 1, targetIds = Set.fromList [ 1, 2, 3 ] } project

                    notes =
                        firstTrackNotes result.project |> List.sortBy .id
                in
                Expect.equal
                    ( [ { id = 1, pitch = 60, start = 0, duration = 80, velocity = 100 }, c ]
                    , Set.fromList [ 1, 3 ]
                    )
                    ( notes, result.newSelection )
        , test "Project.mergeNotes: 選択が空 → projectは完全不変" <|
            \_ ->
                let
                    a =
                        note 1 0 40 60

                    project =
                        baseProject [ a ]

                    result =
                        Project.mergeNotes { trackId = 1, targetIds = Set.empty } project
                in
                Expect.equal project result.project
        , test "Project.mergeNotes: 選択中に隣接ペアがない → projectは完全不変" <|
            \_ ->
                let
                    a =
                        note 1 0 40 60

                    b =
                        note 2 200 40 60

                    project =
                        baseProject [ a, b ]

                    result =
                        Project.mergeNotes { trackId = 1, targetIds = Set.fromList [ 1, 2 ] } project
                in
                Expect.equal project result.project
        ]
