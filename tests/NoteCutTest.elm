module NoteCutTest exposing (suite)

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
    describe "Note cut"
        [ test "Note.splitAt: 中央分割は duration合計一致・後半start=tick・pitch/velocity保持" <|
            \_ ->
                let
                    n =
                        note 1 0 160 60
                in
                case Note.splitAt 60 n of
                    Just ( first, second ) ->
                        Expect.equal
                            { durationSum = n.duration, secondStart = 60, secondPitch = n.pitch, secondVelocity = n.velocity }
                            { durationSum = first.duration + second.duration, secondStart = second.start, secondPitch = second.pitch, secondVelocity = second.velocity }

                    Nothing ->
                        Expect.fail "splitAt should split a note that fully contains the tick"
        , test "Note.splitAt: 境界一致・範囲外は分割しない" <|
            \_ ->
                let
                    n =
                        note 1 0 160 60
                in
                Expect.equal
                    [ Nothing, Nothing, Nothing ]
                    [ Note.splitAt 0 n, Note.splitAt 160 n, Note.splitAt 300 n ]
        , test "Project.cutNotesAt: 選択2個+非選択1個がまたぐ → 選択のみ分割、非選択は不変" <|
            \_ ->
                let
                    selectedA =
                        note 1 0 160 60

                    selectedB =
                        note 2 20 160 62

                    unselected =
                        note 3 40 160 64

                    project =
                        baseProject [ selectedA, selectedB, unselected ]

                    result =
                        Project.cutNotesAt { trackId = 1, tick = 60, targetIds = Set.fromList [ 1, 2 ] } project

                    notes =
                        firstTrackNotes result.project
                in
                Expect.equal
                    ( 5, True, Just 60 )
                    ( List.length notes
                    , List.member unselected notes
                    , notes |> List.filter (\nn -> nn.id == 1) |> List.head |> Maybe.map .duration
                    )
        , test "Project.cutNotesAt: id採番は nextIdから連番・個数ぶん進む・衝突なし" <|
            \_ ->
                let
                    a =
                        note 1 0 160 60

                    b =
                        note 2 20 160 62

                    project =
                        baseProject [ a, b ]

                    result =
                        Project.cutNotesAt { trackId = 1, tick = 60, targetIds = Set.fromList [ 1, 2 ] } project

                    ids =
                        firstTrackNotes result.project |> List.map .id |> List.sort
                in
                Expect.equal
                    ( project.nextId + 2, [ 1, 2, project.nextId, project.nextId + 1 ] )
                    ( result.project.nextId, ids )
        , test "Project.cutNotesAt: またぐノートなし → projectは完全不変" <|
            \_ ->
                let
                    a =
                        note 1 0 40 60

                    project =
                        baseProject [ a ]

                    result =
                        Project.cutNotesAt { trackId = 1, tick = 100, targetIds = Set.fromList [ 1 ] } project
                in
                Expect.equal project result.project
        ]
