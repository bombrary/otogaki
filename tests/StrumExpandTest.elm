module StrumExpandTest exposing (suite)

import Data.Key as Key
import Data.Meter as Meter
import Data.Project as Project exposing (Project)
import Data.ReferenceAudio as ReferenceAudio
import Data.StrumExpand as StrumExpand
import Data.StrumPattern exposing (Direction(..), Pattern)
import Data.Time
import Data.Track exposing (Instrument(..), TrackKind(..))
import Expect
import Set
import Test exposing (Test, describe, test)


testProject : String -> Project
testProject chordText =
    { name = "test"
    , bpm = 120.0
    , tracks = [ { id = 1, name = "guitar", instrument = AcousticGuitar, muted = False, volume = 100, kind = NoteTrack [] } ]
    , chordTrack = { text = chordText, instrument = Piano, muted = False, volume = 100 }
    , sections = [ { id = 2, name = "A", lengthBars = 2, memo = "", key = Key.default, meter = Meter.default } ]
    , scraps = []
    , referenceAudio = ReferenceAudio.empty
    , nextId = 100
    , memo = ""
    , voicings = []
    , voicingEnabled = True
    }


notesOfTrack : Int -> Project -> List { id : Int, pitch : Int, start : Int, duration : Int, velocity : Int }
notesOfTrack trackId project =
    project.tracks
        |> List.filter (\t -> t.id == trackId)
        |> List.head
        |> Maybe.map
            (\t ->
                case t.kind of
                    NoteTrack notes ->
                        notes

                    DrumTrack notes ->
                        notes
            )
        |> Maybe.withDefault []


downUpPattern : Pattern
downUpPattern =
    { name = "test", strums = [ { step = 0, direction = Down, velocity = 100 }, { step = 4, direction = Up, velocity = 100 } ] }


suite : Test
suite =
    describe "Data.StrumExpand"
        [ test "コード区間をまたぐノートが生えない" <|
            \_ ->
                let
                    result =
                        StrumExpand.apply { trackId = 1, startTicks = 0, endTicks = 2 * Data.Time.ticksPerBar } [] downUpPattern (testProject "C | G")

                    notes =
                        notesOfTrack 1 result

                    firstBarNotes =
                        List.filter (\n -> n.start < Data.Time.ticksPerBar) notes
                in
                Expect.equal True (List.all (\n -> n.start + n.duration <= Data.Time.ticksPerBar) firstBarNotes)
        , test "Down と Up で弦の順序が逆になる" <|
            \_ ->
                let
                    result =
                        StrumExpand.apply { trackId = 1, startTicks = 0, endTicks = Data.Time.ticksPerBar } [] downUpPattern (testProject "C")

                    notes =
                        notesOfTrack 1 result |> List.sortBy .start

                    downGroup =
                        List.filter (\n -> n.start < 4 * Data.Time.ticksPerSixteenth) notes

                    upGroup =
                        List.filter (\n -> n.start >= 4 * Data.Time.ticksPerSixteenth) notes

                    downPitches =
                        List.map .pitch downGroup

                    upPitches =
                        List.map .pitch upGroup
                in
                Expect.equal ( True, True )
                    ( List.sort downPitches == downPitches
                    , List.sort upPitches == List.reverse upPitches
                    )
        , test "nextId が重複しない" <|
            \_ ->
                let
                    result =
                        StrumExpand.apply { trackId = 1, startTicks = 0, endTicks = 2 * Data.Time.ticksPerBar } [] downUpPattern (testProject "C | G")

                    notes =
                        notesOfTrack 1 result

                    ids =
                        List.map .id notes
                in
                Expect.equal (List.length ids) (Set.size (Set.fromList ids))
        ]
