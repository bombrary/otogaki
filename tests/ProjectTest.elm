module ProjectTest exposing (suite)

import Data.ChordTrack
import Data.Key
import Data.Meter
import Data.Note exposing (Note)
import Data.Project as Project
import Data.Section exposing (Section)
import Data.Timeline as Timeline
import Data.Track exposing (TrackKind(..))
import Expect
import Test exposing (Test, describe, test)


section : Int -> Section
section id =
    { id = id, name = "s" ++ String.fromInt id, lengthBars = 4, memo = "", key = Data.Key.default, meter = Data.Meter.default }


sectionWithMeter : Int -> Data.Meter.Meter -> Int -> Section
sectionWithMeter id meter lengthBars =
    { id = id, name = "s" ++ String.fromInt id, lengthBars = lengthBars, memo = "", key = Data.Key.default, meter = meter }


note : Int -> Int -> Note
note id start =
    { id = id, pitch = 60, start = start, duration = 100, velocity = 100 }


suite : Test
suite =
    Test.concat
        [ timelineSuite
        , moveSectionToIndexSuite
        , reorderSectionsWithContentSuite
        ]


timelineSuite : Test
timelineSuite =
    describe "Data.Project.timeline"
        [ test "参考オーディオが長ければ minBars が伸びる" <|
            \_ ->
                let
                    project =
                        Project.demo

                    ra =
                        project.referenceAudio

                    -- 120bpm 4/4 で 60秒（=120拍=30小節）分の参考音声
                    withLongRefAudio =
                        { project | referenceAudio = { ra | durationMs = Just 60000 } }

                    tl =
                        Project.timeline withLongRefAudio
                in
                Expect.atLeast 30 (Timeline.totalBars tl)
        , test "参考オーディオがなければ従来通りの minBars（16）を下回らない" <|
            \_ ->
                let
                    project =
                        Project.demo

                    tl =
                        Project.timeline project
                in
                Expect.atLeast 16 (Timeline.totalBars tl)
        , test "offsetMs の分だけ実効長が短くなり minBars が小さくなる" <|
            \_ ->
                let
                    project =
                        Project.demo

                    ra =
                        project.referenceAudio

                    withOffset =
                        { project | referenceAudio = { ra | durationMs = Just 60000, offsetMs = 30000 } }

                    withoutOffset =
                        { project | referenceAudio = { ra | durationMs = Just 60000, offsetMs = 0 } }
                in
                Expect.atMost
                    (Timeline.totalBars (Project.timeline withoutOffset))
                    (Timeline.totalBars (Project.timeline withOffset))
        ]


moveSectionToIndexSuite : Test
moveSectionToIndexSuite =
    let
        project =
            { demo | sections = [ section 1, section 2, section 3 ] }

        demo =
            Project.demo
    in
    describe "Data.Project.moveSectionToIndex"
        [ test "後ろの index に移動できる" <|
            \_ ->
                (Project.moveSectionToIndex 1 2 project).sections
                    |> List.map .id
                    |> Expect.equal [ 2, 3, 1 ]
        , test "前の index に移動できる" <|
            \_ ->
                (Project.moveSectionToIndex 3 0 project).sections
                    |> List.map .id
                    |> Expect.equal [ 3, 1, 2 ]
        , test "負の index は先頭にクランプされる" <|
            \_ ->
                (Project.moveSectionToIndex 3 -5 project).sections
                    |> List.map .id
                    |> Expect.equal [ 3, 1, 2 ]
        , test "大きすぎる index は末尾にクランプされる" <|
            \_ ->
                (Project.moveSectionToIndex 1 99 project).sections
                    |> List.map .id
                    |> Expect.equal [ 2, 3, 1 ]
        , test "存在しない sectionId を渡すと何も変わらない" <|
            \_ ->
                (Project.moveSectionToIndex 999 0 project).sections
                    |> List.map .id
                    |> Expect.equal [ 1, 2, 3 ]
        ]


reorderSectionsWithContentSuite : Test
reorderSectionsWithContentSuite =
    let
        demo =
            Project.demo

        withTrackAndSections sections notes chordText baseProject =
            { baseProject
                | sections = sections
                , tracks = [ { id = 1, name = "t", instrument = Data.Track.instrumentFromString "Piano" |> Maybe.withDefault Data.Track.Piano, muted = False, volume = 100, kind = NoteTrack notes } ]
                , chordTrack = { text = chordText, instrument = Data.Track.Piano, muted = False, volume = 100 }
            }

        ticksPerBar44 =
            Data.Meter.ticksPerBar Data.Meter.default

        project2x4 =
            demo
                |> withTrackAndSections
                    [ section 1, section 2 ]
                    [ note 1 0, note 2 (ticksPerBar44 * 4) ]
                    "A | B | C | D | E | F | G | H"

        allNoteStarts p =
            p.tracks
                |> List.concatMap
                    (\t ->
                        case t.kind of
                            NoteTrack ns ->
                                List.map .start ns

                            DrumTrack ns ->
                                List.map .start ns
                    )
                |> List.sort
    in
    describe "Data.Project.reorderSectionsWithContent 経由の moveSectionToIndex"
        [ test "4/4 2セクション swap でノートが入れ替わる" <|
            \_ ->
                (Project.moveSectionToIndex 2 0 project2x4 |> allNoteStarts)
                    |> Expect.equal [ 0, ticksPerBar44 * 4 ]
        , test "4/4 2セクション swap でコード小節が入れ替わる" <|
            \_ ->
                let
                    moved =
                        Project.moveSectionToIndex 2 0 project2x4
                in
                Data.ChordTrack.cells (Project.timeline moved) moved.chordTrack
                    |> List.map (\c -> c.chords |> List.map .token |> String.join "")
                    |> List.take 8
                    |> Expect.equal [ "E", "F", "G", "H", "A", "B", "C", "D" ]
        , test "拍子混在（3/4x2 + 4/4x1）でも swap 後の tick が正しい" <|
            \_ ->
                let
                    meter34 =
                        { numerator = 3, denominator = 4 }

                    ticksPerBar34 =
                        Data.Meter.ticksPerBar meter34

                    projectMixed =
                        demo
                            |> withTrackAndSections
                                [ sectionWithMeter 1 meter34 2, sectionWithMeter 2 Data.Meter.default 1 ]
                                [ note 1 0, note 2 (ticksPerBar34 * 2) ]
                                ""

                    moved =
                        Project.moveSectionToIndex 2 0 projectMixed
                in
                allNoteStarts moved
                    |> Expect.equal [ 0, ticksPerBar44 ]
        , test "セクション合計を超えた余白領域のノートは動かない" <|
            \_ ->
                let
                    overflowStart =
                        ticksPerBar44 * 8 + 123

                    projectWithOverflow =
                        project2x4
                            |> withTrackAndSections
                                [ section 1, section 2 ]
                                [ note 1 0, note 2 (ticksPerBar44 * 4), note 3 overflowStart ]
                                "A | B | C | D | E | F | G | H"

                    moved =
                        Project.moveSectionToIndex 2 0 projectWithOverflow
                in
                allNoteStarts moved
                    |> Expect.equal [ 0, ticksPerBar44 * 4, overflowStart ]
        , test "2ステップ移動のローテーションでも内容が正しく付いてくる" <|
            \_ ->
                let
                    project3 =
                        demo
                            |> withTrackAndSections
                                [ section 1, section 2, section 3 ]
                                [ note 1 0, note 2 (ticksPerBar44 * 4), note 3 (ticksPerBar44 * 8) ]
                                "A | B | C | D | E | F | G | H | I | J | K | L"

                    moved =
                        Project.moveSectionToIndex 1 2 project3
                in
                ( moved.sections |> List.map .id, allNoteStarts moved )
                    |> Expect.equal ( [ 2, 3, 1 ], [ 0, ticksPerBar44 * 4, ticksPerBar44 * 8 ] )
        ]
