module ProjectEditTest exposing (suite)

import Data.Key as Key
import Data.Meter as Meter
import Data.Project as Project
import Data.ReferenceAudio as ReferenceAudio
import Data.Timeline as Timeline
import Data.Track exposing (Instrument(..), TrackKind(..))
import Expect
import Test exposing (Test, describe, test)


note : Int -> Int -> Int -> Int -> { id : Int, pitch : Int, start : Int, duration : Int, velocity : Int }
note id start duration pitch =
    { id = id, pitch = pitch, start = start, duration = duration, velocity = 100 }


section : Int -> Int -> Meter.Meter -> { id : Int, name : String, lengthBars : Int, memo : String, key : Key.Key, meter : Meter.Meter }
section id lengthBars meter =
    { id = id, name = "s" ++ String.fromInt id, lengthBars = lengthBars, memo = "", key = Key.default, meter = meter }


baseProject : List { id : Int, name : String, lengthBars : Int, memo : String, key : Key.Key, meter : Meter.Meter } -> List { id : Int, pitch : Int, start : Int, duration : Int, velocity : Int } -> String -> Project.Project
baseProject sections notes chordText =
    { name = "test"
    , bpm = 120.0
    , tracks =
        [ { id = 1, name = "track", instrument = Piano, muted = False, volume = 100, kind = NoteTrack notes }
        ]
    , chordTrack = { text = chordText, instrument = Piano, muted = False, volume = 100 }
    , sections = sections
    , scraps = []
    , referenceAudio = ReferenceAudio.empty
    , nextId = 1000
    , memo = ""
    , voicings = []
    , voicingEnabled = True
    }


ticksPerBar4_4 : Int
ticksPerBar4_4 =
    Meter.ticksPerBar Meter.default


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
    describe "Data.Project insertBars / removeBars"
        [ test "insertBars: 挿入点以降のノートは後ろにシフトし、前は動かない" <|
            \_ ->
                let
                    project =
                        baseProject
                            [ section 1 4 Meter.default ]
                            [ note 1 0 ticksPerBar4_4 60
                            , note 2 (ticksPerBar4_4 * 2) ticksPerBar4_4 64
                            ]
                            "C | G | Am | F"

                    result =
                        Project.insertBars { beforeBar = 2, count = 1 } project

                    notes =
                        firstTrackNotes result |> List.sortBy .id
                in
                Expect.equal
                    [ 0, ticksPerBar4_4 * 3 ]
                    (List.map .start notes)
        , test "insertBars: 対象セクションの lengthBars が count 分増える" <|
            \_ ->
                let
                    project =
                        baseProject [ section 1 4 Meter.default ] [] "C | G | Am | F"

                    result =
                        Project.insertBars { beforeBar = 2, count = 2 } project
                in
                Expect.equal (Just 6) (result.sections |> List.head |> Maybe.map .lengthBars)
        , test "insertBars: 拍子混在でも barAt 経由で正しい tick 量を挿入する" <|
            \_ ->
                let
                    threeFour =
                        { numerator = 3, denominator = 4 }

                    project =
                        baseProject
                            [ section 1 2 Meter.default, section 2 2 threeFour ]
                            [ note 1 (ticksPerBar4_4 * 3) ticksPerBar4_4 60 ]
                            ""

                    -- bar 2 は2つ目のセクション（3/4）の先頭
                    result =
                        Project.insertBars { beforeBar = 2, count = 1 } project

                    expectedDelta =
                        Meter.ticksPerBar threeFour

                    notes =
                        firstTrackNotes result
                in
                Expect.equal [ ticksPerBar4_4 * 3 + expectedDelta ] (List.map .start notes)
        , test "insertBars: 参考オーディオの offsetMs が挿入分だけ前へずれる（後ろにずらす）" <|
            \_ ->
                let
                    project =
                        baseProject [ section 1 4 Meter.default ] [] ""

                    result =
                        Project.insertBars { beforeBar = 0, count = 1 } project

                    expectedMs =
                        round (toFloat ticksPerBar4_4 / 480 * 60 / 120.0 * 1000)
                in
                Expect.equal (negate expectedMs) result.referenceAudio.offsetMs
        , test "removeBars: 範囲内に開始するノートは削除される" <|
            \_ ->
                let
                    project =
                        baseProject
                            [ section 1 4 Meter.default ]
                            [ note 1 ticksPerBar4_4 ticksPerBar4_4 60 ]
                            ""

                    result =
                        Project.removeBars { fromBar = 1, count = 1 } project
                in
                Expect.equal [] (firstTrackNotes result)
        , test "removeBars: 範囲を跨ぐノートは削除範囲の始まりで短縮される" <|
            \_ ->
                let
                    project =
                        baseProject
                            [ section 1 4 Meter.default ]
                            [ note 1 0 (ticksPerBar4_4 * 2) 60 ]
                            ""

                    result =
                        Project.removeBars { fromBar = 1, count = 1 } project

                    notes =
                        firstTrackNotes result
                in
                Expect.equal [ ( 0, ticksPerBar4_4 ) ] (List.map (\n -> ( n.start, n.duration )) notes)
        , test "removeBars: 範囲後のノートは削除分だけ前へシフトする" <|
            \_ ->
                let
                    project =
                        baseProject
                            [ section 1 4 Meter.default ]
                            [ note 1 (ticksPerBar4_4 * 3) ticksPerBar4_4 60 ]
                            ""

                    result =
                        Project.removeBars { fromBar = 1, count = 1 } project

                    notes =
                        firstTrackNotes result
                in
                Expect.equal [ ticksPerBar4_4 * 2 ] (List.map .start notes)
        , test "removeBars: セクションの lengthBars が削除分だけ減る" <|
            \_ ->
                let
                    project =
                        baseProject [ section 1 4 Meter.default ] [] ""

                    result =
                        Project.removeBars { fromBar = 1, count = 2 } project
                in
                Expect.equal (Just 2) (result.sections |> List.head |> Maybe.map .lengthBars)
        , test "removeBars: lengthBars が0以下になったセクションは削除される" <|
            \_ ->
                let
                    project =
                        baseProject [ section 1 2 Meter.default ] [] ""

                    result =
                        Project.removeBars { fromBar = 0, count = 2 } project
                in
                Expect.equal [] result.sections
        , test "removeBars: 参考オーディオの offsetMs が削除分だけ前へすすむ（削除分を足す）" <|
            \_ ->
                let
                    project =
                        baseProject [ section 1 4 Meter.default ] [] ""

                    result =
                        Project.removeBars { fromBar = 0, count = 1 } project

                    expectedMs =
                        round (toFloat ticksPerBar4_4 / 480 * 60 / 120.0 * 1000)
                in
                Expect.equal expectedMs result.referenceAudio.offsetMs
        , test "removeBars: 拍子混在の範囲でも barAt 経由で正しい tick 量を削除する" <|
            \_ ->
                let
                    threeFour =
                        { numerator = 3, denominator = 4 }

                    project =
                        baseProject
                            [ section 1 2 Meter.default, section 2 2 threeFour ]
                            [ note 1 (ticksPerBar4_4 * 4) ticksPerBar4_4 60 ]
                            ""

                    -- bar 2・3 は2つ目のセクション（3/4）の2小節
                    result =
                        Project.removeBars { fromBar = 2, count = 2 } project

                    expectedDelta =
                        Meter.ticksPerBar threeFour * 2

                    notes =
                        firstTrackNotes result
                in
                Expect.equal [ ticksPerBar4_4 * 4 - expectedDelta ] (List.map .start notes)
        ]
