module DrumPatternTest exposing (suite)

import Data.DrumPattern as DrumPattern
import Data.Key as Key
import Data.Project as Project
import Data.ReferenceAudio as ReferenceAudio
import Data.Time
import Data.Track exposing (Instrument(..), TrackKind(..))
import Expect
import Set
import Test exposing (Test, describe, test)


baseProject : Project.Project
baseProject =
    { name = "test"
    , bpm = 120.0
    , tracks =
        [ { id = 1, name = "drums", instrument = Piano, muted = False, volume = 100, kind = DrumTrack [] }
        ]
    , chordTrack = { text = "", instrument = Piano, muted = False, volume = 100, rhythm = Nothing }
    , sections = []
    , scraps = []
    , referenceAudio = ReferenceAudio.empty
    , nextId = 1000
    , memo = ""
    , voicings = []
    , voicingEnabled = True
    , guitarFormEnabled = True
    , ghostTrackIds = Set.empty
    }


{-| baseProject のトラック1のノートを指定リストに差し替えたプロジェクト。既存ノートを持つ状態から
apply する系のテスト（非破壊・レーン絞り込み）で使う。
-}
projectWithNotes : List { id : Int, pitch : Int, start : Int, duration : Int, velocity : Int } -> Project.Project
projectWithNotes notes =
    { baseProject | tracks = [ { id = 1, name = "drums", instrument = Piano, muted = False, volume = 100, kind = DrumTrack notes } ] }


notesInTrack : Int -> Project.Project -> List { id : Int, pitch : Int, start : Int, duration : Int, velocity : Int }
notesInTrack trackId project =
    project.tracks
        |> List.filter (\t -> t.id == trackId)
        |> List.head
        |> Maybe.map
            (\t ->
                case t.kind of
                    DrumTrack notes ->
                        notes

                    NoteTrack notes ->
                        notes
            )
        |> Maybe.withDefault []


patternOrEmpty : String -> DrumPattern.Pattern
patternOrEmpty name =
    DrumPattern.byName name |> Maybe.withDefault { name = "", hits = [] }


{-| 指定プリセットを 1小節分、Replace・AllLanes で apply した結果のノート列を返す（既存テスト互換）。
-}
notesFor : String -> List { id : Int, pitch : Int, start : Int, duration : Int, velocity : Int }
notesFor presetName =
    case DrumPattern.byName presetName of
        Just pattern ->
            DrumPattern.apply
                { trackId = 1, startTicks = 0, endTicks = Data.Time.ticksPerBar, mode = DrumPattern.Replace, lanes = DrumPattern.AllLanes }
                pattern
                baseProject
                |> notesInTrack 1

        Nothing ->
            []


velocityAt : Int -> Int -> List { a | pitch : Int, start : Int, velocity : Int } -> Maybe Int
velocityAt pitch step notes =
    notes
        |> List.filter (\n -> n.pitch == pitch && n.start == step * Data.Time.ticksPerSixteenth)
        |> List.head
        |> Maybe.map .velocity


suite : Test
suite =
    describe "Data.DrumPattern"
        [ describe "プリセットのvelocity強弱"
            [ test "全プリセットで velocity が 1-127 の範囲に収まる" <|
                \_ ->
                    let
                        allVelocities =
                            DrumPattern.patterns
                                |> List.concatMap (\p -> notesFor p.name |> List.map .velocity)
                    in
                    Expect.equal True (List.all (\v -> v >= 1 && v <= 127) allVelocities)
            , describe "各プリセットで velocity が単一値ではない（強弱がついている）"
                (List.map
                    (\p ->
                        test p.name <|
                            \_ ->
                                let
                                    velocities =
                                        notesFor p.name |> List.map .velocity
                                in
                                Expect.notEqual (List.minimum velocities) (List.maximum velocities)
                    )
                    DrumPattern.patterns
                )
            , test "8ビート: 小節頭のキック（pitch 36, step 0）はハイハットの裏拍（pitch 42, step 2）より強い" <|
                \_ ->
                    let
                        notes =
                            notesFor "8ビート"

                        kick =
                            velocityAt 36 0 notes |> Maybe.withDefault 0

                        hatOffbeat =
                            velocityAt 42 2 notes |> Maybe.withDefault 0
                    in
                    Expect.greaterThan hatOffbeat kick
            , test "4つ打ち: 小節頭のキック（pitch 36, step 0）はオープンHH（pitch 46, step 2）より強い" <|
                \_ ->
                    let
                        notes =
                            notesFor "4つ打ち"

                        kick =
                            velocityAt 36 0 notes |> Maybe.withDefault 0

                        openHat =
                            velocityAt 46 2 notes |> Maybe.withDefault 0
                    in
                    Expect.greaterThan openHat kick
            ]
        , describe "適用範囲"
            [ test "2小節の範囲でパターンが2周ぶん置かれ、2周目が +1小節ぶんずれる" <|
                \_ ->
                    let
                        pattern =
                            patternOrEmpty "4つ打ち"

                        result =
                            DrumPattern.apply
                                { trackId = 1, startTicks = 0, endTicks = Data.Time.ticksPerBar * 2, mode = DrumPattern.Replace, lanes = DrumPattern.AllLanes }
                                pattern
                                baseProject

                        kickStarts =
                            notesInTrack 1 result |> List.filter (\n -> n.pitch == 36) |> List.map .start |> List.sort
                    in
                    Expect.equal
                        [ 0, 480, 960, 1440, Data.Time.ticksPerBar, Data.Time.ticksPerBar + 480, Data.Time.ticksPerBar + 960, Data.Time.ticksPerBar + 1440 ]
                        kickStarts
            , test "1拍の範囲では step 0〜3 のヒットだけが残る（クリップ）" <|
                \_ ->
                    let
                        pattern =
                            patternOrEmpty "8ビート"

                        oneBeat =
                            4 * Data.Time.ticksPerSixteenth

                        result =
                            DrumPattern.apply
                                { trackId = 1, startTicks = 0, endTicks = oneBeat, mode = DrumPattern.Replace, lanes = DrumPattern.AllLanes }
                                pattern
                                baseProject

                        notes =
                            notesInTrack 1 result
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (List.any (\n -> n.pitch == 36 && n.start == 0) notes)
                        , \_ -> Expect.equal True (List.any (\n -> n.pitch == 42 && n.start == 2 * Data.Time.ticksPerSixteenth) notes)
                        , \_ -> Expect.equal False (List.any (\n -> n.start >= oneBeat) notes)
                        , \_ -> Expect.equal 3 (List.length notes)
                        ]
                        ()
            , test "startTicks 分だけオフセットすると範囲手前にノートが出ない" <|
                \_ ->
                    let
                        pattern =
                            patternOrEmpty "4つ打ち"

                        offset =
                            Data.Time.ticksPerBar

                        result =
                            DrumPattern.apply
                                { trackId = 1, startTicks = offset, endTicks = offset + Data.Time.ticksPerBar, mode = DrumPattern.Replace, lanes = DrumPattern.AllLanes }
                                pattern
                                baseProject
                    in
                    Expect.equal False (notesInTrack 1 result |> List.any (\n -> n.start < offset))
            , test "空範囲では project が完全に不変（undo に空エントリを積まない）" <|
                \_ ->
                    let
                        pattern =
                            patternOrEmpty "8ビート"

                        existingProject =
                            projectWithNotes [ { id = 1, pitch = 49, start = 100, duration = 120, velocity = 90 } ]

                        result =
                            DrumPattern.apply
                                { trackId = 1, startTicks = 500, endTicks = 500, mode = DrumPattern.Replace, lanes = DrumPattern.AllLanes }
                                pattern
                                existingProject
                    in
                    Expect.equal existingProject result
            ]
        , describe "非破壊モード"
            [ test "Replace は範囲内を pitch を問わず全消去する" <|
                \_ ->
                    let
                        project =
                            projectWithNotes [ { id = 1, pitch = 49, start = 0, duration = 120, velocity = 100 } ]

                        pattern =
                            patternOrEmpty "8ビート"

                        result =
                            DrumPattern.apply
                                { trackId = 1, startTicks = 0, endTicks = Data.Time.ticksPerBar, mode = DrumPattern.Replace, lanes = DrumPattern.AllLanes }
                                pattern
                                project
                    in
                    Expect.equal False (notesInTrack 1 result |> List.any (\n -> n.pitch == 49))
            , test "ReplaceLanes はパターンに含まれない pitch を範囲内でも残す" <|
                \_ ->
                    let
                        project =
                            projectWithNotes [ { id = 1, pitch = 49, start = 0, duration = 120, velocity = 100 } ]

                        pattern =
                            patternOrEmpty "8ビート"

                        result =
                            DrumPattern.apply
                                { trackId = 1, startTicks = 0, endTicks = Data.Time.ticksPerBar, mode = DrumPattern.ReplaceLanes, lanes = DrumPattern.AllLanes }
                                pattern
                                project
                    in
                    Expect.equal True (notesInTrack 1 result |> List.any (\n -> n.pitch == 49 && n.id == 1))
            , test "ReplaceLanes は同じ適用を2回押しても結果が変わらない（冪等）" <|
                \_ ->
                    let
                        pattern =
                            patternOrEmpty "8ビート"

                        cfg =
                            { trackId = 1, startTicks = 0, endTicks = Data.Time.ticksPerBar, mode = DrumPattern.ReplaceLanes, lanes = DrumPattern.AllLanes }

                        once =
                            DrumPattern.apply cfg pattern baseProject

                        twice =
                            DrumPattern.apply cfg pattern once

                        pairsOf project =
                            notesInTrack 1 project |> List.map (\n -> ( n.pitch, n.start )) |> List.sort
                    in
                    Expect.equal (pairsOf once) (pairsOf twice)
            , test "Merge は既存ノートと pitch・start が一致すれば新規を足さず、既存の velocity を保つ" <|
                \_ ->
                    let
                        existingNote =
                            { id = 42, pitch = 36, start = 0, duration = 120, velocity = 77 }

                        project =
                            projectWithNotes [ existingNote ]

                        pattern =
                            patternOrEmpty "8ビート"

                        result =
                            DrumPattern.apply
                                { trackId = 1, startTicks = 0, endTicks = Data.Time.ticksPerBar, mode = DrumPattern.Merge, lanes = DrumPattern.AllLanes }
                                pattern
                                project

                        kickAtZero =
                            notesInTrack 1 result |> List.filter (\n -> n.pitch == 36 && n.start == 0)
                    in
                    Expect.equal [ existingNote ] kickAtZero
            , test "Merge は既存ノートを一つも消さない" <|
                \_ ->
                    let
                        project =
                            projectWithNotes [ { id = 42, pitch = 49, start = 999999, duration = 120, velocity = 50 } ]

                        pattern =
                            patternOrEmpty "8ビート"

                        result =
                            DrumPattern.apply
                                { trackId = 1, startTicks = 0, endTicks = Data.Time.ticksPerBar, mode = DrumPattern.Merge, lanes = DrumPattern.AllLanes }
                                pattern
                                project
                    in
                    Expect.equal True (notesInTrack 1 result |> List.map .id |> List.member 42)
            , test "Merge の dedupe された分は id を消費しない" <|
                \_ ->
                    let
                        project =
                            projectWithNotes [ { id = 42, pitch = 36, start = 0, duration = 120, velocity = 77 } ]

                        pattern =
                            patternOrEmpty "8ビート"

                        result =
                            DrumPattern.apply
                                { trackId = 1, startTicks = 0, endTicks = Data.Time.ticksPerBar, mode = DrumPattern.Merge, lanes = DrumPattern.AllLanes }
                                pattern
                                project
                    in
                    Expect.equal (1000 + (List.length pattern.hits - 1)) result.nextId
            ]
        , describe "レーン絞り込み"
            [ test "OnlyLanes [36] + ReplaceLanes はキックだけ書き込み、他の pitch は無傷" <|
                \_ ->
                    let
                        project =
                            projectWithNotes [ { id = 1, pitch = 42, start = 0, duration = 120, velocity = 90 } ]

                        pattern =
                            patternOrEmpty "4つ打ち"

                        result =
                            DrumPattern.apply
                                { trackId = 1, startTicks = 0, endTicks = Data.Time.ticksPerBar, mode = DrumPattern.ReplaceLanes, lanes = DrumPattern.OnlyLanes (Set.fromList [ 36 ]) }
                                pattern
                                project

                        notes =
                            notesInTrack 1 result
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (List.any (\n -> n.pitch == 36) notes)
                        , \_ -> Expect.equal True (List.any (\n -> n.pitch == 42 && n.id == 1) notes)
                        , \_ -> Expect.equal False (List.any (\n -> n.pitch == 46) notes)
                        ]
                        ()
            , test "OnlyLanes が空集合なら project が不変" <|
                \_ ->
                    let
                        pattern =
                            patternOrEmpty "8ビート"

                        result =
                            DrumPattern.apply
                                { trackId = 1, startTicks = 0, endTicks = Data.Time.ticksPerBar, mode = DrumPattern.ReplaceLanes, lanes = DrumPattern.OnlyLanes Set.empty }
                                pattern
                                baseProject
                    in
                    Expect.equal baseProject result
            ]
        , describe "変拍子"
            [ test "3/4 セクションでは各周で step 12〜15 が落ちる（1440 ticks = 12ステップ）" <|
                \_ ->
                    let
                        section =
                            { id = 1, name = "s", lengthBars = 3, memo = "", key = Key.default, meter = { numerator = 3, denominator = 4 } }

                        project =
                            { baseProject | sections = [ section ] }

                        pattern =
                            patternOrEmpty "8ビート"

                        result =
                            DrumPattern.apply
                                { trackId = 1, startTicks = 0, endTicks = 1440 * 3, mode = DrumPattern.Replace, lanes = DrumPattern.AllLanes }
                                pattern
                                project

                        hatInSecondCycle =
                            notesInTrack 1 result
                                |> List.filter (\n -> n.pitch == 42 && n.start >= 1440 && n.start < 2880)
                    in
                    Expect.equal 6 (List.length hatInSecondCycle)
            , test "6/8 も 3/4 と同じ 12ステップになる（1440 ticks）" <|
                \_ ->
                    let
                        section =
                            { id = 1, name = "s", lengthBars = 1, memo = "", key = Key.default, meter = { numerator = 6, denominator = 8 } }

                        project =
                            { baseProject | sections = [ section ] }

                        pattern =
                            patternOrEmpty "8ビート"

                        result =
                            DrumPattern.apply
                                { trackId = 1, startTicks = 0, endTicks = 1440, mode = DrumPattern.Replace, lanes = DrumPattern.AllLanes }
                                pattern
                                project

                        hatNotes =
                            notesInTrack 1 result |> List.filter (\n -> n.pitch == 42)
                    in
                    Expect.equal 6 (List.length hatNotes)
            , test "3/4→4/4 の切替をまたぐと周の開始が 0, 1440, 2880, 4800 になる" <|
                \_ ->
                    let
                        threeFour =
                            { id = 1, name = "a", lengthBars = 2, memo = "", key = Key.default, meter = { numerator = 3, denominator = 4 } }

                        fourFour =
                            { id = 2, name = "b", lengthBars = 2, memo = "", key = Key.default, meter = Key.default |> always { numerator = 4, denominator = 4 } }

                        project =
                            { baseProject | sections = [ threeFour, fourFour ] }

                        pattern =
                            patternOrEmpty "4つ打ち"

                        result =
                            DrumPattern.apply
                                { trackId = 1, startTicks = 0, endTicks = 1440 * 2 + 1920 * 2, mode = DrumPattern.Replace, lanes = DrumPattern.AllLanes }
                                pattern
                                project

                        kickStarts =
                            notesInTrack 1 result |> List.filter (\n -> n.pitch == 36) |> List.map .start |> List.sort
                    in
                    Expect.equal
                        [ 0, 480, 960, 1440, 1920, 2400, 2880, 3360, 3840, 4320, 4800, 5280, 5760, 6240 ]
                        kickStarts
            , test "5/4 ではパターンが全て入り、start の最大値が 2400 を超えない" <|
                \_ ->
                    let
                        section =
                            { id = 1, name = "s", lengthBars = 1, memo = "", key = Key.default, meter = { numerator = 5, denominator = 4 } }

                        project =
                            { baseProject | sections = [ section ] }

                        pattern =
                            patternOrEmpty "16ビート"

                        result =
                            DrumPattern.apply
                                { trackId = 1, startTicks = 0, endTicks = 2400, mode = DrumPattern.Replace, lanes = DrumPattern.AllLanes }
                                pattern
                                project

                        notes =
                            notesInTrack 1 result
                    in
                    Expect.all
                        [ \_ -> Expect.equal (List.length pattern.hits) (List.length notes)
                        , \_ -> Expect.equal True (notes |> List.map .start |> List.all (\s -> s < 2400))
                        ]
                        ()
            ]
        ]
