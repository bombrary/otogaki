module ChordSheetTest exposing (suite)

import Data.ChordSheet as ChordSheet
import Data.ChordTrack as ChordTrack
import Data.Key
import Data.Meter
import Data.Project
import Data.Timeline
import Data.Track exposing (Instrument(..))
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    Test.concat
        [ parseSuite
        , barSplittingSuite
        , errorSuite
        , integrationSuite
        , toSheetTextSuite
        , applyToProjectPreservingIdsSuite
        ]


sampleSheet : String
sampleSheet =
    """intro
FM7 | G7/F | Em7/F | Am/F |
FM7 | G7/F | Em7/F | Am/F |

A
Fmaj7 | G7/F | Em7 | Am7 |
Fmaj7 | G7/F | Em7 | Am7 |

B1
F | G | G#dim | Am |
F | G | G#dim | Am |

サビ
C | Fmaj7 | G G#dim | Am7 |
Fmaj7 | Em | Dm7 | Dm7/G |
C | Fmaj7 | G G#dim | Am7 |
Fmaj7 | Em | Dm7 | Dm7/G |

ラスト
Am7 | D7 | F/G | C |
Am7 | D7 | F/G | C |

間奏
C | FM7 | G G#dim | Am7 |
FM7 |Em7 Am7 | F/G |

B2
E | F# | Gdim | G#m |
E | F# | Gdim | G#m |

C
EM7 ┃ F#7/E ┃ Ebm7/E ┃ G#m7/E ┃
EM7 ┃ F#7/E ┃ Ebm7/E ┃ G#m7/E ┃

アウトロ
FM7 ┃ G7/F ┃ Em7/F ┃ Am7/F ┃
FM7 ┃ G7/F ┃ Em7/F ┃ Am7/F ┃"""


parseSuite : Test
parseSuite =
    describe "parse"
        [ test "提示サンプルが9ブロックに分かれる" <|
            \_ ->
                case ChordSheet.parse sampleSheet of
                    Ok blocks ->
                        List.length blocks |> Expect.equal 9

                    Err e ->
                        Expect.fail ("parse failed at line " ++ String.fromInt e.line ++ ": " ++ e.message)
        , test "各ブロックのタイトルが正しい" <|
            \_ ->
                case ChordSheet.parse sampleSheet of
                    Ok blocks ->
                        List.map .title blocks
                            |> Expect.equal
                                [ Just "intro"
                                , Just "A"
                                , Just "B1"
                                , Just "サビ"
                                , Just "ラスト"
                                , Just "間奏"
                                , Just "B2"
                                , Just "C"
                                , Just "アウトロ"
                                ]

                    Err e ->
                        Expect.fail ("parse failed at line " ++ String.fromInt e.line ++ ": " ++ e.message)
        , test "各ブロックの小節数が正しい（間奏は4+3=7、サビは4行×4=16）" <|
            \_ ->
                case ChordSheet.parse sampleSheet of
                    Ok blocks ->
                        List.map (\b -> List.length b.bars) blocks
                            |> Expect.equal [ 8, 8, 8, 16, 8, 7, 8, 8, 8 ]

                    Err e ->
                        Expect.fail ("parse failed at line " ++ String.fromInt e.line ++ ": " ++ e.message)
        , test "全角縦棒（┃）も半角 | と同じように小節分割される" <|
            \_ ->
                case ChordSheet.parse sampleSheet of
                    Ok blocks ->
                        List.drop 7 blocks
                            |> List.head
                            |> Maybe.map .bars
                            |> Expect.equal
                                (Just
                                    [ "EM7", "F#7/E", "Ebm7/E", "G#m7/E"
                                    , "EM7", "F#7/E", "Ebm7/E", "G#m7/E"
                                    ]
                                )

                    Err e ->
                        Expect.fail ("parse failed at line " ++ String.fromInt e.line ++ ": " ++ e.message)
        , test "タイトルなしのブロック（最初の行がバー行）" <|
            \_ ->
                ChordSheet.parse "FM7 | G7 |"
                    |> Expect.equal (Ok [ { title = Nothing, bars = [ "FM7", "G7" ], startLine = 1 } ])
        , test "コメントのみの行はブロックを割らない" <|
            \_ ->
                ChordSheet.parse "A\nFM7 | G7 |\n// memo\nAm | Dm |"
                    |> Expect.equal (Ok [ { title = Just "A", bars = [ "FM7", "G7", "Am", "Dm" ], startLine = 1 } ])
        , test "行内コメントは行末まで除去される" <|
            \_ ->
                ChordSheet.parse "FM7 | G7 | // trailing comment"
                    |> Expect.equal (Ok [ { title = Nothing, bars = [ "FM7", "G7" ], startLine = 1 } ])
        , test "連続空行は1つの区切りとして扱われる" <|
            \_ ->
                ChordSheet.parse "A\nFM7 | G7 |\n\n\n\nB\nAm | Dm |"
                    |> Expect.equal
                        (Ok
                            [ { title = Just "A", bars = [ "FM7", "G7" ], startLine = 1 }
                            , { title = Just "B", bars = [ "Am", "Dm" ], startLine = 6 }
                            ]
                        )
        , test "CRLFも LF と同じようにパースできる" <|
            \_ ->
                ChordSheet.parse "A\u{000D}\nFM7 | G7 |\u{000D}\n"
                    |> Expect.equal (Ok [ { title = Just "A", bars = [ "FM7", "G7" ], startLine = 1 } ])
        ]


barSplittingSuite : Test
barSplittingSuite =
    describe "小節の切り出し"
        [ test "行頭・行末の | は両方許容される" <|
            \_ ->
                ChordSheet.parse "| FM7 | G7 |"
                    |> Expect.equal (Ok [ { title = Nothing, bars = [ "FM7", "G7" ], startLine = 1 } ])
        , test "行末の | がなくても同じ結果" <|
            \_ ->
                ChordSheet.parse "FM7 | G7"
                    |> Expect.equal (Ok [ { title = Nothing, bars = [ "FM7", "G7" ], startLine = 1 } ])
        , test "スペース不揃いでもパースできる" <|
            \_ ->
                ChordSheet.parse "FM7 |Em7 Am7 | F/G |"
                    |> Expect.equal (Ok [ { title = Nothing, bars = [ "FM7", "Em7 Am7", "F/G" ], startLine = 1 } ])
        , test "中間の空白チャンクは空小節として保持される" <|
            \_ ->
                ChordSheet.parse "FM7 | | G7"
                    |> Expect.equal (Ok [ { title = Nothing, bars = [ "FM7", "", "G7" ], startLine = 1 } ])
        , test "|| 単独行は空小節1つ" <|
            \_ ->
                ChordSheet.parse "A\n||"
                    |> Expect.equal (Ok [ { title = Just "A", bars = [ "" ], startLine = 1 } ])
        ]


errorSuite : Test
errorSuite =
    describe "エラーケース"
        [ test "空入力は Err" <|
            \_ ->
                ChordSheet.parse ""
                    |> Expect.equal (Err { line = 1, message = "コード譜が空です" })
        , test "空白のみの入力も Err" <|
            \_ ->
                ChordSheet.parse "   \n  \n"
                    |> Expect.equal (Err { line = 1, message = "コード譜が空です" })
        , test "タイトルのみのブロックはタイトル行の行番号で Err" <|
            \_ ->
                ChordSheet.parse "intro\n\nFM7 | G7 |"
                    |> Expect.equal (Err { line = 1, message = "小節（| 区切り）を含まないブロックです" })
        , test "| 単独行（小節0個）は Err" <|
            \_ ->
                ChordSheet.parse "|"
                    |> Expect.equal (Err { line = 1, message = "小節（| 区切り）を含まないブロックです" })
        , test "タイトルなしでバー行の後にテキスト行が来ると Err" <|
            \_ ->
                ChordSheet.parse "FM7 | G7 |\nOops"
                    |> Expect.equal (Err { line = 2, message = "小節区切り | のない行です" })
        , test "タイトルありでバー行の後にテキスト行が来ると Err（行番号一致）" <|
            \_ ->
                ChordSheet.parse "A\nFM7 | G7 |\nOops"
                    |> Expect.equal (Err { line = 3, message = "小節区切り | のない行です" })
        ]


integrationSuite : Test
integrationSuite =
    describe "toSections / toChordText"
        [ test "toChordText の区切り | 総数は全小節数-1に一致し、barCount が一致する" <|
            \_ ->
                case ChordSheet.parse "A\nC | G |\n\nB\nAm | F |" of
                    Ok blocks ->
                        let
                            text =
                                ChordSheet.toChordText blocks

                            track =
                                { text = text, instrument = Piano, muted = False, volume = 100, rhythm = Nothing }
                        in
                        ChordTrack.barCount track |> Expect.equal 4

                    Err e ->
                        Expect.fail ("parse failed: " ++ e.message)
        , test "生成された sections と chordTrack を Timeline に通して小節対応が正しい" <|
            \_ ->
                case ChordSheet.parse "A\nC | G |\n\nB\nAm | F |" of
                    Ok blocks ->
                        let
                            sections =
                                ChordSheet.toSections { firstId = 1, key = Data.Key.default, meter = Data.Meter.default } blocks

                            track =
                                { text = ChordSheet.toChordText blocks, instrument = Piano, muted = False, volume = 100, rhythm = Nothing }

                            timeline =
                                Data.Timeline.fromSections { minBars = 4 } sections

                            tokens =
                                ChordTrack.cells timeline track
                                    |> List.map (\c -> List.map .token c.chords)
                        in
                        tokens |> Expect.equal [ [ "C" ], [ "G" ], [ "Am" ], [ "F" ] ]

                    Err e ->
                        Expect.fail ("parse failed: " ++ e.message)
        ]


toSheetTextSuite : Test
toSheetTextSuite =
    describe "toSheetText"
        [ test "単純ケース" <|
            \_ ->
                let
                    sections =
                        [ { id = 1, name = "Aメロ", lengthBars = 2, memo = "", key = Data.Key.default, meter = Data.Meter.default } ]

                    track =
                        { text = "C | G", instrument = Piano, muted = False, volume = 100, rhythm = Nothing }
                in
                ChordSheet.toSheetText sections track |> Expect.equal "Aメロ\nC | G |"
        , test "パディングされた空小節を含むセクション（デモ相当）" <|
            \_ ->
                let
                    sections =
                        [ { id = 2, name = "Aメロ", lengthBars = 4, memo = "", key = Data.Key.default, meter = Data.Meter.default }
                        , { id = 3, name = "サビ", lengthBars = 4, memo = "", key = Data.Key.default, meter = Data.Meter.default }
                        ]

                    track =
                        { text = "C | G | Am | F", instrument = Piano, muted = False, volume = 100, rhythm = Nothing }
                in
                ChordSheet.toSheetText sections track
                    |> Expect.equal "Aメロ\nC | G | Am | F |\n\nサビ\n|  |  |  |  |"
        , test "往復プロパティ：パディングケースの出力を再パースすると空小節も含めて復元する" <|
            \_ ->
                let
                    sections =
                        [ { id = 2, name = "Aメロ", lengthBars = 4, memo = "", key = Data.Key.default, meter = Data.Meter.default }
                        , { id = 3, name = "サビ", lengthBars = 4, memo = "", key = Data.Key.default, meter = Data.Meter.default }
                        ]

                    track =
                        { text = "C | G | Am | F", instrument = Piano, muted = False, volume = 100, rhythm = Nothing }

                    sheetText =
                        ChordSheet.toSheetText sections track
                in
                case ChordSheet.parse sheetText of
                    Ok blocks ->
                        Expect.all
                            [ \bs -> List.map .title bs |> Expect.equal [ Just "Aメロ", Just "サビ" ]
                            , \bs -> List.map .bars bs |> Expect.equal [ [ "C", "G", "Am", "F" ], [ "", "", "", "" ] ]
                            ]
                            blocks

                    Err e ->
                        Expect.fail ("parse failed at line " ++ String.fromInt e.line ++ ": " ++ e.message)
        , test "先頭小節が空のケースの往復" <|
            \_ ->
                let
                    sections =
                        [ { id = 1, name = "X", lengthBars = 3, memo = "", key = Data.Key.default, meter = Data.Meter.default } ]

                    track =
                        { text = "| C |", instrument = Piano, muted = False, volume = 100, rhythm = Nothing }

                    sheetText =
                        ChordSheet.toSheetText sections track
                in
                case ChordSheet.parse sheetText of
                    Ok blocks ->
                        List.map .bars blocks |> Expect.equal [ [ "", "C", "" ] ]

                    Err e ->
                        Expect.fail ("parse failed at line " ++ String.fromInt e.line ++ ": " ++ e.message)
        , test "セクション合計を超えるオーバーフロー小節は出力に含まれない" <|
            \_ ->
                let
                    sections =
                        [ { id = 1, name = "Y", lengthBars = 2, memo = "", key = Data.Key.default, meter = Data.Meter.default } ]

                    track =
                        { text = "C | G | Am", instrument = Piano, muted = False, volume = 100, rhythm = Nothing }
                in
                ChordSheet.toSheetText sections track |> Expect.equal "Y\nC | G |"
        ]


applyToProjectPreservingIdsSuite : Test
applyToProjectPreservingIdsSuite =
    describe "applyToProjectPreservingIds"
        [ test "同数ブロック：ID/memo/key/meterを維持し、nextIdは不変" <|
            \_ ->
                let
                    before =
                        let
                            demo =
                                Data.Project.demo
                        in
                        { demo
                            | sections =
                                [ { id = 2, name = "Aメロ", lengthBars = 4, memo = "original", key = Data.Key.default, meter = Data.Meter.default }
                                , { id = 3, name = "サビ", lengthBars = 4, memo = "", key = Data.Key.default, meter = Data.Meter.default }
                                ]
                        }
                in
                case ChordSheet.parse "イントロ\nDm | Em |\n\n落ちサビ\nF | G | A7 | B7 |" of
                    Ok blocks ->
                        let
                            after =
                                ChordSheet.applyToProjectPreservingIds blocks before
                        in
                        Expect.all
                            [ \p -> List.map .id p.sections |> Expect.equal [ 2, 3 ]
                            , \p -> List.map .name p.sections |> Expect.equal [ "イントロ", "落ちサビ" ]
                            , \p -> List.map .lengthBars p.sections |> Expect.equal [ 2, 4 ]
                            , \p -> List.map .memo p.sections |> Expect.equal [ "original", "" ]
                            , \p -> p.nextId |> Expect.equal before.nextId
                            , \p -> p.tracks |> Expect.equal before.tracks
                            , \p -> p.chordTrack.text |> Expect.equal (ChordSheet.toChordText blocks)
                            ]
                            after

                    Err e ->
                        Expect.fail ("parse failed: " ++ e.message)
        , test "ブロック増加：超過分に新規ID、nextIdが進む" <|
            \_ ->
                let
                    before =
                        Data.Project.demo
                in
                case ChordSheet.parse "イントロ\nDm | Em |\n\n落ちサビ\nF | G | A7 | B7 |\n\nアウトロ\nC | C |" of
                    Ok blocks ->
                        let
                            after =
                                ChordSheet.applyToProjectPreservingIds blocks before
                        in
                        Expect.all
                            [ \p -> List.map .id p.sections |> Expect.equal [ 2, 3, before.nextId ]
                            , \p -> p.nextId |> Expect.equal (before.nextId + 1)
                            ]
                            after

                    Err e ->
                        Expect.fail ("parse failed: " ++ e.message)
        , test "ブロック減少：末尾セクションが消える" <|
            \_ ->
                let
                    before =
                        Data.Project.demo
                in
                case ChordSheet.parse "イントロ\nDm | Em |" of
                    Ok blocks ->
                        let
                            after =
                                ChordSheet.applyToProjectPreservingIds blocks before
                        in
                        Expect.all
                            [ \p -> List.map .id p.sections |> Expect.equal [ 2 ]
                            , \p -> p.nextId |> Expect.equal before.nextId
                            ]
                            after

                    Err e ->
                        Expect.fail ("parse failed: " ++ e.message)
        , test "無題ブロックは既存名にフォールバックせず「セクション N」になる" <|
            \_ ->
                let
                    before =
                        Data.Project.demo
                in
                case ChordSheet.parse "C | G |" of
                    Ok blocks ->
                        ChordSheet.applyToProjectPreservingIds blocks before
                            |> .sections
                            |> List.map .name
                            |> Expect.equal [ "セクション 1" ]

                    Err e ->
                        Expect.fail ("parse failed: " ++ e.message)
        ]
