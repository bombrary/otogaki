module ChordTrackTest exposing (suite)

import Data.ChordTrack as ChordTrack
import Data.Key
import Data.Meter
import Data.Section exposing (Section)
import Data.Timeline
import Data.Track exposing (Instrument(..))
import Expect
import Test exposing (Test, describe, test)


track : String -> ChordTrack.ChordTrack
track text =
    { text = text, instrument = Piano, muted = False, volume = 100 }


{-| 4/4 固定の十分長いセクションで作った timeline。既存テストはこれを通して resolved を呼ぶ。
-}
timeline : Data.Timeline.Timeline
timeline =
    Data.Timeline.fromSections { minBars = 64 }
        [ { id = 1, name = "s", lengthBars = 64, memo = "", key = Data.Key.default, meter = Data.Meter.default } ]


{-| 2小節ずつの 3 セクションを持つ timeline。namedSpans/sectionSummaries のセクション跨ぎテストで使う。
-}
multiSectionTimeline : Data.Timeline.Timeline
multiSectionTimeline =
    Data.Timeline.fromSections { minBars = 6 }
        [ { id = 1, name = "A", lengthBars = 2, memo = "", key = Data.Key.default, meter = Data.Meter.default }
        , { id = 2, name = "B", lengthBars = 2, memo = "", key = Data.Key.default, meter = Data.Meter.default }
        , { id = 3, name = "C", lengthBars = 2, memo = "", key = Data.Key.default, meter = Data.Meter.default }
        ]


resolved : ChordTrack.ChordTrack -> List ChordTrack.ResolvedChord
resolved =
    ChordTrack.resolved timeline


suite : Test
suite =
    describe "コードトラック展開"
        [ test "% は直前のコードを繰り返す" <|
            \_ ->
                resolved (track "C | %")
                    |> List.map .startTicks
                    |> Expect.equal [ 0, Data.Meter.ticksPerBar Data.Meter.default ]
        , test "= は直前のコードを伸ばす" <|
            \_ ->
                case resolved (track "C | =") of
                    [ ev ] ->
                        Expect.equal (Data.Meter.ticksPerBar Data.Meter.default * 2) ev.durationTicks

                    _ ->
                        Expect.fail "イベント数が想定外"
        , test "_ は休符" <|
            \_ ->
                resolved (track "C | _ | C")
                    |> List.length
                    |> Expect.equal 2
        , test "休符の後の % は休符前のコードを繰り返す" <|
            \_ ->
                resolved (track "C | _ | %")
                    |> List.map .startTicks
                    |> Expect.equal [ 0, Data.Meter.ticksPerBar Data.Meter.default * 2 ]
        , test "改行は無視される（小節区切りは | だけ）" <|
            \_ ->
                resolved (track "C |\nG")
                    |> List.map .startTicks
                    |> Expect.equal [ 0, Data.Meter.ticksPerBar Data.Meter.default ]
        , test "改行をまたぐ小節は空白と同じ扱い" <|
            \_ ->
                resolved (track "C\nG")
                    |> List.map .startTicks
                    |> Expect.equal [ 0, Data.Meter.ticksPerBar Data.Meter.default // 2 ]
        , test "小節内分割と = の組合せ" <|
            \_ ->
                case resolved (track "C =") of
                    [ ev ] ->
                        Expect.equal (Data.Meter.ticksPerBar Data.Meter.default) ev.durationTicks

                    _ ->
                        Expect.fail "イベント数が想定外"
        , test "// 以降は行末までコメントとして無視される" <|
            \_ ->
                resolved (track "C | G  // ここからBメロ\n%")
                    |> List.length
                    |> Expect.equal 3
        , test "コメントの中の | は小節区切りとして数えられない" <|
            \_ ->
                ChordTrack.barCount (track "C | G  // 1|2|3 とか書くかも\n| Am")
                    |> Expect.equal 3
        , test "コメントだけの行は空小節扱い" <|
            \_ ->
                resolved (track "C | // この小節はまだ未定\n| G")
                    |> List.map .startTicks
                    |> Expect.equal [ 0, Data.Meter.ticksPerBar Data.Meter.default * 2 ]
        , test "stripComments は // 以降だけを取り除く" <|
            \_ ->
                ChordTrack.stripComments "C | G  // メモ\nAm"
                    |> Expect.equal "C | G  \nAm"
        , test "transpose はコメントの中身を変えない" <|
            \_ ->
                (ChordTrack.transpose 2 (track "C  // C7 が好き")).text
                    |> Expect.equal "D  // C7 が好き"
        , test "transposeBars は指定範囲の小節だけを移調し、範囲外（前/後）は変わらない" <|
            \_ ->
                (ChordTrack.transposeBars 1 2 2 (track "C | D | E | F")).text
                    |> Expect.equal "C | E | F# | F"
        , test "transposeBars は対象範囲内のコメントを保ったまま移調する" <|
            \_ ->
                (ChordTrack.transposeBars 0 1 2 (track "C // note\n| D")).text
                    |> Expect.equal "D // note\n| D"
        , test "namedSpans は % と = を解決したコード名を返す" <|
            \_ ->
                ChordTrack.namedSpans timeline (track "C | G7 | % | =")
                    |> List.map .name
                    |> Expect.equal [ "C", "G7", "G7" ]
        , test "namedSpans は = で伸ばされたスパンを 1 つにまとめる" <|
            \_ ->
                ChordTrack.namedSpans timeline (track "C | G7 | % | =")
                    |> List.length
                    |> Expect.equal 3
        , test "namedSpans は単一セクションなら全て sectionIndex 0" <|
            \_ ->
                ChordTrack.namedSpans timeline (track "C | G | Am | F")
                    |> List.map .sectionIndex
                    |> Expect.equal [ Just 0, Just 0, Just 0, Just 0 ]
        , test "namedSpans はセクションをまたぐと sectionIndex が切り替わる" <|
            \_ ->
                ChordTrack.namedSpans multiSectionTimeline (track "C | C | G | Am")
                    |> List.map .sectionIndex
                    |> Expect.equal [ Just 0, Just 0, Just 1, Just 1 ]
        ]
