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
        ]
