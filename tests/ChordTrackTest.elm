module ChordTrackTest exposing (suite)

import Data.ChordTrack as ChordTrack
import Data.Time
import Data.Track exposing (Instrument(..))
import Expect
import Test exposing (Test, describe, test)


track : String -> ChordTrack.ChordTrack
track text =
    { text = text, instrument = Piano, muted = False, volume = 100 }


suite : Test
suite =
    describe "コードトラック展開"
        [ test "% は直前のコードを繰り返す" <|
            \_ ->
                ChordTrack.resolved (track "C | %")
                    |> List.map .startTicks
                    |> Expect.equal [ 0, Data.Time.ticksPerBar ]
        , test "= は直前のコードを伸ばす" <|
            \_ ->
                case ChordTrack.resolved (track "C | =") of
                    [ ev ] ->
                        Expect.equal (Data.Time.ticksPerBar * 2) ev.durationTicks

                    _ ->
                        Expect.fail "イベント数が想定外"
        , test "_ は休符" <|
            \_ ->
                ChordTrack.resolved (track "C | _ | C")
                    |> List.length
                    |> Expect.equal 2
        , test "休符の後の % は休符前のコードを繰り返す" <|
            \_ ->
                ChordTrack.resolved (track "C | _ | %")
                    |> List.map .startTicks
                    |> Expect.equal [ 0, Data.Time.ticksPerBar * 2 ]
        , test "改行は無視される（小節区切りは | だけ）" <|
            \_ ->
                ChordTrack.resolved (track "C |\nG")
                    |> List.map .startTicks
                    |> Expect.equal [ 0, 1920 ]
        , test "改行をまたぐ小節は空白と同じ扱い" <|
            \_ ->
                ChordTrack.resolved (track "C\nG")
                    |> List.map .startTicks
                    |> Expect.equal [ 0, 960 ]
        , test "小節内分割と = の組合せ" <|
            \_ ->
                case ChordTrack.resolved (track "C =") of
                    [ ev ] ->
                        Expect.equal Data.Time.ticksPerBar ev.durationTicks

                    _ ->
                        Expect.fail "イベント数が想定外"
        ]
