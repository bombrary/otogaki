module MetronomeTest exposing (suite)

import Codec.Performance as Performance
import Data.Key as Key
import Data.Meter as Meter
import Data.Timeline as Timeline
import Expect
import Test exposing (Test, describe, test)


section : Int -> Int -> Meter.Meter -> { id : Int, name : String, lengthBars : Int, memo : String, key : Key.Key, meter : Meter.Meter }
section id lengthBars meter =
    { id = id, name = "s" ++ String.fromInt id, lengthBars = lengthBars, memo = "", key = Key.default, meter = meter }


suite : Test
suite =
    describe "Codec.Performance.metronomeEvents"
        [ test "4/4 単一セクションでは1小節で4クリック、拍1は cowbell、拍2-4は rimshot" <|
            \_ ->
                let
                    tl =
                        Timeline.fromSections { minBars = 0 } [ section 1 1 Meter.default ]

                    events =
                        Performance.metronomeEvents tl

                    beatTicks =
                        Meter.ticksPerBeat Meter.default

                    clickDuration =
                        Meter.ticksPerBeat Meter.default // 4 |> max 1
                in
                Expect.equal
                    [ { ticks = 0, durationTicks = clickDuration, pitch = 56, velocity = 115, instrument = "drumKit", trackId = -2 }
                    , { ticks = beatTicks, durationTicks = clickDuration, pitch = 37, velocity = 95, instrument = "drumKit", trackId = -2 }
                    , { ticks = beatTicks * 2, durationTicks = clickDuration, pitch = 37, velocity = 95, instrument = "drumKit", trackId = -2 }
                    , { ticks = beatTicks * 3, durationTicks = clickDuration, pitch = 37, velocity = 95, instrument = "drumKit", trackId = -2 }
                    ]
                    events
        , test "拍子変化（4/4 → 3/4）をまたいでクリック数と小節頭位置が正しい" <|
            \_ ->
                let
                    threeFour =
                        { numerator = 3, denominator = 4 }

                    tl =
                        Timeline.fromSections { minBars = 0 }
                            [ section 1 1 Meter.default
                            , section 2 1 threeFour
                            ]

                    events =
                        Performance.metronomeEvents tl

                    bar1Ticks =
                        Meter.ticksPerBar Meter.default

                    downbeats =
                        events |> List.filter (\e -> e.pitch == 56) |> List.map .ticks
                in
                Expect.all
                    [ \_ -> Expect.equal 7 (List.length events)
                    , \_ -> Expect.equal [ 0, bar1Ticks ] downbeats
                    ]
                    ()
        , test "6/8 は1小節で6打、間隔は八分音符" <|
            \_ ->
                let
                    sixEight =
                        { numerator = 6, denominator = 8 }

                    tl =
                        Timeline.fromSections { minBars = 0 } [ section 1 1 sixEight ]

                    events =
                        Performance.metronomeEvents tl

                    beatTicks =
                        Meter.ticksPerBeat sixEight
                in
                Expect.all
                    [ \_ -> Expect.equal 6 (List.length events)
                    , \_ -> Expect.equal (List.map (\i -> i * beatTicks) (List.range 0 5)) (List.map .ticks events)
                    ]
                    ()
        , test "全イベントが metronomeTrackId かつ drumKit" <|
            \_ ->
                let
                    tl =
                        Timeline.fromSections { minBars = 0 }
                            [ section 1 2 Meter.default
                            , section 2 2 { numerator = 6, denominator = 8 }
                            ]

                    events =
                        Performance.metronomeEvents tl
                in
                Expect.all
                    [ \_ -> Expect.equal True (List.all (\e -> e.trackId == -2) events)
                    , \_ -> Expect.equal True (List.all (\e -> e.instrument == "drumKit") events)
                    ]
                    ()
        ]
