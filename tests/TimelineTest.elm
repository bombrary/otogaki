module TimelineTest exposing (suite)

import Data.Key as Key
import Data.Meter as Meter
import Data.Timeline as Timeline
import Expect
import Test exposing (Test, describe, test)


section : Int -> Int -> Meter.Meter -> Key.Key -> { id : Int, name : String, lengthBars : Int, memo : String, key : Key.Key, meter : Meter.Meter }
section id lengthBars meter key =
    { id = id, name = "s" ++ String.fromInt id, lengthBars = lengthBars, memo = "", key = key, meter = meter }


suite : Test
suite =
    describe "Data.Timeline"
        [ test "セクションがなければ minBars 分だけ 4/4 ・C major で埋める" <|
            \_ ->
                let
                    tl =
                        Timeline.fromSections { minBars = 4 } []
                in
                Expect.equal 4 (Timeline.totalBars tl)
        , test "小節の累積が拍子ごとに正しい（4/4 と 3/4 の混在）" <|
            \_ ->
                let
                    tl =
                        Timeline.fromSections { minBars = 0 }
                            [ section 1 2 Meter.default Key.default
                            , section 2 2 { numerator = 3, denominator = 4 } Key.default
                            ]

                    bar2 =
                        Timeline.barAt 2 tl
                in
                Expect.equal (Just (Meter.ticksPerBar Meter.default * 2)) (Maybe.map .startTicks bar2)
        , test "sectionAt は小節内の tick 位置を正しいセクション id に解決する" <|
            \_ ->
                let
                    tl =
                        Timeline.fromSections { minBars = 0 }
                            [ section 10 4 Meter.default Key.default
                            , section 20 4 Meter.default Key.default
                            ]
                in
                Expect.equal (Just 20) (Timeline.sectionAt (Meter.ticksPerBar Meter.default * 5) tl)
        , test "sectionIndexAt は id ではなくリスト内インデックスを返す" <|
            \_ ->
                let
                    tl =
                        Timeline.fromSections { minBars = 0 }
                            [ section 100 4 Meter.default Key.default
                            , section 5 4 Meter.default Key.default
                            , section 42 4 Meter.default Key.default
                            ]
                in
                Expect.all
                    [ \_ -> Expect.equal (Just 0) (Timeline.sectionIndexAt (Meter.ticksPerBar Meter.default * 1) tl)
                    , \_ -> Expect.equal (Just 1) (Timeline.sectionIndexAt (Meter.ticksPerBar Meter.default * 5) tl)
                    , \_ -> Expect.equal (Just 2) (Timeline.sectionIndexAt (Meter.ticksPerBar Meter.default * 9) tl)
                    ]
                    ()
        , test "sectionIndexAt は削除・並べ替え後も新しいリスト順で振り直される" <|
            \_ ->
                let
                    -- 最初のセクション（id=100）を削除して入れ替えた後の順番
                    tl =
                        Timeline.fromSections { minBars = 0 }
                            [ section 5 4 Meter.default Key.default
                            , section 42 4 Meter.default Key.default
                            ]
                in
                Expect.all
                    [ \_ -> Expect.equal (Just 0) (Timeline.sectionIndexAt (Meter.ticksPerBar Meter.default * 1) tl)
                    , \_ -> Expect.equal (Just 1) (Timeline.sectionIndexAt (Meter.ticksPerBar Meter.default * 5) tl)
                    ]
                    ()
        , test "sectionBounds は小節数に応じた tick 範囲を返す" <|
            \_ ->
                let
                    tl =
                        Timeline.fromSections { minBars = 0 }
                            [ section 10 4 Meter.default Key.default
                            , section 20 3 { numerator = 3, denominator = 4 } Key.default
                            ]

                    bounds =
                        Timeline.sectionBounds 20 tl
                in
                Expect.equal
                    (Just
                        { startTicks = Meter.ticksPerBar Meter.default * 4
                        , endTicks = Meter.ticksPerBar Meter.default * 4 + Meter.ticksPerBar { numerator = 3, denominator = 4 } * 3
                        }
                    )
                    bounds
        , test "keyAt はセクションごとの転調を反映する" <|
            \_ ->
                let
                    keyD =
                        { tonic = 2, mode = Key.Major }

                    tl =
                        Timeline.fromSections { minBars = 0 }
                            [ section 1 4 Meter.default Key.default
                            , section 2 4 Meter.default keyD
                            ]
                in
                Expect.equal keyD (Timeline.keyAt (Meter.ticksPerBar Meter.default * 5) tl)
        , test "ticksToBarBeat は 1-based の小節・拍を返す" <|
            \_ ->
                let
                    tl =
                        Timeline.fromSections { minBars = 4 } []

                    result =
                        Timeline.ticksToBarBeat (Meter.ticksPerBar Meter.default + Meter.ticksPerBeat Meter.default) tl
                in
                Expect.equal { bar = 2, beat = 2 } result
        , test "ticksToFractionalBar は小節先頭で整数値を返す" <|
            \_ ->
                let
                    tl =
                        Timeline.fromSections { minBars = 4 } []
                in
                Expect.within (Expect.Absolute 0.0001) 2.0 (Timeline.ticksToFractionalBar (Meter.ticksPerBar Meter.default * 2) tl)
        , test "ticksToFractionalBar は小節の中間で 0.5 を返す" <|
            \_ ->
                let
                    tl =
                        Timeline.fromSections { minBars = 4 } []
                in
                Expect.within (Expect.Absolute 0.0001) 1.5 (Timeline.ticksToFractionalBar (Meter.ticksPerBar Meter.default + Meter.ticksPerBar Meter.default // 2) tl)
        , test "fractionalBarToTicks は ticksToFractionalBar の逆変換になる（小節先頭）" <|
            \_ ->
                let
                    tl =
                        Timeline.fromSections { minBars = 4 } []

                    ticks =
                        Meter.ticksPerBar Meter.default * 3
                in
                Expect.equal ticks (Timeline.fractionalBarToTicks (Timeline.ticksToFractionalBar ticks tl) tl)
        , test "fractionalBarToTicks は負の入力を 0 にクランプする（小節0より左に振っても末尾にワープしない）" <|
            \_ ->
                let
                    tl =
                        Timeline.fromSections { minBars = 4 } []
                in
                Expect.all
                    [ \_ -> Expect.equal 0 (Timeline.fractionalBarToTicks -0.3 tl)
                    , \_ -> Expect.equal 0 (Timeline.fractionalBarToTicks -5 tl)
                    ]
                    ()
        , test "拍子が変わるセクションをまたいでも正しい小数点位置を返す" <|
            \_ ->
                let
                    tl =
                        Timeline.fromSections { minBars = 0 }
                            [ section 1 2 Meter.default Key.default
                            , section 2 2 { numerator = 3, denominator = 4 } Key.default
                            ]

                    bar2Start =
                        Meter.ticksPerBar Meter.default * 2

                    threeQuarterBarTicks =
                        Meter.ticksPerBar { numerator = 3, denominator = 4 }
                in
                Expect.within (Expect.Absolute 0.0001) 2.5 (Timeline.ticksToFractionalBar (bar2Start + threeQuarterBarTicks // 2) tl)
        , test "fractionalBarToTicks は拍子が変わるセクション内でも正しい tick を返す" <|
            \_ ->
                let
                    tl =
                        Timeline.fromSections { minBars = 0 }
                            [ section 1 2 Meter.default Key.default
                            , section 2 2 { numerator = 3, denominator = 4 } Key.default
                            ]

                    bar2Start =
                        Meter.ticksPerBar Meter.default * 2

                    threeQuarterBarTicks =
                        Meter.ticksPerBar { numerator = 3, denominator = 4 }
                in
                Expect.equal (bar2Start + threeQuarterBarTicks // 2) (Timeline.fractionalBarToTicks 2.5 tl)
        ]
