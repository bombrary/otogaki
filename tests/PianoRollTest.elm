module PianoRollTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import View.PianoRoll as PianoRoll


suite : Test
suite =
    describe "View.PianoRoll"
        [ test "pixelsToTicks と ticksToPixels は defaultPxPerSixteenth で往復する" <|
            \_ ->
                let
                    px =
                        PianoRoll.defaultPxPerSixteenth * 3
                in
                PianoRoll.pixelsToTicks PianoRoll.defaultPxPerSixteenth (toFloat px)
                    |> PianoRoll.ticksToPixels PianoRoll.defaultPxPerSixteenth
                    |> Expect.within (Expect.Absolute 0.001) (toFloat px)
        , test "pixelsToTicks と ticksToPixels は拡大したスケールでも往復する" <|
            \_ ->
                let
                    zoom =
                        40

                    px =
                        zoom * 5
                in
                PianoRoll.pixelsToTicks zoom (toFloat px)
                    |> PianoRoll.ticksToPixels zoom
                    |> Expect.within (Expect.Absolute 0.001) (toFloat px)
        , test "zoomStep は deltaY が負ならズームイン（値が大きくなる）" <|
            \_ ->
                PianoRoll.zoomStep -1 PianoRoll.defaultPxPerSixteenth
                    |> Expect.greaterThan PianoRoll.defaultPxPerSixteenth
        , test "zoomStep は deltaY が正ならズームアウト（値が小さくなる）" <|
            \_ ->
                PianoRoll.zoomStep 1 PianoRoll.defaultPxPerSixteenth
                    |> (\next -> next < PianoRoll.defaultPxPerSixteenth)
                    |> Expect.equal True
        , test "zoomStep は maxPxPerSixteenth を超えてズームインしない" <|
            \_ ->
                PianoRoll.zoomStep -1 PianoRoll.maxPxPerSixteenth
                    |> Expect.equal PianoRoll.maxPxPerSixteenth
        , test "zoomStep は minPxPerSixteenth を下回ってズームアウトしない" <|
            \_ ->
                PianoRoll.zoomStep 1 PianoRoll.minPxPerSixteenth
                    |> Expect.equal PianoRoll.minPxPerSixteenth
        , test "visibleTickRange は scrollX=0 で先頭から始まる可視範囲を返す" <|
            \_ ->
                PianoRoll.visibleTickRange 20 { scrollX = 0, width = 200 }
                    |> Expect.equal { startTicks = 0, endTicks = 1200 }
        , test "visibleTickRange は scrollX 分だけ開始 ticks が進む" <|
            \_ ->
                PianoRoll.visibleTickRange 20 { scrollX = 320, width = 200 }
                    |> .startTicks
                    |> Expect.equal 1920
        , test "visibleTickRange は ticksToPixels と整合する" <|
            \_ ->
                let
                    zoom =
                        20

                    t =
                        960

                    scrollX =
                        PianoRoll.ticksToPixels zoom t
                in
                PianoRoll.visibleTickRange zoom { scrollX = scrollX, width = 200 }
                    |> .startTicks
                    |> Expect.equal t
        , test "visibleTickRange はズームが2倍になると同一ピクセル窓での範囲ticks幅が半分になる" <|
            \_ ->
                let
                    rangeWidth zoom =
                        let
                            r =
                                PianoRoll.visibleTickRange zoom { scrollX = 0, width = 200 }
                        in
                        r.endTicks - r.startTicks
                in
                rangeWidth 40
                    |> Expect.equal (rangeWidth 20 // 2)
        , test "pixelsToTicks と ticksToPixels は三連八分音符（160 tick）でも往復する" <|
            \_ ->
                let
                    zoom =
                        30

                    ticks =
                        160
                in
                PianoRoll.ticksToPixels zoom ticks
                    |> PianoRoll.pixelsToTicks zoom
                    |> Expect.equal ticks
        , test "pixelsToTicks と ticksToPixels は三連四分音符（320 tick）でも往復する" <|
            \_ ->
                let
                    zoom =
                        30

                    ticks =
                        320
                in
                PianoRoll.ticksToPixels zoom ticks
                    |> PianoRoll.pixelsToTicks zoom
                    |> Expect.equal ticks
        ]
