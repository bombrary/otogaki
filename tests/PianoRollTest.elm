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
        , test "splitTickFromOffset はオフセット0ならノート開始位置をそのまま返す" <|
            \_ ->
                PianoRoll.splitTickFromOffset PianoRoll.defaultPxPerSixteenth 0 480
                    |> Expect.equal 480
        , test "splitTickFromOffset はオフセットをtick換算してノート開始位置に足す" <|
            \_ ->
                let
                    px =
                        PianoRoll.defaultPxPerSixteenth * 2
                in
                PianoRoll.splitTickFromOffset PianoRoll.defaultPxPerSixteenth (toFloat px) 480
                    |> Expect.equal (480 + PianoRoll.pixelsToTicks PianoRoll.defaultPxPerSixteenth (toFloat px))
        , test "splitTickFromOffset は拡大したズームでも同じ式で成り立つ" <|
            \_ ->
                let
                    zoom =
                        40

                    px =
                        zoom * 3
                in
                PianoRoll.splitTickFromOffset zoom (toFloat px) 960
                    |> Expect.equal (960 + PianoRoll.pixelsToTicks zoom (toFloat px))
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
        , test "medianPitch は空リストで C4(60) を返す" <|
            \_ ->
                PianoRoll.medianPitch []
                    |> Expect.equal 60
        , test "medianPitch は奇数個で中央値を返す（未ソート入力でも）" <|
            \_ ->
                PianoRoll.medianPitch [ 48, 72, 60 ]
                    |> Expect.equal 60
        , test "medianPitch は偶数個で n // 2 番目（上側の中央値）を返す" <|
            \_ ->
                PianoRoll.medianPitch [ 60, 61, 62, 63 ]
                    |> Expect.equal 62
        , test "medianPitch は minPitch より低い値を minPitch にクランプする" <|
            \_ ->
                PianoRoll.medianPitch [ -100 ]
                    |> Expect.equal PianoRoll.minPitch
        , test "medianPitch は maxPitch より高い値を maxPitch にクランプする" <|
            \_ ->
                PianoRoll.medianPitch [ 300 ]
                    |> Expect.equal PianoRoll.maxPitch
        , test "medianPitch は重複を含むリストでも壊れない" <|
            \_ ->
                PianoRoll.medianPitch [ 60, 60, 60 ]
                    |> Expect.equal 60
        ]
