module View.Zoom exposing (step)


{-| ホイールズームの1ステップ。deltaY が負（手前に回す）ならズームイン、
正（向こうに回す）ならズームアウト。結果は limits にクランプする。
`View.PianoRoll.zoomStep` と `View.SectionBar.regionZoomStep` の共通実装。
-}
step : { min : Int, max : Int } -> Float -> Int -> Int
step limits deltaY current =
    let
        factor =
            if deltaY < 0 then
                1.2

            else if deltaY > 0 then
                1 / 1.2

            else
                1
    in
    clamp limits.min limits.max (round (toFloat current * factor))
