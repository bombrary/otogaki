module View.Zoom exposing (step)


{-| ホイールズームの1ステップ。deltaY が負（手前に回す）ならズームイン、
正（向こうに回す）ならズームアウト。結果は limits にクランプする。
`View.PianoRoll.zoomStep` と `View.SectionBar.regionZoomStep` の共通実装。

deltaY の大きさを連続的に反映する（ホイール1ノッチ≈100 で従来同等の1.2倍、トラックパッドの微小な delta ではより緩やかに動く）。
方向ごとに ceiling/floor を使い分けて、どんなに微小な deltaY でも必ず1以上動くことを保証する（round だと量子化されて小さな zoom 値で動かなくなるため）。
deltaY は ±100 にクランプし、数百以上の deltaY を送ってくる環境での1イベント大ジャンプを抑える。
-}
step : { min : Int, max : Int } -> Float -> Int -> Int
step limits deltaY current =
    if deltaY == 0 then
        clamp limits.min limits.max current

    else
        let
            dy =
                clamp -100 100 deltaY

            factor =
                1.2 ^ (-dy / 100)

            target =
                toFloat current * factor

            next =
                if deltaY < 0 then
                    ceiling target

                else
                    floor target
        in
        clamp limits.min limits.max next
