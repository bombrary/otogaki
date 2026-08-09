module Data.Time exposing (Ticks, formatDuration, ppq, ticksPerBar, ticksPerSixteenth)


type alias Ticks =
    Int


ppq : Int
ppq =
    480


ticksPerBar : Int
ticksPerBar =
    ppq * 4


ticksPerSixteenth : Int
ticksPerSixteenth =
    ppq // 4


{-| tick数を「小節.拍.16分音符.tick」の4成分表記に分解する（4/4固定のグリッドを前提）。ノートホバーツールチップの長さ表示で使う。
-}
formatDuration : Int -> String
formatDuration d =
    let
        bars =
            d // ticksPerBar

        beats =
            modBy ticksPerBar d // ppq

        sixteenths =
            modBy ppq d // ticksPerSixteenth

        ticks =
            modBy ticksPerSixteenth d
    in
    [ bars, beats, sixteenths, ticks ]
        |> List.map String.fromInt
        |> String.join "."
