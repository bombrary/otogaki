module Data.Time exposing (Ticks, ppq, ticksPerBar, ticksPerSixteenth)


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
