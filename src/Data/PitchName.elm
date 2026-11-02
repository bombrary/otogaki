module Data.PitchName exposing (OctaveConvention(..), default, fromString, label, octave, toString)

{-| オクターブ番号付き音名の表記規約。

MIDI 番号や音は一切変えず、表示だけを切り替える。
`C4Is60` は国際式（MIDI 60 = C4）、`C3Is60` は Studio One / Cubase / Yamaha 式（MIDI 60 = C3）。
オクターブ番号の計算はこのモジュールに集約する（他所で octave を直接計算しない）。

-}

import Data.Chord.Format as Format


type OctaveConvention
    = C4Is60
    | C3Is60


{-| 既定は Studio One 式。ドラフトの持ち込み先に合わせている。
-}
default : OctaveConvention
default =
    C3Is60


octave : OctaveConvention -> Int -> Int
octave convention pitch =
    case convention of
        C4Is60 ->
            pitch // 12 - 1

        C3Is60 ->
            pitch // 12 - 2


{-| 「C#4」のような音名＋オクターブ。
-}
label : OctaveConvention -> Int -> String
label convention pitch =
    Format.pitchName False (modBy 12 pitch) ++ String.fromInt (octave convention pitch)


toString : OctaveConvention -> String
toString convention =
    case convention of
        C4Is60 ->
            "c4"

        C3Is60 ->
            "c3"


fromString : String -> OctaveConvention
fromString s =
    case s of
        "c4" ->
            C4Is60

        "c3" ->
            C3Is60

        _ ->
            default
