module Data.Note exposing (Note, pitchLabel)

import Data.Chord.Format as Format
import Data.Time exposing (Ticks)


type alias Note =
    { id : Int
    , pitch : Int
    , start : Ticks
    , duration : Ticks
    , velocity : Int
    }


{-| MIDIピッチ番号を「E 2」のような音名＋オクターブ番号の表記に変換する。ノートホバーツールチップで使う。
keyRow（View/PianoRoll.elm）と同じ pitch // 12 - 1 のオクターブ換算を使い、音名部分は Data.Chord.Format.pitchName に委ねる。
-}
pitchLabel : Int -> String
pitchLabel pitch =
    Format.pitchName False (modBy 12 pitch) ++ " " ++ String.fromInt (pitch // 12 - 1)
