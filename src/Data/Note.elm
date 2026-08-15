module Data.Note exposing (Note, mergeAdjacent, pitchLabel, splitAt)

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


{-| 指定 tick でノートを前半・後半に分割する純関数。tick が note.start〜note.start+duration の開区間に完全に含まれない（境界一致も含む）場合は
 Nothing。後半はこの時点では元の note と同じ id を持つまま返す（新規 id の発行は呼び出し側の責任。Data.Project.cutNotesAt が行う）。
-}
splitAt : Ticks -> Note -> Maybe ( Note, Note )
splitAt tick note =
    if tick > note.start && tick < note.start + note.duration then
        Just
            ( { note | duration = tick - note.start }
            , { note | start = tick, duration = note.start + note.duration - tick }
            )

    else
        Nothing


{-| 隣接する2つのノートを結合する。pitch が同じかつ a の終了 tick と b の開始 tick が完全一致する時のみ Just。
結合後の id/velocity は a（前半）のものを引き継ぐ（splitAt が後半に元idを残すのと対称）。
-}
mergeAdjacent : Note -> Note -> Maybe Note
mergeAdjacent a b =
    if a.pitch == b.pitch && a.start + a.duration == b.start then
        Just { a | duration = a.duration + b.duration }

    else
        Nothing
