module Data.ReferenceAudio exposing (ReferenceAudio, empty)


{-| 耳コピ・分析用の参考オーディオ。
音声データ自体は保存しない（localStorage の容量制限のため）。
offsetMs は「オーディオのどこを小節1・拍1に合わせるか」。
正なら頭を削る、負ならオーディオ全体を後ろにずらす。
-}
type alias ReferenceAudio =
    { fileName : Maybe String
    , offsetMs : Int
    , volume : Int
    , muted : Bool
    }


empty : ReferenceAudio
empty =
    { fileName = Nothing
    , offsetMs = 0
    , volume = 80
    , muted = False
    }
