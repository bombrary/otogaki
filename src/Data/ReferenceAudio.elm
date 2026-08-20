module Data.ReferenceAudio exposing (ReferenceAudio, empty)


{-| 耳コピ・分析用の参考オーディオ。
音声データ自体は localStorage ではなくブラウザの IndexedDB（js/audioCache.js）に保存されるので、
リロード後も自動復元される。この型自体（JSONに乗る方）にはファイル名と長さ（durationMs）だけを保持する。
リロード直後（波形自動復元前でも）グリッドの長さを保ちたいため（「24小節の壁」の再発防止）。
offsetMs は「オーディオのどこを小節1・拍1に合わせるか」。
正なら頭を削る、負ならオーディオ全体を後ろにずらす。
-}
type alias ReferenceAudio =
    { fileName : Maybe String
    , offsetMs : Int
    , volume : Int
    , muted : Bool
    , durationMs : Maybe Int
    }


empty : ReferenceAudio
empty =
    { fileName = Nothing
    , offsetMs = 0
    , volume = 80
    , muted = False
    , durationMs = Nothing
    }
