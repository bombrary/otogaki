module Data.Chord.Format exposing (degreeLabel, degreeLabelExtended, format, formatPlain, pitchName, qualitySuffix)

import Data.Chord exposing (Alteration(..), Chord, Extension(..), Quality(..))


{-| Chord を文字列に戻す。`Data.Chord.Parser` の qualityTable/suffixTable は多対1の表記ゆれを受け付けるため、
こちらは Quality/Extension/Alteration ごとに「パースしても余分な拡張を含まない」正規形を一つ選ぶ。
拡張・アルタレーションは常に括弧付きで出力し、Quality の接頭辞と混ざらないようにする。
roundtrip 保証は `tests/ChordFormatTest.elm` を参照。`chord.voicing` があれば末尾に `@name` を付ける。
-}
format : { preferFlat : Bool } -> Chord -> String
format opts chord =
    formatPlain opts chord
        ++ (case chord.voicing of
                Just name ->
                    "@" ++ name

                Nothing ->
                    ""
           )


{-| `format` から `@name`（ボイシング指定）を落とした版。第三者に渡すプレーンなコード譜に使う。
-}
formatPlain : { preferFlat : Bool } -> Chord -> String
formatPlain opts chord =
    pitchName opts.preferFlat chord.root
        ++ qualitySuffix chord.quality
        ++ String.concat (List.map extensionSuffix chord.extensions)
        ++ String.concat (List.map alterationSuffix chord.alterations)
        ++ (case chord.bass of
                Just bass ->
                    "/" ++ pitchName opts.preferFlat bass

                Nothing ->
                    ""
           )


{-| Quality 単体の正規形。どれも `Data.Chord.Parser.qualityTable` 上で extensions/alterations を
伴わないキーを選んでいる（例: "aug7" ではなく "aug"）ので、後ろに拡張トークンを続けても安全。
-}
qualitySuffix : Quality -> String
qualitySuffix quality =
    case quality of
        Maj ->
            ""

        Min ->
            "m"

        Dom7 ->
            "7"

        Maj7 ->
            "maj7"

        Min7 ->
            "m7"

        MinMaj7 ->
            "mM7"

        Dim ->
            "dim"

        Dim7 ->
            "dim7"

        HalfDim7 ->
            "m7b5"

        Aug ->
            "aug"

        Sus2 ->
            "sus2"

        Sus4 ->
            "sus4"

        Dom7Sus4 ->
            "7sus4"

        Sixth ->
            "6"

        MinSixth ->
            "m6"

        Power ->
            "5"


{-| 括弧付きの正規形だけを使う。括弧なしの短縮形（例: 裸の "9"）は qualityTable 側の
短縮と衝突する可能性があるため選ばない。
-}
extensionSuffix : Extension -> String
extensionSuffix ext =
    case ext of
        Add9 ->
            "(add9)"

        Nine ->
            "(9)"

        FlatNine ->
            "(b9)"

        SharpNine ->
            "(#9)"

        Eleven ->
            "(11)"

        SharpEleven ->
            "(#11)"

        Thirteen ->
            "(13)"

        FlatThirteen ->
            "(b13)"


alterationSuffix : Alteration -> String
alterationSuffix alt =
    case alt of
        Flat5 ->
            "(b5)"

        Sharp5 ->
            "(#5)"


sharpNames : List String
sharpNames =
    [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]


flatNames : List String
flatNames =
    [ "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B" ]


pitchName : Bool -> Int -> String
pitchName preferFlat pitchClass =
    let
        names =
            if preferFlat then
                flatNames

            else
                sharpNames
    in
    List.drop (modBy 12 pitchClass) names
        |> List.head
        |> Maybe.withDefault "C"


{-| ルートからの半音間隔（interval）を度数表記に変換する。`modBy 12` で正規化してからマッチするため、
負の値や12以上の値もオクターブを畳んで扱える（例: 12 -> "R", 16 -> "3"）。異名同音の解釈が分かれる箇所は
Extension の命名（`FlatNine` 等）と揃えた: 1半音は b9、2半音は 9、5半音は 11、9半音は 6。
-}
degreeLabel : Int -> String
degreeLabel interval =
    case modBy 12 interval of
        0 ->
            "R"

        1 ->
            "b9"

        2 ->
            "9"

        3 ->
            "b3"

        4 ->
            "3"

        5 ->
            "11"

        6 ->
            "b5"

        7 ->
            "5"

        8 ->
            "#5"

        9 ->
            "6"

        10 ->
            "b7"

        11 ->
            "7"

        _ ->
            ""


{-| `degreeLabel` の拡張版。オクターブ0（ルートから11半音以内、コードトーン域）と
それ以外（1オクターブ以上離れたテンション域）で別のテーブルを引く。このアプリが採用する
ピアノロール表記（b3/3/4/b5/5/#5/b7/7/9/#9/11/#11/13/b13など）はこちらで表現する。
`modBy 12` はElmの仕様上、負のoffsetでも常に0〜11の非負値を返す（例: `modBy 12 -1 == 11`）。
そのため `octave = (offset - semitone) // 12` は負のoffsetでも安全に整数のオクターブ数
（0, ±1, ±2...）になる。
-}
degreeLabelExtended : Int -> String
degreeLabelExtended offset =
    let
        semitone =
            modBy 12 offset

        octave =
            (offset - semitone) // 12
    in
    if octave == 0 then
        case semitone of
            0 ->
                "R"

            1 ->
                "b9"

            2 ->
                "9"

            3 ->
                "b3"

            4 ->
                "3"

            5 ->
                "4"

            6 ->
                "b5"

            7 ->
                "5"

            8 ->
                "#5"

            9 ->
                "6"

            10 ->
                "b7"

            11 ->
                "7"

            _ ->
                ""

    else
        case semitone of
            0 ->
                "R"

            1 ->
                "b9"

            2 ->
                "9"

            3 ->
                "b3"

            4 ->
                "3"

            5 ->
                "11"

            6 ->
                "#11"

            7 ->
                "5"

            8 ->
                "b13"

            9 ->
                "13"

            10 ->
                "b7"

            11 ->
                "7"

            _ ->
                ""
