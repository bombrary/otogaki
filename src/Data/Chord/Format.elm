module Data.Chord.Format exposing (format)

import Data.Chord exposing (Alteration(..), Chord, Extension(..), Quality(..))


{-| Chord を文字列に戻す。`Data.Chord.Parser` の qualityTable/suffixTable は多対1の表記ゆれを受け付けるため、
こちらは Quality/Extension/Alteration ごとに「パースしても余分な拡張を含まない」正規形を一つ選ぶ。
拡張・アルタレーションは常に括弧付きで出力し、Quality の接頭辞と混ざらないようにする。
roundtrip 保証は `tests/ChordFormatTest.elm` を参照。
-}
format : { preferFlat : Bool } -> Chord -> String
format opts chord =
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
