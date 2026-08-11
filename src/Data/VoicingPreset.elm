module Data.VoicingPreset exposing (Shape(..), offsetsFor, qualities, qualityByLabel, qualityLabel, shapeByName, shapeLabel, shapeSuffix, shapes)

import Data.Chord exposing (Quality(..), qualityIntervals)


{-| ボイシングプリセットの配置。どれも anchorPitch（= root のピッチクラス）を offset 0 のベース音として含む。
-}
type Shape
    = Closed
    | Drop2
    | Wide


{-| 全 quality。`Data.Chord.Quality` に並べ列挙リストが無いのでここで新設。
-}
qualities : List Quality
qualities =
    [ Maj, Min, Dom7, Maj7, Min7, MinMaj7, Dim, Dim7, HalfDim7, Aug, Sus2, Sus4, Dom7Sus4, Sixth, MinSixth, Power ]


shapes : List ( String, Shape )
shapes =
    [ ( "クローズド", Closed )
    , ( "ドロップ2", Drop2 )
    , ( "ワイド", Wide )
    ]


shapeLabel : Shape -> String
shapeLabel shape =
    shapes
        |> List.filter (\( _, s ) -> s == shape)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault ""


shapeByName : String -> Maybe Shape
shapeByName name =
    shapes
        |> List.filter (\( n, _ ) -> n == name)
        |> List.head
        |> Maybe.map Tuple.second


{-| 保存名（ボイシング名）に使うサフィックス。3値とも重複せず、非空・ASCIIな短い識別子。
-}
shapeSuffix : Shape -> String
shapeSuffix shape =
    case shape of
        Closed ->
            "closed"

        Drop2 ->
            "drop2"

        Wide ->
            "wide"


{-| 選択 UI 用の短い表示名。`Data.Chord.Parser.qualityTable` は多対1なので名前の出どころには使わず、
ここで Quality と 1対1のラベルを持つ。
-}
qualityLabel : Quality -> String
qualityLabel quality =
    case quality of
        Maj ->
            "maj"

        Min ->
            "min"

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


qualityByLabel : String -> Maybe Quality
qualityByLabel label =
    qualities
        |> List.filter (\q -> qualityLabel q == label)
        |> List.head


{-| quality × 配置から `Data.Voicing.Voicing.offsets` を作る。常に offset 0（ベース音）を含むので空リストにはならない。
-}
offsetsFor : Quality -> Shape -> List Int
offsetsFor quality shape =
    let
        intervals =
            qualityIntervals quality
    in
    case shape of
        Closed ->
            0 :: List.map ((+) 12) intervals

        Drop2 ->
            let
                closedTones =
                    List.map ((+) 12) intervals

                n =
                    List.length closedTones
            in
            if n < 2 then
                0 :: closedTones

            else
                let
                    secondFromTopIndex =
                        n - 2
                in
                0
                    :: (closedTones
                            |> List.indexedMap
                                (\i v ->
                                    if i == secondFromTopIndex then
                                        v - 12

                                    else
                                        v
                                )
                            |> List.sort
                       )

        Wide ->
            0
                :: (intervals
                        |> List.indexedMap (\i v -> v + 12 + 12 * modBy 2 i)
                        |> List.sort
                   )
