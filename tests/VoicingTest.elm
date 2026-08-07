module VoicingTest exposing (suite)

import Data.Chord as Chord
import Data.Chord.Format as Format
import Data.Chord.Parser as Parser
import Data.Voicing exposing (Voicing)
import Expect
import Set
import Test exposing (Test, describe, test)


wideVoicing : Voicing
wideVoicing =
    { name = "wide", offsets = [ 0, 7, 16, 23 ], stringPicks = Set.empty }


suite : Test
suite =
    describe "ボイシングの @NAME 記法"
        [ test "Cmaj7@wide のパース→format 往復で元の文字列に戻る" <|
            \_ ->
                case Parser.parse "Cmaj7@wide" of
                    Ok chord ->
                        Expect.equal "Cmaj7@wide" (Format.format { preferFlat = False } chord)

                    Err reason ->
                        Expect.fail ("パース失敗: " ++ reason)
        , test "Cmaj7@wide/E と Cmaj7/E@wide は同じ Chord になる" <|
            \_ ->
                Expect.equal (Parser.parse "Cmaj7@wide/E") (Parser.parse "Cmaj7/E@wide")
        , test "未登録のボイシング名では既存の固定ルールにフォールバックする" <|
            \_ ->
                case Parser.parse "Cmaj7@nope" of
                    Ok chord ->
                        Expect.equal (Chord.toPitches { chord | voicing = Nothing }) (Chord.toPitchesWith [ wideVoicing ] chord)

                    Err reason ->
                        Expect.fail ("パース失敗: " ++ reason)
        , test "トランスポーズしても @NAME が残る" <|
            \_ ->
                case Parser.parse "Cmaj7@wide" of
                    Ok chord ->
                        let
                            transposed =
                                { chord | root = modBy 12 (chord.root + 2) }
                        in
                        Expect.equal "Dmaj7@wide" (Format.format { preferFlat = False } transposed)

                    Err reason ->
                        Expect.fail ("パース失敗: " ++ reason)
        , test "toPitchesWith [] はボイシング指定を無視して toPitches と同じ結果になる" <|
            \_ ->
                case Parser.parse "Cmaj7@wide" of
                    Ok chord ->
                        Expect.equal (Chord.toPitches { chord | voicing = Nothing }) (Chord.toPitchesWith [] chord)

                    Err reason ->
                        Expect.fail ("パース失敗: " ++ reason)
        , test "toPitchesWith [wide] chord は Data.Voicing.pitchesFor と一致する" <|
            \_ ->
                case Parser.parse "Cmaj7@wide" of
                    Ok chord ->
                        Expect.equal (Data.Voicing.pitchesFor chord.root wideVoicing) (Chord.toPitchesWith [ wideVoicing ] chord)

                    Err reason ->
                        Expect.fail ("パース失敗: " ++ reason)
        , test "offset 0 を含むボイシングではベース音がユニゾン重複しない" <|
            \_ ->
                case Parser.parse "Cmaj7@wide" of
                    Ok chord ->
                        let
                            pitches =
                                Chord.toPitchesWith [ wideVoicing ] chord

                            uniquePitches =
                                List.foldl
                                    (\p acc ->
                                        if List.member p acc then
                                            acc

                                        else
                                            acc ++ [ p ]
                                    )
                                    []
                                    pitches
                        in
                        Expect.equal (List.length pitches) (List.length uniquePitches)

                    Err reason ->
                        Expect.fail ("パース失敗: " ++ reason)
        , test "/E 付きのときだけベースが先頭に付く" <|
            \_ ->
                case Parser.parse "Cmaj7@wide/E" of
                    Ok chord ->
                        case Chord.toPitchesWith [ wideVoicing ] chord of
                            first :: _ ->
                                Expect.equal (Data.Voicing.anchorPitch + 4) first

                            [] ->
                                Expect.fail "ピッチが空"

                    Err reason ->
                        Expect.fail ("パース失敗: " ++ reason)
        , test "pitchesFor は root のピッチクラスで正しく移調する" <|
            \_ ->
                Expect.equal
                    (List.map ((+) 2) (Data.Voicing.pitchesFor 0 wideVoicing))
                    (Data.Voicing.pitchesFor 2 wideVoicing)
        , test "shiftOffsets は選択中の offset だけを動かす" <|
            \_ ->
                Expect.equal
                    [ 2, 7, 16 ]
                    (Data.Voicing.shiftOffsets 2 100 (Set.singleton 0) [ 0, 7, 16 ])
        , test "shiftOffsets は未選択の offset を動かさない" <|
            \_ ->
                Expect.equal
                    [ 0, 9, 16 ]
                    (Data.Voicing.shiftOffsets 2 100 (Set.singleton 7) [ 0, 7, 16 ])
        , test "shiftOffsets は 0 未満にはクランプされる" <|
            \_ ->
                Expect.equal
                    [ 0, 7 ]
                    (Data.Voicing.shiftOffsets -5 100 (Set.singleton 0) [ 0, 7 ])
        , test "shiftOffsets は maxOffset を超えないようにクランプされる" <|
            \_ ->
                Expect.equal
                    [ 10 ]
                    (Data.Voicing.shiftOffsets 100 10 (Set.singleton 0) [ 0 ])
        , test "shiftOffsets で移動先が既存の別 offset と重複したら片方に潰れる" <|
            \_ ->
                Expect.equal
                    1
                    (Data.Voicing.shiftOffsets 7 100 (Set.singleton 0) [ 0, 7 ] |> List.length)
        , test "removeOffsets は選択中の offset だけを削除する" <|
            \_ ->
                Expect.equal
                    [ 0, 16 ]
                    (Data.Voicing.removeOffsets (Set.singleton 7) [ 0, 7, 16 ])
        , test "removeOffsets は選択が空なら何も削除しない" <|
            \_ ->
                Expect.equal
                    [ 0, 7 ]
                    (Data.Voicing.removeOffsets Set.empty [ 0, 7 ])
        , test "displayRoot は最小 offset を足したピッチを返す" <|
            \_ ->
                Expect.equal
                    43
                    (Data.Voicing.displayRoot 36 [ 7, 16, 23 ])
        , test "displayRoot は offsets が空なら rootPitch をそのまま返す" <|
            \_ ->
                Expect.equal
                    36
                    (Data.Voicing.displayRoot 36 [])
        , test "findByName は名前が一致するボイシングを返す" <|
            \_ ->
                Expect.equal
                    (Just wideVoicing)
                    (Data.Voicing.findByName "wide" [ wideVoicing ])
        , test "findByName は一致するものがなければ Nothing を返す" <|
            \_ ->
                Expect.equal
                    Nothing
                    (Data.Voicing.findByName "nope" [ wideVoicing ])
        ]
