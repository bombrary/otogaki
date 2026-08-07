module GuitarFormTest exposing (suite)

import Data.Chord exposing (Chord, Quality(..))
import Data.GuitarForm as GuitarForm
import Expect
import Set
import Test exposing (Test, describe, test)


chord : Int -> Quality -> Chord
chord root quality =
    { root = root, quality = quality, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }


suite : Test
suite =
    describe "Data.GuitarForm"
        [ test "オープン C のピッチ列が期待値と一致する" <|
            \_ ->
                case GuitarForm.forChord (chord 0 Maj) of
                    Just form ->
                        Expect.equal [ 48, 52, 55, 60, 64 ] (GuitarForm.toPitches form)

                    Nothing ->
                        Expect.fail "C major のフォームが引けなかった"
        , test "F が E型1フレットバレーになる" <|
            \_ ->
                case GuitarForm.forChord (chord 5 Maj) of
                    Just form ->
                        Expect.equal [ 41, 48, 53, 57, 60, 65 ] (GuitarForm.toPitches form)

                    Nothing ->
                        Expect.fail "F major のフォームが引けなかった"
        , test "未対応 quality で例外を投げずに Nothing を返す" <|
            \_ ->
                Expect.equal Nothing (GuitarForm.forChord (chord 0 Sus2))
        , test "bestForm の結果を toPitches に通すと元のピッチ集合に戻る（往復性）" <|
            \_ ->
                let
                    pitches =
                        [ 48, 52, 55, 60, 64 ]
                in
                case GuitarForm.bestForm pitches of
                    Just resultForm ->
                        Expect.equal (List.sort pitches) (GuitarForm.toPitches resultForm |> List.sort)

                    Nothing ->
                        Expect.fail "bestForm が Nothing を返した"
        , test "7音以上のピッチ列では Nothing" <|
            \_ ->
                Expect.equal Nothing (GuitarForm.bestForm [ 40, 45, 50, 55, 59, 64, 69 ])
        , test "どの弦にも収まらない極端に低いピッチでは Nothing" <|
            \_ ->
                Expect.equal Nothing (GuitarForm.bestForm [ 20 ])
        , test "どの弦にも収まらない極端に高いピッチでは Nothing" <|
            \_ ->
                Expect.equal Nothing (GuitarForm.bestForm [ 120 ])
        , test "開放弦そのものを渡すと全弦が開放で選ばれる" <|
            \_ ->
                Expect.equal (Just (List.repeat 6 (Just 0))) (GuitarForm.bestForm GuitarForm.openStrings |> Maybe.map .frets)
        , test "openStrings はレギュラーチューニング（E2/A2/D3/G3/B3/E4）" <|
            \_ ->
                Expect.equal [ 40, 45, 50, 55, 59, 64 ] GuitarForm.openStrings
        , test "shiftPicks は選択中 offset の運指だけを動かす" <|
            \_ ->
                Expect.equal
                    (Set.fromList [ ( 2, 0 ), ( 7, 1 ) ])
                    (GuitarForm.shiftPicks 2 100 (Set.singleton 0) (Set.fromList [ ( 0, 0 ), ( 7, 1 ) ]))
        , test "shiftPicks は未選択の運指を動かさない" <|
            \_ ->
                Expect.equal
                    (Set.fromList [ ( 0, 0 ), ( 9, 1 ) ])
                    (GuitarForm.shiftPicks 2 100 (Set.singleton 7) (Set.fromList [ ( 0, 0 ), ( 7, 1 ) ]))
        , test "shiftPicks は 0 未満にはクランプされる" <|
            \_ ->
                Expect.equal
                    (Set.fromList [ ( 0, 0 ) ])
                    (GuitarForm.shiftPicks -5 100 (Set.singleton 0) (Set.fromList [ ( 0, 0 ) ]))
        , test "shiftPicks は maxOffset を超えないようにクランプされる" <|
            \_ ->
                Expect.equal
                    (Set.fromList [ ( 10, 0 ) ])
                    (GuitarForm.shiftPicks 100 10 (Set.singleton 0) (Set.fromList [ ( 0, 0 ) ]))
        , test "removePicks は指定 offset の運指を、弦が違っても全部捨てる" <|
            \_ ->
                Expect.equal
                    (Set.fromList [ ( 0, 0 ), ( 16, 2 ) ])
                    (GuitarForm.removePicks (Set.singleton 7) (Set.fromList [ ( 0, 0 ), ( 7, 1 ), ( 7, 3 ), ( 16, 2 ) ]))
        , test "removePicks は選択が空なら何も削除しない" <|
            \_ ->
                Expect.equal
                    (Set.fromList [ ( 0, 0 ), ( 7, 1 ) ])
                    (GuitarForm.removePicks Set.empty (Set.fromList [ ( 0, 0 ), ( 7, 1 ) ]))
        , test "formFromPicks は offsets 全てに運指があれば Just を返す" <|
            \_ ->
                -- root=40(E2と同じピッチ)、offset 0 を 0弦開放、offset 7 を 1弦 0フレット（open=45, fret=2）
                Expect.equal
                    (Just { frets = [ Just 0, Just 2, Nothing, Nothing, Nothing, Nothing ] })
                    (GuitarForm.formFromPicks 40 [ 0, 7 ] (Set.fromList [ ( 0, 0 ), ( 7, 1 ) ]))
        , test "formFromPicks は一部の offset に運指がなければ Nothing を返す" <|
            \_ ->
                Expect.equal
                    Nothing
                    (GuitarForm.formFromPicks 40 [ 0, 7 ] (Set.fromList [ ( 0, 0 ) ]))
        , test "formFromPicks は offsets が空なら Nothing を返す" <|
            \_ ->
                Expect.equal
                    Nothing
                    (GuitarForm.formFromPicks 40 [] (Set.fromList [ ( 0, 0 ) ]))
        , test "toIndexedPitches はミュート弦を除いて (弦インデックス, ピッチ) のペア列を返す" <|
            \_ ->
                Expect.equal
                    [ ( 0, 40 ), ( 1, 47 ) ]
                    (GuitarForm.toIndexedPitches { frets = [ Just 0, Just 2, Nothing, Nothing, Nothing, Nothing ] })
        , test "toIndexedPitches は全弦ミュートなら空リストを返す" <|
            \_ ->
                Expect.equal
                    []
                    (GuitarForm.toIndexedPitches { frets = [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ] })
        ]
