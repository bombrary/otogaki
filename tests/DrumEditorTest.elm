module DrumEditorTest exposing (suite)

import Expect
import Set
import Test exposing (Test, describe, test)
import View.DrumEditor as DrumEditor


note : Int -> Int -> { id : Int, pitch : Int, start : Int, duration : Int, velocity : Int }
note id pitch =
    { id = id, pitch = pitch, start = 0, duration = 120, velocity = 100 }


suite : Test
suite =
    describe "View.DrumEditor"
        [ describe "pitchesInYRange"
            [ test "1行に収まる範囲は該当行のpitchだけを返す（先頭行＝クラッシュ 49）" <|
                \_ ->
                    DrumEditor.pitchesInYRange (DrumEditor.rowsFor []) 0 10
                        |> Expect.equal (Set.fromList [ 49 ])
            , test "1行に収まる範囲は該当行のpitchだけを返す（中間行＝ハイハット 42、row3=[66,88)）" <|
                \_ ->
                    DrumEditor.pitchesInYRange (DrumEditor.rowsFor []) 70 80
                        |> Expect.equal (Set.fromList [ 42 ])
            , test "複数行にまたがる範囲はまたがった全行のpitchを返す" <|
                \_ ->
                    -- rowHeight=22, rowsFor [] = [49クラッシュ, 51ライド, 46オープンHH, 42ハイハット, 45タム, 38スネア, 36キック]
                    -- row0=[0,22) 49, row1=[22,44) 51, row2=[44,66) 46
                    DrumEditor.pitchesInYRange (DrumEditor.rowsFor []) 20 50
                        |> Expect.equal (Set.fromList [ 49, 51, 46 ])
            , test "全rowsを覆う範囲はコア行すべてのpitchを返す" <|
                \_ ->
                    DrumEditor.pitchesInYRange (DrumEditor.rowsFor []) 0 1000
                        |> Expect.equal (Set.fromList (List.map Tuple.first (DrumEditor.rowsFor [])))
            , test "行境界ちょうどでは境界の次の行を含まない（排他的判定）" <|
                \_ ->
                    -- y1 = 22 は row0=[0,22) の終端ちょうどなので row1=[22,44) は含まない
                    DrumEditor.pitchesInYRange (DrumEditor.rowsFor []) 0 22
                        |> Expect.equal (Set.fromList [ 49 ])
            , test "範囲がグリッド外（負の座標）なら空集合を返す" <|
                \_ ->
                    DrumEditor.pitchesInYRange (DrumEditor.rowsFor []) -20 -5
                        |> Expect.equal Set.empty
            ]
        , describe "rowsFor"
            [ test "ノートが空ならコア行のみで、ライド(51)を含む" <|
                \_ ->
                    DrumEditor.rowsFor []
                        |> List.map Tuple.first
                        |> Expect.equal [ 49, 51, 46, 42, 45, 38, 36 ]
            , test "コア行に無い pitch のノートがあると行が現れ、コア行の相対順序は変わらない" <|
                \_ ->
                    DrumEditor.rowsFor [ note 1 39 ]
                        |> List.map Tuple.first
                        |> Expect.equal [ 39, 49, 51, 46, 42, 45, 38, 36 ]
            , test "同じ pitch のノートが複数あっても行は1つ" <|
                \_ ->
                    DrumEditor.rowsFor [ note 1 39, note 2 39, note 3 39 ]
                        |> List.filter (\( p, _ ) -> p == 39)
                        |> List.length
                        |> Expect.equal 1
            ]
        ]
