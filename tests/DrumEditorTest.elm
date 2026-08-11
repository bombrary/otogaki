module DrumEditorTest exposing (suite)

import Expect
import Set
import Test exposing (Test, describe, test)
import View.DrumEditor as DrumEditor


suite : Test
suite =
    describe "View.DrumEditor"
        [ describe "pitchesInYRange"
            [ test "1行に収まる範囲は該当行のpitchだけを返す（先頭行＝クラッシュ 49）" <|
                \_ ->
                    DrumEditor.pitchesInYRange 0 10
                        |> Expect.equal (Set.fromList [ 49 ])
            , test "1行に収まる範囲は該当行のpitchだけを返す（中間行＝ハイハット 42）" <|
                \_ ->
                    DrumEditor.pitchesInYRange 45 60
                        |> Expect.equal (Set.fromList [ 42 ])
            , test "複数行にまたがる範囲はまたがった全行のpitchを返す" <|
                \_ ->
                    -- rowHeight=22: row0=[0,22) 49, row1=[22,44) 46, row2=[44,66) 42
                    DrumEditor.pitchesInYRange 20 50
                        |> Expect.equal (Set.fromList [ 49, 46, 42 ])
            , test "全rowsを覆う範囲は6行すべてのpitchを返す" <|
                \_ ->
                    DrumEditor.pitchesInYRange 0 132
                        |> Expect.equal (Set.fromList [ 49, 46, 42, 45, 38, 36 ])
            , test "行境界ちょうどでは境界の次の行を含まない（排他的判定）" <|
                \_ ->
                    -- y1 = 22 は row0=[0,22) の終端ちょうどなので row1=[22,44) は含まない
                    DrumEditor.pitchesInYRange 0 22
                        |> Expect.equal (Set.fromList [ 49 ])
            , test "範囲がグリッド外（負の座標）なら空集合を返す" <|
                \_ ->
                    DrumEditor.pitchesInYRange -20 -5
                        |> Expect.equal Set.empty
            ]
        ]
