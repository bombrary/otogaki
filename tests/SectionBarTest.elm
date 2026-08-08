module SectionBarTest exposing (suite)

import Data.Key
import Data.Meter
import Data.Section exposing (Section)
import Expect
import Test exposing (Test, describe, test)
import View.SectionBar as SectionBar


section : Int -> Int -> Section
section id lengthBars =
    { id = id, name = "s" ++ String.fromInt id, lengthBars = lengthBars, memo = "", key = Data.Key.default, meter = Data.Meter.default }


{-| id=1(4小節,idx0) id=2(2小節,idx1) id=3(4小節,idx2) の3つらべ。idx0とidx2はどちらも
隣（idx1, id=2）が2小節 = regionPxPerBar * 2 = 80px、しきい値はその半分の40px。
-}
sections : List Section
sections =
    [ section 1 4, section 2 2, section 3 4 ]


suite : Test
suite =
    Test.concat
        [ sectionDragTargetIndexSuite
        , sectionStartBarsSuite
        ]


sectionDragTargetIndexSuite : Test
sectionDragTargetIndexSuite =
    describe "View.SectionBar.sectionDragTargetIndex"
        [ test "隣接セクション幅の半分未満の accumDx（正方向）では Nothingを返す" <|
            \_ ->
                SectionBar.sectionDragTargetIndex sections 0 39
                    |> Expect.equal Nothing
        , test "隣接セクション幅の半分以上（正方向）で Just を返し、index が1進む" <|
            \_ ->
                SectionBar.sectionDragTargetIndex sections 0 40
                    |> Expect.equal (Just ( 1, -40 ))
        , test "隣接セクション幅の半分以上（負方向）で Just を返し、index が1減る" <|
            \_ ->
                SectionBar.sectionDragTargetIndex sections 2 -40
                    |> Expect.equal (Just ( 1, 40 ))
        , test "隣接セクション幅の半分未満の accumDx（負方向）では Nothingを返す" <|
            \_ ->
                SectionBar.sectionDragTargetIndex sections 2 -39
                    |> Expect.equal Nothing
        , test "進行方向（末尾）に隣接セクションがなければ常に Nothing" <|
            \_ ->
                SectionBar.sectionDragTargetIndex sections 2 1000
                    |> Expect.equal Nothing
        , test "進行方向（先頭）に隣接セクションがなければ常に Nothing" <|
            \_ ->
                SectionBar.sectionDragTargetIndex sections 0 -1000
                    |> Expect.equal Nothing
        ]


sectionStartBarsSuite : Test
sectionStartBarsSuite =
    describe "View.SectionBar.sectionStartBars"
        [ test "セクションの開始小節（0-based）を累積で返す" <|
            \_ ->
                SectionBar.sectionStartBars sections
                    |> Expect.equal [ 0, 4, 6 ]
        , test "空リストでは空リストを返す" <|
            \_ ->
                SectionBar.sectionStartBars []
                    |> Expect.equal []
        , test "セクションが1つだけなら [0] を返す" <|
            \_ ->
                SectionBar.sectionStartBars [ section 1 8 ]
                    |> Expect.equal [ 0 ]
        ]
