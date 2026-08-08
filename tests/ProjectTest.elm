module ProjectTest exposing (suite)

import Data.Key
import Data.Meter
import Data.Project as Project
import Data.Section exposing (Section)
import Data.Timeline as Timeline
import Expect
import Test exposing (Test, describe, test)


section : Int -> Section
section id =
    { id = id, name = "s" ++ String.fromInt id, lengthBars = 4, memo = "", key = Data.Key.default, meter = Data.Meter.default }


suite : Test
suite =
    Test.concat
        [ timelineSuite
        , moveSectionToIndexSuite
        ]


timelineSuite : Test
timelineSuite =
    describe "Data.Project.timeline"
        [ test "参考オーディオが長ければ minBars が伸びる" <|
            \_ ->
                let
                    project =
                        Project.demo

                    ra =
                        project.referenceAudio

                    -- 120bpm 4/4 で 60秒（=120拍=30小節）分の参考音声
                    withLongRefAudio =
                        { project | referenceAudio = { ra | durationMs = Just 60000 } }

                    tl =
                        Project.timeline withLongRefAudio
                in
                Expect.atLeast 30 (Timeline.totalBars tl)
        , test "参考オーディオがなければ従来通りの minBars（16）を下回らない" <|
            \_ ->
                let
                    project =
                        Project.demo

                    tl =
                        Project.timeline project
                in
                Expect.atLeast 16 (Timeline.totalBars tl)
        , test "offsetMs の分だけ実効長が短くなり minBars が小さくなる" <|
            \_ ->
                let
                    project =
                        Project.demo

                    ra =
                        project.referenceAudio

                    withOffset =
                        { project | referenceAudio = { ra | durationMs = Just 60000, offsetMs = 30000 } }

                    withoutOffset =
                        { project | referenceAudio = { ra | durationMs = Just 60000, offsetMs = 0 } }
                in
                Expect.atMost
                    (Timeline.totalBars (Project.timeline withoutOffset))
                    (Timeline.totalBars (Project.timeline withOffset))
        ]


moveSectionToIndexSuite : Test
moveSectionToIndexSuite =
    let
        project =
            { demo | sections = [ section 1, section 2, section 3 ] }

        demo =
            Project.demo
    in
    describe "Data.Project.moveSectionToIndex"
        [ test "後ろの index に移動できる" <|
            \_ ->
                (Project.moveSectionToIndex 1 2 project).sections
                    |> List.map .id
                    |> Expect.equal [ 2, 3, 1 ]
        , test "前の index に移動できる" <|
            \_ ->
                (Project.moveSectionToIndex 3 0 project).sections
                    |> List.map .id
                    |> Expect.equal [ 3, 1, 2 ]
        , test "負の index は先頭にクランプされる" <|
            \_ ->
                (Project.moveSectionToIndex 3 -5 project).sections
                    |> List.map .id
                    |> Expect.equal [ 3, 1, 2 ]
        , test "大きすぎる index は末尾にクランプされる" <|
            \_ ->
                (Project.moveSectionToIndex 1 99 project).sections
                    |> List.map .id
                    |> Expect.equal [ 2, 3, 1 ]
        , test "存在しない sectionId を渡すと何も変わらない" <|
            \_ ->
                (Project.moveSectionToIndex 999 0 project).sections
                    |> List.map .id
                    |> Expect.equal [ 1, 2, 3 ]
        ]
