module ProjectTest exposing (suite)

import Data.Project as Project
import Data.Timeline as Timeline
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
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
