module TimeTest exposing (suite)

import Data.Time as Time
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Data.Time"
        [ describe "gridTicks"
            [ test "Sixteenth は 120 tick" <|
                \_ -> Time.gridTicks Time.Sixteenth |> Expect.equal 120
            , test "EighthTriplet は 160 tick" <|
                \_ -> Time.gridTicks Time.EighthTriplet |> Expect.equal 160
            , test "SixteenthTriplet は 80 tick" <|
                \_ -> Time.gridTicks Time.SixteenthTriplet |> Expect.equal 80
            ]
        , describe "gridUnitToString / gridUnitFromString"
            [ test "Sixteenth の往復" <|
                \_ ->
                    Time.Sixteenth
                        |> Time.gridUnitToString
                        |> Time.gridUnitFromString
                        |> Expect.equal (Just Time.Sixteenth)
            , test "EighthTriplet の往復" <|
                \_ ->
                    Time.EighthTriplet
                        |> Time.gridUnitToString
                        |> Time.gridUnitFromString
                        |> Expect.equal (Just Time.EighthTriplet)
            , test "SixteenthTriplet の往復" <|
                \_ ->
                    Time.SixteenthTriplet
                        |> Time.gridUnitToString
                        |> Time.gridUnitFromString
                        |> Expect.equal (Just Time.SixteenthTriplet)
            , test "不正な文字列は Nothing" <|
                \_ -> Time.gridUnitFromString "nope" |> Expect.equal Nothing
            ]
        , describe "snapRound"
            [ test "160 grid で 250 は 320 に丸まる" <|
                \_ -> Time.snapRound 160 250 |> Expect.equal 320
            , test "160 grid で 230 は 160 に丸まる" <|
                \_ -> Time.snapRound 160 230 |> Expect.equal 160
            , test "負の delta も正しく丸まる" <|
                \_ -> Time.snapRound 160 -90 |> Expect.equal -160
            ]
        , describe "snapFloor"
            [ test "80 grid で 119 は 80 に切り下げ" <|
                \_ -> Time.snapFloor 80 119 |> Expect.equal 80
            , test "160 grid で 479 は 320 に切り下げ" <|
                \_ -> Time.snapFloor 160 479 |> Expect.equal 320
            , test "snapFloor の結果は常に grid の倍数" <|
                \_ -> modBy 160 (Time.snapFloor 160 733) |> Expect.equal 0
            ]
        , test "1小節(1920)は120/160/80すべての公倍数（どのグリッドでも小節頭に到達可能）" <|
            \_ ->
                [ Time.gridTicks Time.Sixteenth, Time.gridTicks Time.EighthTriplet, Time.gridTicks Time.SixteenthTriplet ]
                    |> List.map (\g -> modBy g Time.ticksPerBar)
                    |> Expect.equal [ 0, 0, 0 ]
        ]
