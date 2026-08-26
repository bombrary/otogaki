module TapTest exposing (suite)

import Data.Tap as Tap
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Data.Tap"
        [ describe "isDoubleTap"
            [ test "同一ターゲット・250ms・10pxならダブルタップと判定する" <|
                \_ ->
                    let
                        last =
                            Tap.record (Tap.TapNote 1) { timeStamp = 0, clientX = 100, clientY = 100 }
                    in
                    Tap.isDoubleTap (Tap.TapNote 1) { timeStamp = 250, clientX = 105, clientY = 95 } last
                        |> Expect.equal True
            , test "350ms離れていればダブルタップではない" <|
                \_ ->
                    let
                        last =
                            Tap.record (Tap.TapNote 1) { timeStamp = 0, clientX = 100, clientY = 100 }
                    in
                    Tap.isDoubleTap (Tap.TapNote 1) { timeStamp = 350, clientX = 100, clientY = 100 } last
                        |> Expect.equal False
            , test "30px離れていればダブルタップではない" <|
                \_ ->
                    let
                        last =
                            Tap.record (Tap.TapNote 1) { timeStamp = 0, clientX = 100, clientY = 100 }
                    in
                    Tap.isDoubleTap (Tap.TapNote 1) { timeStamp = 250, clientX = 130, clientY = 100 } last
                        |> Expect.equal False
            , test "ターゲットが違えばダブルタップではない（ノートID違い）" <|
                \_ ->
                    let
                        last =
                            Tap.record (Tap.TapNote 1) { timeStamp = 0, clientX = 100, clientY = 100 }
                    in
                    Tap.isDoubleTap (Tap.TapNote 2) { timeStamp = 250, clientX = 100, clientY = 100 } last
                        |> Expect.equal False
            , test "ターゲットが違えばダブルタップではない（ドラムセルのpitch違い）" <|
                \_ ->
                    let
                        last =
                            Tap.record (Tap.TapDrumCell { pitch = 36, tick = 0 }) { timeStamp = 0, clientX = 100, clientY = 100 }
                    in
                    Tap.isDoubleTap (Tap.TapDrumCell { pitch = 38, tick = 0 }) { timeStamp = 250, clientX = 100, clientY = 100 } last
                        |> Expect.equal False
            , test "直前のタップが無ければ常にFalse" <|
                \_ ->
                    Tap.isDoubleTap (Tap.TapNote 1) { timeStamp = 250, clientX = 100, clientY = 100 } Nothing
                        |> Expect.equal False
            ]
        , describe "record"
            [ test "record した値を isDoubleTap に食わせると往復する" <|
                \_ ->
                    let
                        pos =
                            { timeStamp = 42, clientX = 10, clientY = 20 }
                    in
                    Tap.record (Tap.TapVoicingOffset 0 3) pos
                        |> Tap.isDoubleTap (Tap.TapVoicingOffset 0 3) { timeStamp = 42, clientX = 10, clientY = 20 }
                        |> Expect.equal True
            ]
        ]
