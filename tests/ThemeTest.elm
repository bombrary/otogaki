module ThemeTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import View.Theme as Theme


suite : Test
suite =
    describe "View.Theme"
        [ describe "withAlpha"
            [ test "# 付きHEXをrgbaに変換できる" <|
                \_ ->
                    Expect.equal "rgba(0, 97, 164, 0.5)" (Theme.withAlpha 0.5 "#0061A4")
            , test "# なしHEXも受け付ける" <|
                \_ ->
                    Expect.equal "rgba(0, 97, 164, 0.5)" (Theme.withAlpha 0.5 "0061A4")
            , test "小文字HEXも正しくパースされる" <|
                \_ ->
                    Expect.equal "rgba(74, 144, 217, 0.15)" (Theme.withAlpha 0.15 "#4a90d9")
            , test "白と黒の境界値" <|
                \_ ->
                    Expect.equal "rgba(255, 255, 255, 1)" (Theme.withAlpha 1 "#FFFFFF")
            ]
        ]
