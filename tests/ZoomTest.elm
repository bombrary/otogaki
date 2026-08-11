module ZoomTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import View.Zoom as Zoom


limits : { min : Int, max : Int }
limits =
    { min = 6, max = 120 }


suite : Test
suite =
    describe "View.Zoom.step"
        [ test "deltaY がホイール1ノッチ相当（-100）なら従来互換の1.2倍" <|
            \_ ->
                Zoom.step limits -100 20
                    |> Expect.equal 24
        , test "deltaY がホイール1ノッチ相当（100）なら従来互換の1/1.2倍" <|
            \_ ->
                Zoom.step limits 100 24
                    |> Expect.equal 20
        , test "微小な負の deltaY でも必ず1以上拡大する（量子化で動かなくならない）" <|
            \_ ->
                Zoom.step limits -1 6
                    |> Expect.equal 7
        , test "微小な正の deltaY でも必ず1以上縮小する（量子化で動かなくならない）" <|
            \_ ->
                Zoom.step limits 1 7
                    |> Expect.equal 6
        , test "中間値でも微小な deltaY は1ずつ動く" <|
            \_ ->
                Zoom.step limits -1 40
                    |> Expect.equal 41
        , test "deltaY が 0 なら変化しない" <|
            \_ ->
                Zoom.step limits 0 40
                    |> Expect.equal 40
        , test "上限（max）を超えて拡大しない" <|
            \_ ->
                Zoom.step limits -100 limits.max
                    |> Expect.equal limits.max
        , test "下限（min）を下回って縮小しない" <|
            \_ ->
                Zoom.step limits 100 limits.min
                    |> Expect.equal limits.min
        , test "数百以上の deltaY でも ±100 にクランプされて従来同等の効きに沿えられる" <|
            \_ ->
                Zoom.step limits -500 20
                    |> Expect.equal 24
        ]
