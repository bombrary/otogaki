module PlainTextTest exposing (suite, wrapSuite)

import Data.ChordSheet as ChordSheet
import Data.ChordTrack as ChordTrack
import Expect
import Test exposing (Test, describe, test)


trackWithText : String -> ChordTrack.ChordTrack
trackWithText text =
    let
        base =
            ChordTrack.empty
    in
    { base | text = text }


suite : Test
suite =
    describe "ChordTrack.toPlainText"
        [ test "@NAME だけを落とす" <|
            \_ ->
                Expect.equal "C | G/B | Am7 | F" (ChordTrack.toPlainText (trackWithText "C | G/B@wide | Am7 | F"))
        , test "コメント・改行・%/_/= トークンを壊さない" <|
            \_ ->
                let
                    original =
                        "C@wide | % | _ | =  // コメント\nG7@wide | Cmaj7"

                    expected =
                        "C | % | _ | =  // コメント\nG7 | Cmaj7"
                in
                Expect.equal expected (ChordTrack.toPlainText (trackWithText original))
        , test "パース失敗しているトークンはそのまま残る" <|
            \_ ->
                Expect.equal "C | XyzInvalid | F" (ChordTrack.toPlainText (trackWithText "C | XyzInvalid | F"))
        , test "@NAME を含まないテキストは完全に同じ文字列を返す" <|
            \_ ->
                let
                    original =
                        "C | G/B | Am7 | F  // イントロ"
                in
                Expect.equal original (ChordTrack.toPlainText (trackWithText original))
        ]


wrapSuite : Test
wrapSuite =
    describe "ChordTrack.wrapBarLines"
        [ test "8小節1行は4小節×2行に改行される" <|
            \_ ->
                let
                    original =
                        "C | G | Am | F | C | G | F | G"

                    expected =
                        "C | G | Am | F |\nC | G | F | G |"
                in
                Expect.equal expected (ChordTrack.wrapBarLines 4 original)
        , test "4小節以下の行は完全に不変（空白・区切り含め）" <|
            \_ ->
                let
                    original =
                        "C  |G/B|  Am7 | F"
                in
                Expect.equal original (ChordTrack.wrapBarLines 4 original)
        , test "境界の空小節が | 前置で保存され小節数が不変" <|
            \_ ->
                let
                    original =
                        "C | G | Am | F | | G"

                    wrapped =
                        ChordTrack.wrapBarLines 4 original

                    originalBars =
                        ChordSheet.parse ("セクション\n" ++ original ++ " |")
                            |> Result.map (List.concatMap .bars)

                    wrappedBars =
                        ChordSheet.parse ("セクション\n" ++ wrapped)
                            |> Result.map (List.concatMap .bars)
                in
                Expect.equal originalBars wrappedBars
        , test "コメント付き長行はコメントが最終行に残る" <|
            \_ ->
                let
                    original =
                        "C | G | Am | F | C | G | F | G  // サビ全体"

                    wrapped =
                        ChordTrack.wrapBarLines 4 original
                in
                Expect.all
                    [ \_ -> Expect.equal True (String.endsWith "// サビ全体" wrapped)
                    , \_ -> Expect.equal 2 (List.length (String.split "\n" wrapped))
                    ]
                    ()
        , test "複数セクション行は行ごとに独立に折り返される" <|
            \_ ->
                let
                    original =
                        "C | G | Am | F | C | G | F | G\nDm | G"

                    expected =
                        "C | G | Am | F |\nC | G | F | G |\nDm | G"
                in
                Expect.equal expected (ChordTrack.wrapBarLines 4 original)
        ]
