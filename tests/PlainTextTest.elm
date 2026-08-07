module PlainTextTest exposing (suite)

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
