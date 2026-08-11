module ChordFormatTest exposing (suite)

import Data.Chord exposing (Alteration(..), Chord, Extension(..), Quality(..))
import Data.Chord.Format as Format
import Data.Chord.Parser as Parser
import Expect
import Test exposing (Test, describe, test)


chord : Int -> Quality -> List Extension -> List Alteration -> Maybe Int -> Chord
chord root quality extensions alterations bass =
    { root = root, quality = quality, extensions = extensions, alterations = alterations, bass = bass, voicing = Nothing }


{-| `ChordParserTest` で parse の成功ケースとして与えていると同じ Chord 集合。入力文字列の表記ゆれではなく、
実際にアプリが扱う Chord レコードの集合で roundtrip を検証する。
-}
cases : List Chord
cases =
    [ chord 0 Maj [] [] Nothing
    , chord 7 Maj [] [] Nothing
    , chord 6 Maj [] [] Nothing
    , chord 10 Maj [] [] Nothing
    , chord 9 Min [] [] Nothing
    , chord 4 Min [] [] Nothing
    , chord 0 Dom7 [] [] Nothing
    , chord 7 Dom7 [] [] Nothing
    , chord 0 Maj7 [] [] Nothing
    , chord 9 Min7 [] [] Nothing
    , chord 2 Min7 [] [] Nothing
    , chord 6 Min7 [] [] Nothing
    , chord 0 Min7 [ Nine ] [] Nothing
    , chord 0 Dom7 [ Nine ] [] Nothing
    , chord 0 Maj7 [ Nine ] [] Nothing
    , chord 0 Dom7 [ Thirteen ] [] Nothing
    , chord 0 Maj [ Add9 ] [] Nothing
    , chord 0 Sixth [] [] Nothing
    , chord 0 MinSixth [] [] Nothing
    , chord 0 Power [] [] Nothing
    , chord 0 Sus4 [] [] Nothing
    , chord 2 Sus2 [] [] Nothing
    , chord 4 Dom7Sus4 [] [] Nothing
    , chord 11 Dim [] [] Nothing
    , chord 11 Dim7 [] [] Nothing
    , chord 11 HalfDim7 [] [] Nothing
    , chord 0 Aug [] [] Nothing
    , chord 0 MinMaj7 [] [] Nothing
    , chord 0 Dom7 [ FlatNine ] [] Nothing
    , chord 0 Dom7 [ SharpNine ] [] Nothing
    , chord 0 Dom7 [ SharpEleven ] [] Nothing
    , chord 9 Min7 [] [] (Just 7)
    , chord 0 Maj [] [] (Just 4)
    , chord 7 Dom7 [] [ Flat5 ] Nothing
    , chord 4 Dom7 [] [ Sharp5 ] Nothing
    , chord 1 Maj7 [ Nine ] [] Nothing
    , chord 0 Maj7 [ Thirteen ] [] Nothing
    , chord 0 Sixth [ Nine ] [] Nothing
    , chord 0 Dom7 [ FlatNine ] [ Sharp5 ] Nothing
    ]


roundtripTest : Bool -> Chord -> Test
roundtripTest preferFlat c =
    test (Format.format { preferFlat = preferFlat } c) <|
        \_ -> Expect.equal (Ok c) (Parser.parse (Format.format { preferFlat = preferFlat } c))


degreeLabelCases : List ( Int, String )
degreeLabelCases =
    [ ( 0, "R" )
    , ( 1, "b9" )
    , ( 2, "9" )
    , ( 3, "b3" )
    , ( 4, "3" )
    , ( 5, "11" )
    , ( 6, "b5" )
    , ( 7, "5" )
    , ( 8, "#5" )
    , ( 9, "6" )
    , ( 10, "b7" )
    , ( 11, "7" )
    ]


degreeLabelTest : ( Int, String ) -> Test
degreeLabelTest ( interval, label ) =
    test ("degreeLabel " ++ String.fromInt interval ++ " == " ++ label) <|
        \_ -> Expect.equal label (Format.degreeLabel interval)


degreeLabelExtendedCases : List ( Int, String )
degreeLabelExtendedCases =
    [ -- 明示的にサポートする14種の度数記号
      ( 3, "b3" )
    , ( 4, "3" )
    , ( 5, "4" )
    , ( 6, "b5" )
    , ( 7, "5" )
    , ( 8, "#5" )
    , ( 10, "b7" )
    , ( 11, "7" )
    , ( 14, "9" )
    , ( 15, "b3" )
    , ( 17, "11" )
    , ( 18, "#11" )
    , ( 21, "13" )
    , ( 20, "b13" )

    -- root
    , ( 0, "R" )

    -- リスト外だが既存degreeLabelと同じ規則を踏襲する代表値（オクターブ内）
    , ( 1, "b9" )
    , ( 2, "9" )
    , ( 9, "6" )

    -- 1オクターブ上のroot
    , ( 12, "R" )

    -- 負のoffset（Voicing.offsetsの下限は-12。-1はElmの modBy 12 -1 == 11 により
    -- semitone=11, octave=(-1-11)//12=-1 となり、octaveが0でないのでテンション側のテーブルを引く。
    -- テンション側テーブルはsemitone=11 -> "7"なので、-1 は "7" になる（elm-testで実際に確認済み）。
    , ( -1, "7" )
    ]


degreeLabelExtendedTest : ( Int, String ) -> Test
degreeLabelExtendedTest ( offset, label ) =
    test ("degreeLabelExtended " ++ String.fromInt offset ++ " == " ++ label) <|
        \_ -> Expect.equal label (Format.degreeLabelExtended offset)


suite : Test
suite =
    describe "Data.Chord.Format roundtrip"
        [ describe "preferFlat = False" (List.map (roundtripTest False) cases)
        , describe "preferFlat = True" (List.map (roundtripTest True) cases)
        , describe "degreeLabel" (List.map degreeLabelTest degreeLabelCases)
        , describe "degreeLabel modBy 12 wraps around"
            [ test "12 wraps to R (same as 0)" <|
                \_ -> Expect.equal "R" (Format.degreeLabel 12)
            , test "16 wraps to 3 (same as 4)" <|
                \_ -> Expect.equal "3" (Format.degreeLabel 16)
            , test "-1 wraps to 7 (same as 11)" <|
                \_ -> Expect.equal "7" (Format.degreeLabel -1)
            ]
        , describe "degreeLabelExtended" (List.map degreeLabelExtendedTest degreeLabelExtendedCases)
        ]
