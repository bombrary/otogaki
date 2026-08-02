module ChordFormatTest exposing (suite)

import Data.Chord exposing (Alteration(..), Chord, Extension(..), Quality(..))
import Data.Chord.Format as Format
import Data.Chord.Parser as Parser
import Expect
import Test exposing (Test, describe, test)


chord : Int -> Quality -> List Extension -> List Alteration -> Maybe Int -> Chord
chord root quality extensions alterations bass =
    { root = root, quality = quality, extensions = extensions, alterations = alterations, bass = bass }


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


suite : Test
suite =
    describe "Data.Chord.Format roundtrip"
        [ describe "preferFlat = False" (List.map (roundtripTest False) cases)
        , describe "preferFlat = True" (List.map (roundtripTest True) cases)
        ]
