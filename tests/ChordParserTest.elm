module ChordParserTest exposing (suite)

import Data.Chord exposing (Alteration(..), Extension(..), Quality(..))
import Data.Chord.Parser as Parser
import Expect
import Test exposing (Test, describe, test)


type alias Expected =
    { root : Int
    , quality : Quality
    , extensions : List Extension
    , alterations : List Alteration
    , bass : Maybe Int
    , voicing : Maybe String
    }


ok : String -> Expected -> Test
ok input expected =
    test input <|
        \_ -> Expect.equal (Ok expected) (Parser.parse input)


err : String -> Test
err input =
    test ("エラー: " ++ input) <|
        \_ ->
            case Parser.parse input of
                Ok _ ->
                    Expect.fail "パースが成功してしまった"

                Err _ ->
                    Expect.pass


chord : Int -> Quality -> List Extension -> Maybe Int -> Expected
chord root quality extensions bass =
    { root = root, quality = quality, extensions = extensions, alterations = [], bass = bass, voicing = Nothing }


altChord : Int -> Quality -> List Extension -> List Alteration -> Maybe Int -> Expected
altChord root quality extensions alterations bass =
    { root = root, quality = quality, extensions = extensions, alterations = alterations, bass = bass, voicing = Nothing }


suite : Test
suite =
    describe "コードパーサ"
        [ ok "C" (chord 0 Maj [] Nothing)
        , ok "G" (chord 7 Maj [] Nothing)
        , ok "F#" (chord 6 Maj [] Nothing)
        , ok "Bb" (chord 10 Maj [] Nothing)
        , ok "Am" (chord 9 Min [] Nothing)
        , ok "Em" (chord 4 Min [] Nothing)
        , ok "C7" (chord 0 Dom7 [] Nothing)
        , ok "G7" (chord 7 Dom7 [] Nothing)
        , ok "Cmaj7" (chord 0 Maj7 [] Nothing)
        , ok "CM7" (chord 0 Maj7 [] Nothing)
        , ok "Am7" (chord 9 Min7 [] Nothing)
        , ok "Dm7" (chord 2 Min7 [] Nothing)
        , ok "F#m7" (chord 6 Min7 [] Nothing)
        , ok "Cm9" (chord 0 Min7 [ Nine ] Nothing)
        , ok "C9" (chord 0 Dom7 [ Nine ] Nothing)
        , ok "Cmaj9" (chord 0 Maj7 [ Nine ] Nothing)
        , ok "C13" (chord 0 Dom7 [ Thirteen ] Nothing)
        , ok "Cadd9" (chord 0 Maj [ Add9 ] Nothing)
        , ok "C6" (chord 0 Sixth [] Nothing)
        , ok "Cm6" (chord 0 MinSixth [] Nothing)
        , ok "C5" (chord 0 Power [] Nothing)
        , ok "Csus4" (chord 0 Sus4 [] Nothing)
        , ok "Dsus2" (chord 2 Sus2 [] Nothing)
        , ok "E7sus4" (chord 4 Dom7Sus4 [] Nothing)
        , ok "C7sus4" (chord 0 Dom7Sus4 [] Nothing)
        , ok "Bdim" (chord 11 Dim [] Nothing)
        , ok "Bdim7" (chord 11 Dim7 [] Nothing)
        , ok "Bm7b5" (chord 11 HalfDim7 [] Nothing)
        , ok "Caug" (chord 0 Aug [] Nothing)
        , ok "C+" (chord 0 Aug [] Nothing)
        , ok "CmM7" (chord 0 MinMaj7 [] Nothing)
        , ok "C7b9" (chord 0 Dom7 [ FlatNine ] Nothing)
        , ok "C7(#9)" (chord 0 Dom7 [ SharpNine ] Nothing)
        , ok "C7(#11)" (chord 0 Dom7 [ SharpEleven ] Nothing)
        , ok "Am7/G" (chord 9 Min7 [] (Just 7))
        , ok "C/E" (chord 0 Maj [] (Just 4))
        , ok "G7b5" (altChord 7 Dom7 [] [ Flat5 ] Nothing)
        , ok "C7(b5)" (altChord 0 Dom7 [] [ Flat5 ] Nothing)
        , ok "E7#5" (altChord 4 Dom7 [] [ Sharp5 ] Nothing)
        , ok "Eaug7" (altChord 4 Dom7 [] [ Sharp5 ] Nothing)
        , ok "C+7" (altChord 0 Dom7 [] [ Sharp5 ] Nothing)
        , ok "DbM9" (chord 1 Maj7 [ Nine ] Nothing)
        , ok "CM13" (chord 0 Maj7 [ Thirteen ] Nothing)
        , ok "C69" (chord 0 Sixth [ Nine ] Nothing)
        , ok "Cm7-5" (chord 0 HalfDim7 [] Nothing)
        , ok "C7#5b9" (altChord 0 Dom7 [ FlatNine ] [ Sharp5 ] Nothing)
        , err "H7"
        , err "Cxyz"
        , err ""
        ]
