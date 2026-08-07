module KeyTest exposing (suite)

import Data.Chord exposing (Quality(..))
import Data.Key as Key exposing (Mode(..))
import Expect
import Set
import Test exposing (Test, describe, test)


majorC : Key.Key
majorC =
    { tonic = 0, mode = Major }


minorA : Key.Key
minorA =
    { tonic = 9, mode = Minor }


mkChord : Int -> Quality -> Data.Chord.Chord
mkChord root quality =
    { root = root, quality = quality, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }


suite : Test
suite =
    describe "Data.Key"
        [ describe "degreeLabel（C Major 基準のダイアトニックコード）"
            [ test "I（C）" <|
                \_ ->
                    Key.degreeLabel majorC { spelledFlat = False } (mkChord 0 Maj)
                        |> Expect.equal "I"
            , test "ii（Dm）" <|
                \_ ->
                    Key.degreeLabel majorC { spelledFlat = False } (mkChord 2 Min)
                        |> Expect.equal "ii"
            , test "V7（G7）" <|
                \_ ->
                    Key.degreeLabel majorC { spelledFlat = False } (mkChord 7 Dom7)
                        |> Expect.equal "V7"
            , test "vi（Am）" <|
                \_ ->
                    Key.degreeLabel majorC { spelledFlat = False } (mkChord 9 Min)
                        |> Expect.equal "vi"
            , test "♭VII（Bb）" <|
                \_ ->
                    Key.degreeLabel majorC { spelledFlat = True } (mkChord 10 Maj)
                        |> Expect.equal "♭VII"
            , test "♯IV（F#、シャープ綴り）" <|
                \_ ->
                    Key.degreeLabel majorC { spelledFlat = False } (mkChord 6 Maj)
                        |> Expect.equal "♯IV"
            , test "♭V（Gb、フラット綴り）" <|
                \_ ->
                    Key.degreeLabel majorC { spelledFlat = True } (mkChord 6 Maj)
                        |> Expect.equal "♭V"
            ]
        , describe "scalePitchClasses"
            [ test "C Major は白鍵の7音" <|
                \_ ->
                    Key.scalePitchClasses majorC
                        |> Expect.equal (Set.fromList [ 0, 2, 4, 5, 7, 9, 11 ])
            , test "A Minor（自然的短音階）も白鍵の7音" <|
                \_ ->
                    Key.scalePitchClasses minorA
                        |> Expect.equal (Set.fromList [ 0, 2, 4, 5, 7, 9, 11 ])
            ]
        , describe "toString / fromString"
            [ test "C" <| \_ -> Key.toString majorC |> Expect.equal "C"
            , test "Am" <| \_ -> Key.toString minorA |> Expect.equal "Am"
            , test "fromString \"C\"" <| \_ -> Key.fromString "C" |> Expect.equal (Just majorC)
            , test "fromString \"Am\"" <| \_ -> Key.fromString "Am" |> Expect.equal (Just minorA)
            , test "fromString \"Bb\"" <| \_ -> Key.fromString "Bb" |> Expect.equal (Just { tonic = 10, mode = Major })
            , test "fromString \"?\" は Nothing" <| \_ -> Key.fromString "?" |> Expect.equal Nothing
            ]
        , describe "isFlatSpelled"
            [ test "Bb はフラット" <| \_ -> Key.isFlatSpelled "Bb" |> Expect.equal True
            , test "C# はフラットではない" <| \_ -> Key.isFlatSpelled "C#" |> Expect.equal False
            , test "C はフラットではない" <| \_ -> Key.isFlatSpelled "C" |> Expect.equal False
            ]
        ]
