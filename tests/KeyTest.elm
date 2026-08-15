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
        , newModesSuite
        ]


newModesSuite : Test
newModesSuite =
    describe "新規モード（Dorian/Phrygian/Lydian/Mixolydian/Locrian/HarmonicMinor）"
        [ describe "scalePitchClasses（scaleIntervals 経由）"
            [ test "C Dorian" <|
                \_ ->
                    Key.scalePitchClasses { tonic = 0, mode = Dorian }
                        |> Expect.equal (Set.fromList [ 0, 2, 3, 5, 7, 9, 10 ])
            , test "C Phrygian" <|
                \_ ->
                    Key.scalePitchClasses { tonic = 0, mode = Phrygian }
                        |> Expect.equal (Set.fromList [ 0, 1, 3, 5, 7, 8, 10 ])
            , test "C Lydian" <|
                \_ ->
                    Key.scalePitchClasses { tonic = 0, mode = Lydian }
                        |> Expect.equal (Set.fromList [ 0, 2, 4, 6, 7, 9, 11 ])
            , test "C Mixolydian" <|
                \_ ->
                    Key.scalePitchClasses { tonic = 0, mode = Mixolydian }
                        |> Expect.equal (Set.fromList [ 0, 2, 4, 5, 7, 9, 10 ])
            , test "C Locrian" <|
                \_ ->
                    Key.scalePitchClasses { tonic = 0, mode = Locrian }
                        |> Expect.equal (Set.fromList [ 0, 1, 3, 5, 6, 8, 10 ])
            , test "C Harmonic Minor" <|
                \_ ->
                    Key.scalePitchClasses { tonic = 0, mode = HarmonicMinor }
                        |> Expect.equal (Set.fromList [ 0, 2, 3, 5, 7, 8, 11 ])
            ]
        , describe "degreeLabel（genericRomanFor 経由）"
            [ test "C Dorian の I（C）" <|
                \_ ->
                    Key.degreeLabel { tonic = 0, mode = Dorian } { spelledFlat = False } (mkChord 0 Maj)
                        |> Expect.equal "I"
            , test "C Dorian の ii（Dm）" <|
                \_ ->
                    Key.degreeLabel { tonic = 0, mode = Dorian } { spelledFlat = False } (mkChord 2 Min)
                        |> Expect.equal "ii"
            , test "C Dorian でスケール外音（半音）を sharp 綴りで出すと直下の度数に ♯" <|
                \_ ->
                    Key.degreeLabel { tonic = 0, mode = Dorian } { spelledFlat = False } (mkChord 11 Maj)
                        |> Expect.equal "♯VII"
            , test "C Dorian で最高度数を超えるスケール外音を flat 綴りで出すと次オクターブの ♭I" <|
                \_ ->
                    Key.degreeLabel { tonic = 0, mode = Dorian } { spelledFlat = True } (mkChord 11 Maj)
                        |> Expect.equal "♭I"
            , test "C Mixolydian の VII7（Bb7）" <|
                \_ ->
                    Key.degreeLabel { tonic = 0, mode = Mixolydian } { spelledFlat = False } (mkChord 10 Dom7)
                        |> Expect.equal "VII7"
            , test "A Harmonic Minor の V7（E7、ハーモニックマイナーのドミナント）" <|
                \_ ->
                    Key.degreeLabel { tonic = 9, mode = HarmonicMinor } { spelledFlat = False } (mkChord 4 Dom7)
                        |> Expect.equal "V7"
            ]
        , describe "modeToString / modeFromString の round-trip"
            [ test "Dorian" <| \_ -> Key.modeFromString (Key.modeToString Dorian) |> Expect.equal (Just Dorian)
            , test "Phrygian" <| \_ -> Key.modeFromString (Key.modeToString Phrygian) |> Expect.equal (Just Phrygian)
            , test "Lydian" <| \_ -> Key.modeFromString (Key.modeToString Lydian) |> Expect.equal (Just Lydian)
            , test "Mixolydian" <| \_ -> Key.modeFromString (Key.modeToString Mixolydian) |> Expect.equal (Just Mixolydian)
            , test "Locrian" <| \_ -> Key.modeFromString (Key.modeToString Locrian) |> Expect.equal (Just Locrian)
            , test "HarmonicMinor" <| \_ -> Key.modeFromString (Key.modeToString HarmonicMinor) |> Expect.equal (Just HarmonicMinor)
            ]
        ]
