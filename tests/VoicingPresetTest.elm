module VoicingPresetTest exposing (suite)

import Data.Chord as Chord exposing (Quality(..))
import Data.Voicing as Voicing
import Data.VoicingPreset as VoicingPreset exposing (Shape(..))
import Expect
import Set
import Test exposing (Test, describe, test)


chord : Chord.Chord
chord =
    { root = 0, quality = Maj7, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }


suite : Test
suite =
    describe "Data.VoicingPreset"
        [ test "offsetsFor Maj7 Closed は toPitches（フォールバック）と同じピッチ集合を作る" <|
            \_ ->
                let
                    voicing =
                        { name = "tmp", offsets = VoicingPreset.offsetsFor Maj7 Closed, stringPicks = Set.empty }

                    fromPreset =
                        Voicing.pitchesFor chord.root voicing |> List.sort

                    fromFallback =
                        Chord.toPitches chord |> List.sort
                in
                Expect.equal fromFallback fromPreset
        , test "Drop2 は maj7 で [0, 7, 12, 16, 23] を返す" <|
            \_ ->
                Expect.equal [ 0, 7, 12, 16, 23 ] (VoicingPreset.offsetsFor Maj7 Drop2)
        , test "Wide は maj7 で [0, 12, 19, 28, 35] を返す" <|
            \_ ->
                Expect.equal [ 0, 12, 19, 28, 35 ] (VoicingPreset.offsetsFor Maj7 Wide)
        , test "全 quality × 全配置で offsetsFor が空リストを返さない" <|
            \_ ->
                let
                    allEmpty =
                        VoicingPreset.qualities
                            |> List.concatMap (\q -> List.map (\( _, shape ) -> VoicingPreset.offsetsFor q shape) VoicingPreset.shapes)
                            |> List.any List.isEmpty
                in
                Expect.equal False allEmpty
        , test "qualityByLabel と qualityLabel は往復する" <|
            \_ ->
                let
                    roundTripOk =
                        VoicingPreset.qualities
                            |> List.all (\q -> VoicingPreset.qualityByLabel (VoicingPreset.qualityLabel q) == Just q)
                in
                Expect.equal True roundTripOk
        , test "shapeByName と shapeLabel は往復する" <|
            \_ ->
                let
                    roundTripOk =
                        VoicingPreset.shapes
                            |> List.all (\( name, shape ) -> VoicingPreset.shapeByName name == Just shape && VoicingPreset.shapeLabel shape == name)
                in
                Expect.equal True roundTripOk
        ]
