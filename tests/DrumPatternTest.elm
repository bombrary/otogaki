module DrumPatternTest exposing (suite)

import Data.DrumPattern as DrumPattern
import Data.Project as Project
import Data.ReferenceAudio as ReferenceAudio
import Data.Time
import Data.Track exposing (Instrument(..), TrackKind(..))
import Expect
import Test exposing (Test, describe, test)


baseProject : Project.Project
baseProject =
    { name = "test"
    , bpm = 120.0
    , tracks =
        [ { id = 1, name = "drums", instrument = Piano, muted = False, volume = 100, kind = DrumTrack [] }
        ]
    , chordTrack = { text = "", instrument = Piano, muted = False, volume = 100 }
    , sections = []
    , scraps = []
    , referenceAudio = ReferenceAudio.empty
    , nextId = 1000
    , memo = ""
    , voicings = []
    , voicingEnabled = True
    , guitarFormEnabled = True
    }


{-| 指定プリセットを 1小節分 apply した結果のノート列を返す。
-}
notesFor : String -> List { id : Int, pitch : Int, start : Int, duration : Int, velocity : Int }
notesFor presetName =
    case DrumPattern.byName presetName of
        Just pattern ->
            DrumPattern.apply { trackId = 1, startTicks = 0, endTicks = Data.Time.ticksPerBar } pattern baseProject
                |> .tracks
                |> List.head
                |> Maybe.map
                    (\t ->
                        case t.kind of
                            DrumTrack notes ->
                                notes

                            NoteTrack notes ->
                                notes
                    )
                |> Maybe.withDefault []

        Nothing ->
            []


velocityAt : Int -> Int -> List { a | pitch : Int, start : Int, velocity : Int } -> Maybe Int
velocityAt pitch step notes =
    notes
        |> List.filter (\n -> n.pitch == pitch && n.start == step * Data.Time.ticksPerSixteenth)
        |> List.head
        |> Maybe.map .velocity


suite : Test
suite =
    describe "Data.DrumPattern プリセットのvelocity強弱"
        [ test "全プリセットで velocity が 1-127 の範囲に収まる" <|
            \_ ->
                let
                    allVelocities =
                        DrumPattern.patterns
                            |> List.concatMap (\p -> notesFor p.name |> List.map .velocity)
                in
                Expect.equal True (List.all (\v -> v >= 1 && v <= 127) allVelocities)
        , describe "各プリセットで velocity が単一値ではない（強弱がついている）"
            (List.map
                (\p ->
                    test p.name <|
                        \_ ->
                            let
                                velocities =
                                    notesFor p.name |> List.map .velocity
                            in
                            Expect.notEqual (List.minimum velocities) (List.maximum velocities)
                )
                DrumPattern.patterns
            )
        , test "8ビート: 小節頭のキック（pitch 36, step 0）はハイハットの裏拍（pitch 42, step 2）より強い" <|
            \_ ->
                let
                    notes =
                        notesFor "8ビート"

                    kick =
                        velocityAt 36 0 notes |> Maybe.withDefault 0

                    hatOffbeat =
                        velocityAt 42 2 notes |> Maybe.withDefault 0
                in
                Expect.greaterThan hatOffbeat kick
        , test "4つ打ち: 小節頭のキック（pitch 36, step 0）はオープンHH（pitch 46, step 2）より強い" <|
            \_ ->
                let
                    notes =
                        notesFor "4つ打ち"

                    kick =
                        velocityAt 36 0 notes |> Maybe.withDefault 0

                    openHat =
                        velocityAt 46 2 notes |> Maybe.withDefault 0
                in
                Expect.greaterThan openHat kick
        ]
