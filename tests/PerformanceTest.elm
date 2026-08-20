module PerformanceTest exposing (suite)

import Codec.Performance as Performance
import Data.ChordTrack
import Data.Project as Project
import Data.Track exposing (Instrument(..), TrackKind(..))
import Expect
import Test exposing (Test, describe, test)


note : Int -> Int -> Int -> { id : Int, pitch : Int, start : Int, duration : Int, velocity : Int }
note id start duration =
    { id = id, pitch = 60, start = start, duration = duration, velocity = 100 }


suite : Test
suite =
    describe "Codec.Performance.contentEndTicks"
        [ test "ノート・コードが一つもない空プロジェクトなら 0" <|
            \_ ->
                Expect.equal 0 (Performance.contentEndTicks Project.empty)
        , test "ノートのみなら最後のノートの終端 tick と一致する" <|
            \_ ->
                let
                    project =
                        { demo
                            | tracks =
                                [ { id = 1, name = "t", instrument = Piano, muted = False, volume = 100, kind = NoteTrack [ note 1 0 240, note 2 480 120 ] } ]
                            , chordTrack = Data.ChordTrack.empty
                        }

                    demo =
                        Project.empty
                in
                Expect.equal 600 (Performance.contentEndTicks project)
        , test "コードのみでもコード進行の終端まで含む（toEvents の集計と一致）" <|
            \_ ->
                let
                    project =
                        { demo
                            | tracks = []
                            , chordTrack = { text = "C | G | Am | F", instrument = Piano, muted = False, volume = 100, rhythm = Nothing }
                        }

                    demo =
                        Project.empty

                    fromToEvents =
                        Performance.toEvents project
                            |> List.foldl (\e acc -> Basics.max acc (e.ticks + e.durationTicks)) 0
                in
                Expect.all
                    [ \_ -> Expect.greaterThan 0 (Performance.contentEndTicks project)
                    , \_ -> Expect.equal fromToEvents (Performance.contentEndTicks project)
                    ]
                    ()
        , test "メトロノームは toEvents にそもそも混ざらない設計を前提にしている" <|
            \_ ->
                Performance.toEvents Project.empty
                    |> List.any (\e -> e.trackId == Performance.metronomeTrackId)
                    |> Expect.equal False
        ]
