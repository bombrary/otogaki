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
                            , chordTrack = { text = "C | G | Am | F", instrument = Piano, muted = False, volume = 100, rhythm = Nothing, octaveShift = 0 }
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
        , describe "コード進行の音域（octaveShift）"
            [ test "octaveShift 0 と 1 では pitch だけが +12 で他は同じ（ベタうち・ストローク両方）" <|
                \_ ->
                    let
                        eventsWith rhythm shift =
                            Performance.toEvents (chordProject rhythm shift)
                                |> List.filter (\e -> e.trackId == Data.ChordTrack.trackId)

                        check rhythm =
                            let
                                a =
                                    eventsWith rhythm 0

                                b =
                                    eventsWith rhythm 1
                            in
                            Expect.all
                                [ \_ -> Expect.greaterThan 0 (List.length a)
                                , \_ -> Expect.equal (List.length a) (List.length b)
                                , \_ -> Expect.equal (List.map (\e -> e.pitch + 12) a) (List.map .pitch b)
                                , \_ -> Expect.equal (List.map (\e -> { e | pitch = 0 }) a) (List.map (\e -> { e | pitch = 0 }) b)
                                ]
                                ()
                    in
                    Expect.all [ \_ -> check Nothing, \_ -> check (Just "8ビート") ] ()
            , test "octaveShift 2 でも pitch は 127 を超えない" <|
                \_ ->
                    Performance.toEvents (chordProject Nothing 2)
                        |> List.filter (\e -> e.trackId == Data.ChordTrack.trackId)
                        |> List.all (\e -> e.pitch <= 127 && e.pitch >= 0)
                        |> Expect.equal True
            , test "通常トラックのノートは octaveShift に影響されない" <|
                \_ ->
                    let
                        pitchesOf shift =
                            Performance.toEvents (chordProject Nothing shift)
                                |> List.filter (\e -> e.trackId == 1)
                                |> List.map .pitch
                    in
                    Expect.equal (pitchesOf 0) (pitchesOf 2)
            ]
        ]


chordProject : Maybe String -> Int -> Project.Project
chordProject rhythm shift =
    { emptyProject
        | tracks = [ { id = 1, name = "t", instrument = Piano, muted = False, volume = 100, kind = NoteTrack [ note 1 0 240 ] } ]
        , chordTrack = { text = "C | G | Am | F", instrument = Piano, muted = False, volume = 100, rhythm = rhythm, octaveShift = shift }
    }


emptyProject : Project.Project
emptyProject =
    Project.empty
