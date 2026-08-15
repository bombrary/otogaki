module StrumExpandTest exposing (suite)

import Data.Chord exposing (Extension(..), Quality(..))
import Data.GuitarForm as GuitarForm
import Data.Key as Key
import Data.Meter as Meter
import Data.StrumExpand as StrumExpand
import Data.StrumPattern exposing (Direction(..), Pattern, Pick(..))
import Data.Time
import Data.Timeline
import Data.Track exposing (Instrument(..))
import Data.Voicing
import Expect
import Set
import Test exposing (Test, describe, test)


downUpPattern : Pattern
downUpPattern =
    { name = "test", strums = [ { step = 0, direction = Down, velocity = 100, pick = AllStrings }, { step = 4, direction = Up, velocity = 70, pick = AllStrings } ] }


suite : Test
suite =
    Test.concat
        [ strumExpandSuite
        , previewNotesRhythmSuite
        ]


strumExpandSuite : Test
strumExpandSuite =
    describe "Data.StrumExpand"
        [ test "Down と Up で弦の順序が逆になる" <|
            \_ ->
                let
                    chordC =
                        { root = 0, quality = Maj, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }

                    resolvedChords =
                        [ { startTicks = 0, durationTicks = Data.Time.ticksPerBar, chord = chordC } ]

                    notes =
                        StrumExpand.expand { startTicks = 0, endTicks = Data.Time.ticksPerBar } True [] downUpPattern resolvedChords
                            |> List.sortBy .start

                    downGroup =
                        List.filter (\n -> n.start < 4 * Data.Time.ticksPerSixteenth) notes

                    upGroup =
                        List.filter (\n -> n.start >= 4 * Data.Time.ticksPerSixteenth) notes

                    downPitches =
                        List.map .pitch downGroup

                    upPitches =
                        List.map .pitch upGroup
                in
                Expect.equal ( True, True )
                    ( List.sort downPitches == downPitches
                    , List.sort upPitches == List.reverse upPitches
                    )
        , test "@NAME で登録ボイシングを指定したら、root/quality が forChord の固定表にあっても登録ボイシングの方を使う" <|
            \_ ->
                let
                    -- E は forChord の固定表にあり、何もしなければ [40,47,52,56,59,64] が鳴る。
                    -- ここでは全く別の音集合 [40,45,50,55] を持つボイシングを登録して上書きする
                    myopen =
                        { name = "myopen", offsets = [ 0, 5, 10, 15 ], stringPicks = Set.empty }

                    chordE =
                        { root = 4, quality = Maj, extensions = [], alterations = [], bass = Nothing, voicing = Just "myopen" }

                    resolvedChords =
                        [ { startTicks = 0, durationTicks = Data.Time.ticksPerBar, chord = chordE } ]

                    notes =
                        StrumExpand.expand { startTicks = 0, endTicks = Data.Time.ticksPerBar } True [ myopen ] downUpPattern resolvedChords

                    pitches =
                        notes |> List.map .pitch |> Set.fromList
                in
                Expect.equal ( True, False )
                    ( Set.member 40 pitches && Set.member 45 pitches && Set.member 50 pitches && Set.member 55 pitches
                    , Set.member 47 pitches
                    )
        , test "formFor は登録ボイシングがあれば forChord より優先する" <|
            \_ ->
                let
                    chordE =
                        { root = 4, quality = Maj, extensions = [], alterations = [], bass = Nothing, voicing = Just "myopen" }

                    myopen =
                        { name = "myopen", offsets = [ 0, 5, 10, 15 ], stringPicks = Set.empty }

                    pitches =
                        StrumExpand.formFor True [ myopen ] chordE
                            |> Maybe.map GuitarForm.toPitches
                            |> Maybe.withDefault []
                            |> Set.fromList
                in
                Expect.equal ( True, False )
                    ( Set.member 40 pitches && Set.member 45 pitches && Set.member 50 pitches && Set.member 55 pitches
                    , Set.member 47 pitches
                    )
        , test "formFor はボイシング未登録なら forChord にフォールバックする" <|
            \_ ->
                let
                    chordE =
                        { root = 4, quality = Maj, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }
                in
                Expect.equal (GuitarForm.forChord chordE) (StrumExpand.formFor True [] chordE)
        , test "formFor は forChord にない quality でも guitarFormEnabled が True なら bestForm で実運指を探す" <|
            \_ ->
                let
                    chordHalfDim =
                        { root = 6, quality = HalfDim7, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }
                in
                Expect.notEqual Nothing (StrumExpand.formFor True [] chordHalfDim)
        , test "formFor は guitarFormEnabled が False なら forChord が引けるコードでも常に Nothing を返す" <|
            \_ ->
                let
                    chordC =
                        { root = 0, quality = Maj, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }
                in
                Expect.equal Nothing (StrumExpand.formFor False [] chordC)
        , test "soundingPitches はスラッシュコードのベース音を最低音として鳴らす（Fm7/A#）" <|
            \_ ->
                let
                    chordFm7OnASharp =
                        { root = 5, quality = Min7, extensions = [], alterations = [], bass = Just 10, voicing = Nothing }

                    pitches =
                        StrumExpand.soundingPitches True [] chordFm7OnASharp
                in
                Expect.equal (Just 10) (List.minimum pitches |> Maybe.map (modBy 12))
        , test "soundingPitches は C/E でベースを足しつつフォームの音を保つ" <|
            \_ ->
                let
                    chordCOnE =
                        { root = 0, quality = Maj, extensions = [], alterations = [], bass = Just 4, voicing = Nothing }

                    chordC =
                        { root = 0, quality = Maj, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }

                    pitchesWithBass =
                        StrumExpand.soundingPitches True [] chordCOnE

                    pitchesWithoutBass =
                        StrumExpand.soundingPitches True [] chordC
                in
                Expect.equal ( Just 4, True )
                    ( List.minimum pitchesWithBass |> Maybe.map (modBy 12)
                    , List.all (\p -> List.member p pitchesWithBass) pitchesWithoutBass
                    )
        , test "soundingPitches はベースがフォームの最低音と同じピッチクラスなら重複追加しない" <|
            \_ ->
                let
                    chordCOnC =
                        { root = 0, quality = Maj, extensions = [], alterations = [], bass = Just 0, voicing = Nothing }

                    chordC =
                        { root = 0, quality = Maj, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }
                in
                Expect.equal (StrumExpand.soundingPitches True [] chordC) (StrumExpand.soundingPitches True [] chordCOnC)
        , test "soundingPitches は guitarFormEnabled が False でも従来どおりベース込み" <|
            \_ ->
                let
                    chordFm7OnASharp =
                        { root = 5, quality = Min7, extensions = [], alterations = [], bass = Just 10, voicing = Nothing }

                    pitches =
                        StrumExpand.soundingPitches False [] chordFm7OnASharp
                in
                Expect.equal (Just 10) (List.minimum pitches |> Maybe.map (modBy 12))
        , test "voicingFromForm と formFor の往復でフォームが弦単位のまま再現される" <|
            \_ ->
                let
                    chordBHalfDim =
                        { root = 11, quality = HalfDim7, extensions = [], alterations = [], bass = Nothing, voicing = Just "roundtrip" }

                    rootPitch =
                        Data.Voicing.anchorPitch + modBy 12 chordBHalfDim.root
                in
                case GuitarForm.candidatesFor chordBHalfDim |> List.filter (\c -> c.position == GuitarForm.ARoot) |> List.head of
                    Just cand ->
                        let
                            voicing =
                                StrumExpand.voicingFromForm rootPitch cand.form "roundtrip"

                            reconstructed =
                                StrumExpand.formFor True [ voicing ] chordBHalfDim
                        in
                        Expect.equal (Just cand.form.frets) (Maybe.map .frets reconstructed)

                    Nothing ->
                        Expect.fail "Bm7-5 の A型候補が引けなかった"
        , test "formFor は Cadd9 で9th（D=62）を含むフォームを返す（回帰テスト）" <|
            \_ ->
                let
                    chordCadd9 =
                        { root = 0, quality = Maj, extensions = [ Add9 ], alterations = [], bass = Nothing, voicing = Nothing }

                    pitches =
                        StrumExpand.formFor True [] chordCadd9
                            |> Maybe.map GuitarForm.toPitches
                            |> Maybe.withDefault []
                            |> Set.fromList
                in
                Expect.equal (Set.fromList [ 48, 52, 55, 62 ]) pitches
        , test "soundingPitches は対応スラッシュ（C/E）で最低音が bass pc で、withSlashBass が音を足さない（no-op）" <|
            \_ ->
                let
                    chordCOnE =
                        { root = 0, quality = Maj, extensions = [], alterations = [], bass = Just 4, voicing = Nothing }

                    formPitchCount =
                        GuitarForm.candidatesFor chordCOnE
                            |> List.filter (\c -> c.position == GuitarForm.Open)
                            |> List.head
                            |> Maybe.map (\c -> List.length (GuitarForm.toPitches c.form))
                            |> Maybe.withDefault 0

                    pitches =
                        StrumExpand.soundingPitches True [] chordCOnE

                    lowestPc =
                        List.minimum pitches |> Maybe.map (modBy 12)
                in
                Expect.equal ( Just 4, formPitchCount ) ( lowestPc, List.length pitches )
        , test "soundingPitches は対応スラッシュ（Dm7/G）で最低音が bass pc で、withSlashBass が音を足さない（no-op）" <|
            \_ ->
                let
                    chordDm7OnG =
                        { root = 2, quality = Min7, extensions = [], alterations = [], bass = Just 7, voicing = Nothing }

                    formPitchCount =
                        GuitarForm.candidatesFor chordDm7OnG
                            |> List.filter (\c -> c.position == GuitarForm.Open)
                            |> List.head
                            |> Maybe.map (\c -> List.length (GuitarForm.toPitches c.form))
                            |> Maybe.withDefault 0

                    pitches =
                        StrumExpand.soundingPitches True [] chordDm7OnG

                    lowestPc =
                        List.minimum pitches |> Maybe.map (modBy 12)
                in
                Expect.equal ( Just 7, formPitchCount ) ( lowestPc, List.length pitches )
        , test "8ビートプリセットのstep/direction列が期待どおり" <|
            \_ ->
                let
                    pattern =
                        Data.StrumPattern.byName "8ビート" |> Maybe.withDefault { name = "", strums = [] }

                    actual =
                        List.map (\s -> ( s.step, s.direction )) pattern.strums
                in
                Expect.equal [ ( 0, Down ), ( 4, Down ), ( 6, Up ), ( 10, Up ), ( 12, Down ), ( 14, Up ) ] actual
        , test "16ビートプリセットのstep/direction列が期待どおり" <|
            \_ ->
                let
                    pattern =
                        Data.StrumPattern.byName "16ビート" |> Maybe.withDefault { name = "", strums = [] }

                    actual =
                        List.map (\s -> ( s.step, s.direction )) pattern.strums
                in
                Expect.equal
                    [ ( 0, Down ), ( 4, Down ), ( 6, Down ), ( 7, Up ), ( 9, Up ), ( 10, Down ), ( 12, Down ), ( 14, Down ), ( 15, Up ) ]
                    actual
        , test "アルペジオパターンを適用すると、コード構成音の数だけノートが低音から順に1回ずつ配置される" <|
            \_ ->
                let
                    chordC =
                        { root = 0, quality = Maj, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }

                    pitches =
                        StrumExpand.soundingPitches True [] chordC

                    n =
                        List.length pitches

                    arpeggioPattern =
                        Data.StrumPattern.byName "アルペジオ（分散）" |> Maybe.withDefault { name = "", strums = [] }

                    resolvedChords =
                        [ { startTicks = 0, durationTicks = Data.Time.ticksPerBar, chord = chordC } ]

                    notes =
                        StrumExpand.expand { startTicks = 0, endTicks = Data.Time.ticksPerBar } True [] arpeggioPattern resolvedChords
                            |> List.sortBy .start

                    expectedPitches =
                        List.take n pitches

                    actualPitches =
                        List.map .pitch notes
                in
                Expect.equal ( n, expectedPitches ) ( List.length notes, actualPitches )
        , test "アルペジオは同一ピッチのノートが重複して生成されない" <|
            \_ ->
                let
                    chordCmaj7 =
                        { root = 0, quality = Maj7, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }

                    arpeggioPattern =
                        Data.StrumPattern.byName "アルペジオ（分散）" |> Maybe.withDefault { name = "", strums = [] }

                    resolvedChords =
                        [ { startTicks = 0, durationTicks = Data.Time.ticksPerBar, chord = chordCmaj7 } ]

                    notes =
                        StrumExpand.expand { startTicks = 0, endTicks = Data.Time.ticksPerBar } True [] arpeggioPattern resolvedChords

                    pitches =
                        List.map .pitch notes
                in
                Expect.equal (List.length pitches) (Set.size (Set.fromList pitches))
        , test "アルペジオの各ノートは開始時刻はずれるが、全て同じコードの終端まで伸びて重なる" <|
            \_ ->
                let
                    chordC =
                        { root = 0, quality = Maj, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }

                    arpeggioPattern =
                        Data.StrumPattern.byName "アルペジオ（分散）" |> Maybe.withDefault { name = "", strums = [] }

                    resolvedChords =
                        [ { startTicks = 0, durationTicks = Data.Time.ticksPerBar, chord = chordC } ]

                    notes =
                        StrumExpand.expand { startTicks = 0, endTicks = Data.Time.ticksPerBar } True [] arpeggioPattern resolvedChords

                    ends =
                        notes |> List.map (\n -> n.start + n.duration)
                in
                Expect.equal True (List.all (\e -> e == Data.Time.ticksPerBar) ends)
        , test "アルペジオの隣接ノートは重なって鳴る（次の開始が前の終了より前）" <|
            \_ ->
                let
                    chordC =
                        { root = 0, quality = Maj, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }

                    arpeggioPattern =
                        Data.StrumPattern.byName "アルペジオ（分散）" |> Maybe.withDefault { name = "", strums = [] }

                    resolvedChords =
                        [ { startTicks = 0, durationTicks = Data.Time.ticksPerBar, chord = chordC } ]

                    notes =
                        StrumExpand.expand { startTicks = 0, endTicks = Data.Time.ticksPerBar } True [] arpeggioPattern resolvedChords
                            |> List.sortBy .start

                    overlaps =
                        List.map2 (\a b -> b.start < a.start + a.duration) notes (List.drop 1 notes)
                in
                Expect.equal True (List.all identity overlaps)
        , test "アルペジオは1小節内でコードが切り替わっても、後半のコードの音が0から数え直されて鳴る" <|
            \_ ->
                let
                    chordAm7 =
                        { root = 9, quality = Min7, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }

                    chordFmaj7 =
                        { root = 5, quality = Maj7, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }

                    half =
                        Data.Time.ticksPerBar // 2

                    resolvedChords =
                        [ { startTicks = 0, durationTicks = half, chord = chordAm7 }
                        , { startTicks = half, durationTicks = half, chord = chordFmaj7 }
                        ]

                    arpeggioPattern =
                        Data.StrumPattern.byName "アルペジオ（分散）" |> Maybe.withDefault { name = "", strums = [] }

                    notes =
                        StrumExpand.expand { startTicks = 0, endTicks = Data.Time.ticksPerBar } True [] arpeggioPattern resolvedChords

                    secondHalfNotes =
                        notes |> List.filter (\n -> n.start >= half)
                in
                Expect.notEqual [] secondHalfNotes
        , test "Upストロークでも全音が発音し、順番は Down と逆になる" <|
            \_ ->
                let
                    chordC =
                        { root = 0, quality = Maj, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }

                    pitches =
                        StrumExpand.soundingPitches True [] chordC

                    resolvedChords =
                        [ { startTicks = 0, durationTicks = Data.Time.ticksPerBar, chord = chordC } ]

                    notes =
                        StrumExpand.expand { startTicks = 0, endTicks = Data.Time.ticksPerBar } True [] downUpPattern resolvedChords
                            |> List.sortBy .start

                    upGroupPitches =
                        notes |> List.filter (\note -> note.start >= 4 * Data.Time.ticksPerSixteenth) |> List.map .pitch
                in
                Expect.equal (List.reverse pitches) upGroupPitches
        , test "expand: コード切れ目をまたぐノートは生えない（resolvedChords を直接渡す純関数テスト）" <|
            \_ ->
                let
                    chordC =
                        { root = 0, quality = Maj, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }

                    chordG =
                        { root = 7, quality = Maj, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }

                    resolvedChords =
                        [ { startTicks = 0, durationTicks = Data.Time.ticksPerBar, chord = chordC }
                        , { startTicks = Data.Time.ticksPerBar, durationTicks = Data.Time.ticksPerBar, chord = chordG }
                        ]

                    notes =
                        StrumExpand.expand { startTicks = 0, endTicks = 2 * Data.Time.ticksPerBar } True [] downUpPattern resolvedChords

                    firstBarNotes =
                        List.filter (\n -> n.start < Data.Time.ticksPerBar) notes
                in
                Expect.equal True (List.all (\n -> n.start + n.duration <= Data.Time.ticksPerBar) firstBarNotes)
        , test "expand: Up ストロークの音は Down よりベロシティが低い" <|
            \_ ->
                let
                    chordC =
                        { root = 0, quality = Maj, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }

                    resolvedChords =
                        [ { startTicks = 0, durationTicks = Data.Time.ticksPerBar, chord = chordC } ]

                    notes =
                        StrumExpand.expand { startTicks = 0, endTicks = Data.Time.ticksPerBar } True [] downUpPattern resolvedChords

                    downVelocities =
                        notes |> List.filter (\n -> n.start < 4 * Data.Time.ticksPerSixteenth) |> List.map .velocity

                    upVelocities =
                        notes |> List.filter (\n -> n.start >= 4 * Data.Time.ticksPerSixteenth) |> List.map .velocity
                in
                Expect.equal ( True, True )
                    ( List.all (\v -> v == 100) downVelocities
                    , List.all (\v -> v == 70) upVelocities
                    )
        , test "expand: アルペジオ（StringIndex）パターンは各ステップでちょうど 1 音、かつコード終端まで伸びる" <|
            \_ ->
                let
                    chordC =
                        { root = 0, quality = Maj, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }

                    pitches =
                        StrumExpand.soundingPitches True [] chordC

                    resolvedChords =
                        [ { startTicks = 0, durationTicks = Data.Time.ticksPerBar, chord = chordC } ]

                    arpeggioPattern =
                        Data.StrumPattern.byName "アルペジオ（分散）" |> Maybe.withDefault { name = "", strums = [] }

                    notes =
                        StrumExpand.expand { startTicks = 0, endTicks = Data.Time.ticksPerBar } True [] arpeggioPattern resolvedChords
                in
                Expect.equal ( List.length pitches, True )
                    ( List.length notes, List.all (\n -> n.start + n.duration == Data.Time.ticksPerBar) notes )
        ]


previewNotesRhythmSuite : Test
previewNotesRhythmSuite =
    describe "Data.StrumExpand.previewNotes (rhythm対応)"
        [ test "rhythm = Nothing ならベタうち（コードの全音が同一スタートで鳴る）" <|
            \_ ->
                let
                    timeline =
                        Data.Timeline.fromSections { minBars = 1 } [ { id = 1, name = "A", lengthBars = 1, memo = "", key = Key.default, meter = Meter.default } ]

                    track =
                        { text = "C", instrument = Piano, muted = False, volume = 100, rhythm = Nothing }

                    notes =
                        StrumExpand.previewNotes True [] timeline track

                    distinctStarts =
                        notes |> List.map .start |> List.foldl (\s acc -> if List.member s acc then acc else s :: acc) []
                in
                Expect.equal 1 (List.length distinctStarts)
        , test "rhythm = Just \"8ビート\" ならストロークで複数ステップに分かれて鳴る" <|
            \_ ->
                let
                    timeline =
                        Data.Timeline.fromSections { minBars = 1 } [ { id = 1, name = "A", lengthBars = 1, memo = "", key = Key.default, meter = Meter.default } ]

                    track =
                        { text = "C", instrument = Piano, muted = False, volume = 100, rhythm = Just "8ビート" }

                    notes =
                        StrumExpand.previewNotes True [] timeline track

                    distinctStarts =
                        notes |> List.map .start |> List.foldl (\s acc -> if List.member s acc then acc else s :: acc) []
                in
                Expect.equal True (List.length distinctStarts > 1)
        ]
