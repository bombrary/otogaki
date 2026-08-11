module Data.StrumExpand exposing (apply, formFor, previewNotes, soundingPitches, voicingFromForm)

import Data.Chord
import Data.ChordTrack
import Data.GuitarForm as GuitarForm
import Data.Note exposing (Note)
import Data.Project exposing (Project)
import Data.StrumPattern exposing (Direction(..), Pattern, Pick(..))
import Data.Time
import Data.Timeline exposing (Timeline)
import Data.Voicing exposing (Voicing)
import Set


{-| 弦ごとのストロークタイミングのずれ幅（1弦あたり）。PPQ=480 なら 1/60 拍程度。
-}
stringDelayTicks : Int
stringDelayTicks =
    8


{-| このコードをギターで鳴らすならどの Form を使うか。
`chord.voicing` が登録済みボイシングを指していれば、`guitarFormEnabled` に関係なく常にそれを使う（保存された運指があればそのとおりの弦で、
なければ bestForm が推定した弦で）。登録ボイシングがないコードは、`guitarFormEnabled` が True なら forChord の固定表を試し、それも引けなければ
`bestForm` でコード理論値を実在の弦・フレットに割り当てる（quality を問わない全探索なので forChord の固定表にない dim/sus/m7-5
等でも実運指が出せる）。`guitarFormEnabled` が False なら常に `Nothing`（呼び出し側が toPitchesWith の理論値にフォールバックする）。
ストローク音生成、普段の再生、指板ハイライト表示の全てがこの優先順を共有する。
-}
formFor : Bool -> List Voicing -> Data.Chord.Chord -> Maybe GuitarForm.Form
formFor guitarFormEnabled voicings chord =
    case chord.voicing |> Maybe.andThen (\name -> Data.Voicing.findByName name voicings) of
        Just voicing ->
            let
                rootPitch =
                    Data.Voicing.anchorPitch + modBy 12 chord.root
            in
            case GuitarForm.formFromPicks rootPitch voicing.offsets voicing.stringPicks of
                Just form ->
                    Just form

                Nothing ->
                    GuitarForm.bestForm (Data.Voicing.pitchesFor chord.root voicing)

        Nothing ->
            if guitarFormEnabled then
                case GuitarForm.forChord chord of
                    Just f ->
                        Just f

                    Nothing ->
                        GuitarForm.bestForm (Data.Chord.toPitchesWith voicings chord)

            else
                Nothing


{-| Form を Voicing に変換する。全弦の (offset, 弦インデックス) を stringPicks に入れるので、
`formFor` の `formFromPicks` 経路（coversAll 成立）で選んだフォームを弦単位で完全に再現できる。
`rootPitch` は呼び出し側が `Data.Voicing.anchorPitch + modBy 12 chord.root` で求める。
`View.ChordEditor.formToFretboardData` の picks 計算と同じロジック。
-}
voicingFromForm : Int -> GuitarForm.Form -> String -> Voicing
voicingFromForm rootPitch form name =
    let
        picks =
            List.map2 Tuple.pair GuitarForm.openStrings form.frets
                |> List.indexedMap (\i ( openPitch, maybeFret ) -> Maybe.map (\fret -> ( openPitch + fret - rootPitch, i )) maybeFret)
                |> List.filterMap identity
                |> Set.fromList

        offsets =
            picks
                |> Set.toList
                |> List.map Tuple.first
                |> List.foldl (\o acc -> if List.member o acc then acc else o :: acc) []
                |> List.reverse
    in
    { name = name, offsets = offsets, stringPicks = picks }


{-| コードの発音候補を「弦順（低音から高音）のピッチ列」として取り出す。
Form が引ければ実際の弦・フレットから、引けなければ toPitchesWith の理論値をそのまま昇順に並べたものを
「擬似弦」として使う。
-}
soundingPitches : Bool -> List Voicing -> Data.Chord.Chord -> List Int
soundingPitches guitarFormEnabled voicings chord =
    case formFor guitarFormEnabled voicings chord of
        Just form ->
            withSlashBass chord.bass (GuitarForm.toPitches form)

        Nothing ->
            Data.Chord.toPitchesWith voicings chord |> List.sort


{-| フォーム由来のピッチ列にスラッシュコードのベース音を反映する。
GuitarForm.forChord / 登録ボイシングの弦割当は root と quality だけで決まり、on-chord のベース指定 (`chord.bass`) を無視するため、
ここで別途ベース音を最低音として先頭に足す。フォームの最低音が既に同じピッチクラスならそのまま返す（例: C/C）。
-}
withSlashBass : Maybe Int -> List Int -> List Int
withSlashBass maybeBass pitches =
    case ( maybeBass, List.minimum pitches ) of
        ( Just bass, Just lowest ) ->
            let
                bassPc =
                    modBy 12 bass

                candidate =
                    Data.Voicing.anchorPitch + bassPc
            in
            if modBy 12 lowest == bassPc then
                pitches

            else if candidate < lowest then
                candidate :: pitches

            else
                (candidate - 12) :: pitches

        _ ->
            pitches


{-| コード進行を実際にMIDI化（「→ MIDIトラック化」）した場合に鳴るはずのノート列を計算する。
ピアノロールのMIDIプレビュー表示で使う。soundingPitches と同じギターフォーム判定を通すので、実際の再生（Codec.Performance）と一致する。
id はプレビュー専用の連番（実際のPianoRollのノートとは不一致する）。
-}
previewNotes : Bool -> List Voicing -> Timeline -> Data.ChordTrack.ChordTrack -> List Note
previewNotes guitarFormEnabled voicings timeline track =
    Data.ChordTrack.resolved timeline track
        |> List.concatMap
            (\ev ->
                soundingPitches guitarFormEnabled voicings ev.chord
                    |> List.map (\p -> { pitch = p, start = ev.startTicks, duration = ev.durationTicks })
            )
        |> List.indexedMap
            (\i n -> { id = i, pitch = n.pitch, start = n.start, duration = n.duration, velocity = 80 })


{-| 選択範囲内の既存ノートを置換し、コード進行に沿ったストロークを展開する。
`Data.DrumPattern.apply` と同じ形の Project 直接操作 API。
-}
apply : { trackId : Int, startTicks : Int, endTicks : Int } -> Bool -> List Voicing -> Pattern -> Project -> Project
apply cfg guitarFormEnabled voicings pattern project =
    let
        timeline =
            Data.Project.timeline project

        resolvedChords =
            Data.ChordTrack.resolved timeline project.chordTrack

        chordAt ticks =
            resolvedChords
                |> List.filter (\rc -> ticks >= rc.startTicks && ticks < rc.startTicks + rc.durationTicks)
                |> List.head

        bars =
            Basics.max 1 ((cfg.endTicks - cfg.startTicks) // Data.Time.ticksPerBar)

        strumStarts =
            List.range 0 (bars - 1)
                |> List.concatMap
                    (\bar ->
                        List.map
                            (\s -> ( cfg.startTicks + bar * Data.Time.ticksPerBar + s.step * Data.Time.ticksPerSixteenth, s ))
                            pattern.strums
                    )
                |> List.sortBy Tuple.first

        nextEventStart index =
            strumStarts |> List.drop (index + 1) |> List.head |> Maybe.map Tuple.first

        eventEnd index start =
            case nextEventStart index of
                Just next ->
                    next

                Nothing ->
                    chordAt start
                        |> Maybe.map (\rc -> rc.startTicks + rc.durationTicks)
                        |> Maybe.withDefault (start + Data.Time.ticksPerSixteenth)

        notesForEvent index ( start, s ) =
            case chordAt start of
                Nothing ->
                    []

                Just rc ->
                    let
                        pitches =
                            soundingPitches guitarFormEnabled voicings rc.chord

                        ordered =
                            case s.pick of
                                AllStrings ->
                                    case s.direction of
                                        Down ->
                                            pitches

                                        Up ->
                                            pitches |> List.drop 2 |> List.reverse

                                StringIndex i ->
                                    case List.length pitches of
                                        0 ->
                                            []

                                        n ->
                                            pitches |> List.drop (modBy n i) |> List.take 1

                        dur =
                            Basics.max 1 (eventEnd index start - start)

                        velocity =
                            case s.direction of
                                Down ->
                                    s.velocity

                                Up ->
                                    Basics.max 1 (s.velocity - 20)
                    in
                    ordered
                        |> List.indexedMap
                            (\stringIndex pitch ->
                                { pitch = pitch
                                , start = start + stringIndex * stringDelayTicks
                                , duration = Basics.max 1 (dur - stringIndex * stringDelayTicks)
                                , velocity = velocity
                                }
                            )

        allNotesNoId =
            strumStarts |> List.indexedMap notesForEvent |> List.concat

        ( newNotesRev, finalNextId ) =
            List.foldl
                (\note ( acc, nid ) ->
                    ( { id = nid, pitch = note.pitch, start = note.start, duration = note.duration, velocity = note.velocity } :: acc
                    , nid + 1
                    )
                )
                ( [], project.nextId )
                allNotesNoId

        cleaned =
            Data.Project.mapNotes cfg.trackId
                (\existing ->
                    List.filter (\n -> n.start < cfg.startTicks || n.start >= cfg.endTicks) existing
                        ++ List.reverse newNotesRev
                )
                project
    in
    { cleaned | nextId = finalNextId }
