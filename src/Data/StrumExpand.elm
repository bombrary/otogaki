module Data.StrumExpand exposing (apply, formFor, soundingPitches)

import Data.Chord
import Data.ChordTrack
import Data.GuitarForm as GuitarForm
import Data.Project exposing (Project)
import Data.StrumPattern exposing (Direction(..), Pattern)
import Data.Time
import Data.Voicing exposing (Voicing)


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


{-| コードの発音候補を「弦順（低音から高音）のピッチ列」として取り出す。
Form が引ければ実際の弦・フレットから、引けなければ toPitchesWith の理論値をそのまま昇順に並べたものを
「擬似弦」として使う。
-}
soundingPitches : Bool -> List Voicing -> Data.Chord.Chord -> List Int
soundingPitches guitarFormEnabled voicings chord =
    case formFor guitarFormEnabled voicings chord of
        Just form ->
            GuitarForm.toPitches form

        Nothing ->
            Data.Chord.toPitchesWith voicings chord |> List.sort


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
                            case s.direction of
                                Down ->
                                    pitches

                                Up ->
                                    List.reverse pitches |> List.drop 2

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
