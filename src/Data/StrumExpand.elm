module Data.StrumExpand exposing (expand, formFor, previewNotes, soundingPitches, voicingFromForm)

import Data.Chord
import Data.ChordTrack
import Data.GuitarForm as GuitarForm
import Data.Note exposing (Note)
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


{-| このコードをギターで鳴らすならどの Form を使うか決める。
`chord.voicing` が登録済みボイシングを指していればそれを優先し、フレットに置き換えられない
場合はピッチ集合から近いフォームを探す。ボイシング指定がなければ `guitarFormEnabled` の
true/false で「コード名から自動でフォームを探す」か「フォームを使わない」かを切り替える。
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
後から同じ offset で別の弦を選ぶこともできる。offsets は重複を除いた並び順を保ったリストにしておく。
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
ギターフォームが見つかればそのフォームの弦順（スラッシュベース反映済み）を使い、
見つからなければボイシングテーブルからのピッチ集合を昇順ソートしたものを返す。
-}
soundingPitches : Bool -> List Voicing -> Data.Chord.Chord -> List Int
soundingPitches guitarFormEnabled voicings chord =
    case formFor guitarFormEnabled voicings chord of
        Just form ->
            withSlashBass chord.bass (GuitarForm.toPitches form)

        Nothing ->
            Data.Chord.toPitchesWith voicings chord |> List.sort


{-| フォーム由来のピッチ列にスラッシュコードのベース音を反映する。
最低音がすでにベース音と同じピッチクラスならそのまま、違えば最低音より低いオクターブで
ベース音を先頭に追加する。
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


{-| コード進行を実際にストローク展開して鳴らす場合のノート列を計算する純関数。`resolvedChords` は呼び出し側が
`Data.ChordTrack.resolved` で求めて渡す。`Codec.Performance.chordEvents` / `previewNotes`（実際の再生・プレビュー）が
この関数を共有するので、鳴る音は常に一致する。

soundingPitches（ギターフォーム探索を伴う重い処理）はコード区間ごとに1回だけ事前計算し、
アルペジオ（StringIndex）のステップ番号（localArpeggioIndices）も strumStarts を一回走査するだけで
事前計算する。どちらも「小節内全イベントのたびに全体を走査し直す」 O(N²) を避けるため。
-}
expand :
    { startTicks : Int, endTicks : Int }
    -> Bool
    -> List Voicing
    -> Pattern
    -> List Data.ChordTrack.ResolvedChord
    -> List { pitch : Int, start : Int, duration : Int, velocity : Int }
expand cfg guitarFormEnabled voicings pattern resolvedChords =
    let
        resolvedChordsWithPitches =
            resolvedChords
                |> List.map (\rc -> ( rc, soundingPitches guitarFormEnabled voicings rc.chord ))

        chordAt ticks =
            resolvedChordsWithPitches
                |> List.filter (\( rc, _ ) -> ticks >= rc.startTicks && ticks < rc.startTicks + rc.durationTicks)
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

        chordEndAfter start =
            chordAt start
                |> Maybe.map (\( rc, _ ) -> rc.startTicks + rc.durationTicks)
                |> Maybe.withDefault (start + Data.Time.ticksPerSixteenth)

        eventEnd index start pick =
            case pick of
                StringIndex _ ->
                    chordEndAfter start

                AllStrings ->
                    case nextEventStart index of
                        Just next ->
                            next

                        Nothing ->
                            chordEndAfter start

        chordSegmentKey ticks =
            chordAt ticks |> Maybe.map (\( rc, _ ) -> rc.startTicks) |> Maybe.withDefault -1

        localArpeggioIndices : List Int
        localArpeggioIndices =
            strumStarts
                |> List.foldl
                    (\( start, _ ) ( maybeLastKey, counter, accRev ) ->
                        let
                            key =
                                chordSegmentKey start
                        in
                        if maybeLastKey == Just key then
                            ( Just key, counter + 1, counter :: accRev )

                        else
                            ( Just key, 1, 0 :: accRev )
                    )
                    ( Nothing, 0, [] )
                |> (\( _, _, accRev ) -> List.reverse accRev)

        notesForEvent index ( ( start, s ), localIndex ) =
            case chordAt start of
                Nothing ->
                    []

                Just ( _, pitches ) ->
                    let
                        ordered =
                            case s.pick of
                                AllStrings ->
                                    case s.direction of
                                        Down ->
                                            pitches

                                        Up ->
                                            List.reverse pitches

                                StringIndex _ ->
                                    if localIndex < List.length pitches then
                                        pitches |> List.drop localIndex |> List.take 1

                                    else
                                        []

                        dur =
                            Basics.max 1 (eventEnd index start s.pick - start)

                        velocity =
                            s.velocity
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
    in
    List.map2 Tuple.pair strumStarts localArpeggioIndices
        |> List.indexedMap notesForEvent
        |> List.concat


{-| コード進行を実際にMIDI化（「→ MIDIトラック化」）した場合に鳴るはずのノート列を計算する。
リズムパターン（ストローク）が設定されていれば `expand` で展開し、なければ各コード区間を
そのまま1本のロングノートにする。内部では `expand`（コードトラック展開の中核）と同じロジックを
共有しているので、プレビューと実際の再生・MIDI化の結果は常に一致する。
-}
previewNotes : Bool -> List Voicing -> Timeline -> Data.ChordTrack.ChordTrack -> List Note
previewNotes guitarFormEnabled voicings timeline track =
    let
        resolvedChords =
            Data.ChordTrack.resolved timeline track

        rawNotes =
            case track.rhythm |> Maybe.andThen Data.StrumPattern.byName of
                Just pattern ->
                    case ( List.map .startTicks resolvedChords |> List.minimum, resolvedChords |> List.map (\rc -> rc.startTicks + rc.durationTicks) |> List.maximum ) of
                        ( Just startTicks, Just endTicks ) ->
                            expand { startTicks = startTicks, endTicks = endTicks } guitarFormEnabled voicings pattern resolvedChords

                        _ ->
                            []

                Nothing ->
                    resolvedChords
                        |> List.concatMap
                            (\ev ->
                                soundingPitches guitarFormEnabled voicings ev.chord
                                    |> List.map (\p -> { pitch = p, start = ev.startTicks, duration = ev.durationTicks, velocity = 80 })
                            )
    in
    rawNotes
        |> List.indexedMap
            (\i n -> { id = i, pitch = n.pitch, start = n.start, duration = n.duration, velocity = n.velocity })
