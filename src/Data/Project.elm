module Data.Project exposing
    ( Project
    , addNote
    , addScrap
    , addSection
    , addTrack
    , addVoicing
    , demo
    , insertBars
    , mapAllNotes
    , mapNoteTrackNotes
    , mapNotes
    , moveSection
    , moveSectionToIndex
    , removeBars
    , removeNote
    , removeScrap
    , removeSection
    , removeTrack
    , removeVoicing
    , sectionBounds
    , timeline
    , updateChordTrack
    , updateNote
    , updateScrap
    , updateSection
    , updateTrack
    , updateVoicing
    )

import Data.ChordTrack exposing (ChordTrack)
import Data.Key
import Data.Meter
import Data.Note exposing (Note)
import Data.ReferenceAudio exposing (ReferenceAudio)
import Data.Scrap exposing (Scrap)
import Data.Section exposing (Section)
import Data.Time exposing (ppq)
import Data.Timeline exposing (Timeline)
import Data.Track exposing (Instrument(..), Track, TrackKind(..))
import Data.Voicing exposing (Voicing)
import Dict


type alias Project =
    { name : String
    , bpm : Float
    , tracks : List Track
    , chordTrack : ChordTrack
    , sections : List Section
    , scraps : List Scrap
    , referenceAudio : ReferenceAudio
    , nextId : Int
    , memo : String
    , voicings : List Voicing
    , voicingEnabled : Bool
    , guitarFormEnabled : Bool
    }


{-| 曲全体の小節表。ノートの終端・セクション合計・末尾余白のうち一番長いところまで伸ばす
（グリッドの外側に音を置けず内容が伸びない、という自己強化的な壁を作らないため）。
参考オーディオの長さ（durationMs - offsetMs の実効長）も含めて計算する。
-}
timeline : Project -> Timeline
timeline project =
    let
        eventsEndTicks =
            (project.tracks |> List.concatMap trackNotes)
                |> List.foldl (\n acc -> Basics.max acc (n.start + n.duration)) 0

        sectionBarTotal =
            List.foldl (\s acc -> acc + s.lengthBars) 0 project.sections

        fallbackMeter =
            List.reverse project.sections |> List.head |> Maybe.map .meter |> Maybe.withDefault Data.Meter.default

        eventsEndBars =
            (eventsEndTicks + Data.Meter.ticksPerBar fallbackMeter - 1) // Data.Meter.ticksPerBar fallbackMeter

        chordBars =
            Data.ChordTrack.barCount project.chordTrack

        refAudioBars =
            case project.referenceAudio.durationMs of
                Just ms ->
                    let
                        effectiveMs =
                            toFloat ms - toFloat project.referenceAudio.offsetMs

                        refEndTicks =
                            Basics.max 0 (effectiveMs / 1000 * project.bpm / 60 * toFloat ppq)
                    in
                    ceiling (refEndTicks / toFloat (Data.Meter.ticksPerBar fallbackMeter))

                Nothing ->
                    0

        minBars =
            List.maximum
                [ 16
                , sectionBarTotal + Data.Timeline.tailPaddingBars
                , eventsEndBars + Data.Timeline.tailPaddingBars
                , chordBars
                , refAudioBars
                ]
                |> Maybe.withDefault 16
    in
    Data.Timeline.fromSections { minBars = minBars } project.sections


trackNotes : Track -> List Note
trackNotes track =
    case track.kind of
        NoteTrack notes ->
            notes

        DrumTrack notes ->
            notes


demo : Project
demo =
    let
        eighth =
            ppq // 2

        note index pitch =
            { id = index
            , pitch = pitch
            , start = index * eighth
            , duration = eighth
            , velocity = 100
            }

        melody =
            List.indexedMap note
                [ 60, 64, 67, 72, 67, 64, 60, 55, 60, 64, 67, 72, 76, 72, 67, 64 ]
    in
    { name = "デモ"
    , bpm = 120.0
    , tracks =
        [ { id = 1
          , name = "仮メロ"
          , instrument = SynthLead
          , muted = False
          , volume = 100
          , kind = NoteTrack melody
          }
        ]
    , chordTrack =
        { text = "C | G | Am | F"
        , instrument = Piano
        , muted = False
        , volume = 100
        }
    , sections =
        [ { id = 2, name = "Aメロ", lengthBars = 4, memo = "", key = Data.Key.default, meter = Data.Meter.default }
        , { id = 3, name = "サビ", lengthBars = 4, memo = "", key = Data.Key.default, meter = Data.Meter.default }
        ]
    , scraps = []
    , referenceAudio = Data.ReferenceAudio.empty
    , nextId = 100
    , memo = ""
    , voicings = []
    , voicingEnabled = True
    , guitarFormEnabled = True
    }


updateChordTrack : (ChordTrack -> ChordTrack) -> Project -> Project
updateChordTrack f project =
    { project | chordTrack = f project.chordTrack }


addVoicing : String -> Project -> Project
addVoicing name project =
    { project | voicings = project.voicings ++ [ Data.Voicing.empty name ] }


removeVoicing : Int -> Project -> Project
removeVoicing index project =
    { project | voicings = List.take index project.voicings ++ List.drop (index + 1) project.voicings }


updateVoicing : Int -> (Voicing -> Voicing) -> Project -> Project
updateVoicing index f project =
    { project
        | voicings =
            List.indexedMap
                (\i v ->
                    if i == index then
                        f v

                    else
                        v
                )
                project.voicings
    }


addScrap : List Note -> Project -> Project
addScrap notes project =
    let
        scrap =
            { id = project.nextId
            , name = "断片 " ++ String.fromInt (List.length project.scraps + 1)
            , notes = notes
            }
    in
    { project | scraps = project.scraps ++ [ scrap ], nextId = project.nextId + 1 }


removeScrap : Int -> Project -> Project
removeScrap scrapId project =
    { project | scraps = List.filter (\s -> s.id /= scrapId) project.scraps }


updateScrap : Int -> (Scrap -> Scrap) -> Project -> Project
updateScrap scrapId f project =
    { project
        | scraps =
            List.map
                (\s ->
                    if s.id == scrapId then
                        f s

                    else
                        s
                )
                project.scraps
    }


addSection : Project -> Project
addSection project =
    let
        lastMeter =
            List.reverse project.sections |> List.head |> Maybe.map .meter |> Maybe.withDefault Data.Meter.default

        lastKey =
            List.reverse project.sections |> List.head |> Maybe.map .key |> Maybe.withDefault Data.Key.default

        section =
            { id = project.nextId
            , name = "セクション " ++ String.fromInt (List.length project.sections + 1)
            , lengthBars = 4
            , memo = ""
            , key = lastKey
            , meter = lastMeter
            }
    in
    { project | sections = project.sections ++ [ section ], nextId = project.nextId + 1 }


removeSection : Int -> Project -> Project
removeSection sectionId project =
    { project | sections = List.filter (\s -> s.id /= sectionId) project.sections }


updateSection : Int -> (Section -> Section) -> Project -> Project
updateSection sectionId f project =
    { project
        | sections =
            List.map
                (\s ->
                    if s.id == sectionId then
                        f s

                    else
                        s
                )
                project.sections
    }


moveSection : Int -> Int -> Project -> Project
moveSection sectionId delta project =
    let
        maybeIndex =
            project.sections
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, s ) -> s.id == sectionId)
                |> List.head
                |> Maybe.map Tuple.first
    in
    case maybeIndex of
        Just i ->
            let
                j =
                    i + delta
            in
            if j < 0 || j >= List.length project.sections then
                project

            else
                { project | sections = swap i j project.sections }

        Nothing ->
            project


{-| セクションを任意の絶対 index に移動する。`moveSection`（相対 delta での swap）と違い、ドラッグで任意の
位置へ一発で移動させるのに使う。targetIndex は自動的に 0..(セクション数-1) にクランプされる。
-}
moveSectionToIndex : Int -> Int -> Project -> Project
moveSectionToIndex sectionId targetIndex project =
    let
        moving =
            project.sections |> List.filter (\s -> s.id == sectionId) |> List.head

        rest =
            project.sections |> List.filter (\s -> s.id /= sectionId)

        clampedIndex =
            clamp 0 (List.length rest) targetIndex
    in
    case moving of
        Just s ->
            { project | sections = List.take clampedIndex rest ++ s :: List.drop clampedIndex rest }

        Nothing ->
            project


swap : Int -> Int -> List a -> List a
swap i j list =
    case ( List.drop i list |> List.head, List.drop j list |> List.head ) of
        ( Just a, Just b ) ->
            List.indexedMap
                (\k x ->
                    if k == i then
                        b

                    else if k == j then
                        a

                    else
                        x
                )
                list

        _ ->
            list


{-| セクション id から tick 範囲を引く。小節計算は Data.Timeline に集約されているのでここは薄いラッパ。
-}
sectionBounds : Int -> Project -> Maybe { startTicks : Int, endTicks : Int }
sectionBounds sectionId project =
    Data.Timeline.sectionBounds sectionId (timeline project)


mapTrackNotes : Int -> (List Note -> List Note) -> Project -> Project
mapTrackNotes trackId f project =
    { project
        | tracks =
            List.map
                (\track ->
                    if track.id == trackId then
                        { track | kind = mapKind f track.kind }

                    else
                        track
                )
                project.tracks
    }


mapKind : (List Note -> List Note) -> TrackKind -> TrackKind
mapKind f kind =
    case kind of
        NoteTrack notes ->
            NoteTrack (f notes)

        DrumTrack notes ->
            DrumTrack (f notes)


{-| 全トラック（DrumTrack も含む）のノート列に f を適用する。時間軸上のシフト（小節の挿入・削除）など、
ピッチには依存しない操作で使う。
-}
mapAllNotes : (List Note -> List Note) -> Project -> Project
mapAllNotes f project =
    { project | tracks = List.map (\track -> { track | kind = mapKind f track.kind }) project.tracks }


{-| NoteTrack のノート列にだけ f を適用する（DrumTrack は除外）。ドラムの pitch はドラムマップであり
移調してはいけないので、曲全体の移調に使う。
-}
mapNoteTrackNotes : (List Note -> List Note) -> Project -> Project
mapNoteTrackNotes f project =
    { project
        | tracks =
            List.map
                (\track ->
                    case track.kind of
                        NoteTrack notes ->
                            { track | kind = NoteTrack (f notes) }

                        DrumTrack _ ->
                            track
                )
                project.tracks
    }


addNote : Int -> Note -> Project -> Project
addNote trackId note project =
    mapTrackNotes trackId (\notes -> note :: notes) { project | nextId = project.nextId + 1 }


updateNote : Int -> Int -> (Note -> Note) -> Project -> Project
updateNote trackId noteId f =
    mapTrackNotes trackId
        (List.map
            (\n ->
                if n.id == noteId then
                    f n

                else
                    n
            )
        )


removeNote : Int -> Int -> Project -> Project
removeNote trackId noteId =
    mapTrackNotes trackId (List.filter (\n -> n.id /= noteId))


addTrack : Project -> Project
addTrack project =
    let
        track =
            { id = project.nextId
            , name = "トラック " ++ String.fromInt (List.length project.tracks + 1)
            , instrument = Piano
            , muted = False
            , volume = 100
            , kind = NoteTrack []
            }
    in
    { project | tracks = project.tracks ++ [ track ], nextId = project.nextId + 1 }


removeTrack : Int -> Project -> Project
removeTrack trackId project =
    { project | tracks = List.filter (\t -> t.id /= trackId) project.tracks }


updateTrack : Int -> (Track -> Track) -> Project -> Project
updateTrack trackId f project =
    { project
        | tracks =
            List.map
                (\t ->
                    if t.id == trackId then
                        f t

                    else
                        t
                )
                project.tracks
    }


mapNotes : Int -> (List Note -> List Note) -> Project -> Project
mapNotes =
    mapTrackNotes


ticksToMs : Float -> Int -> Float
ticksToMs bpm ticks =
    toFloat ticks / toFloat ppq * 60 / bpm * 1000


splitChordBars : String -> List String
splitChordBars text =
    String.split "|" text


joinChordBars : List String -> String
joinChordBars bars =
    String.join "|" bars


{-| 指定小節位置に空の小節を count 個挿入する。テキストが短くその位置まで小節がなければ
空小節で埋めてから挿入する。
-}
insertChordBars : Int -> Int -> String -> String
insertChordBars atBar count text =
    let
        bars =
            splitChordBars text

        padded =
            bars ++ List.repeat (Basics.max 0 (atBar - List.length bars)) ""

        before =
            List.take atBar padded

        after =
            List.drop atBar padded
    in
    joinChordBars (before ++ List.repeat count "" ++ after)


{-| 指定小節位置から count 個分の小節を削除する。
-}
removeChordBars : Int -> Int -> String -> String
removeChordBars fromBar count text =
    let
        bars =
            splitChordBars text

        before =
            List.take fromBar bars

        after =
            List.drop (fromBar + count) bars
    in
    joinChordBars (before ++ after)


{-| `beforeBar`（0-based小節番号）の前に無音を count 小節挿入する。挿入する小節の拍子はその位置の拍子
（末尾を超える場合は最後のセクションの拍子）を使う。tick 範囲の算出は必ず `Data.Timeline.barAt`
経由で行う（`ticksPerBar` の一律掛け算は拍子混在時に壊れるため使わない）。
-}
insertBars : { beforeBar : Int, count : Int } -> Project -> Project
insertBars { beforeBar, count } project =
    if count <= 0 then
        project

    else
        let
            tl =
                timeline project

            bar =
                Data.Timeline.barAt beforeBar tl

            insertionTicks =
                bar |> Maybe.map .startTicks |> Maybe.withDefault (Data.Timeline.totalTicks tl)

            fallbackMeter =
                List.reverse project.sections |> List.head |> Maybe.map .meter |> Maybe.withDefault Data.Meter.default

            meter =
                bar |> Maybe.map .meter |> Maybe.withDefault fallbackMeter

            deltaTicks =
                count * Data.Meter.ticksPerBar meter

            shiftedProject =
                mapAllNotes
                    (List.map
                        (\n ->
                            if n.start >= insertionTicks then
                                { n | start = n.start + deltaTicks }

                            else
                                n
                        )
                    )
                    project

            newSections =
                case bar |> Maybe.andThen .sectionId of
                    Just sid ->
                        List.map
                            (\s ->
                                if s.id == sid then
                                    { s | lengthBars = s.lengthBars + count }

                                else
                                    s
                            )
                            shiftedProject.sections

                    Nothing ->
                        shiftedProject.sections

            ct =
                shiftedProject.chordTrack

            newChordText =
                insertChordBars beforeBar count ct.text

            ra =
                shiftedProject.referenceAudio

            deltaMs =
                round (ticksToMs project.bpm deltaTicks)
        in
        { shiftedProject
            | sections = newSections
            , chordTrack = { ct | text = newChordText }
            , referenceAudio = { ra | offsetMs = ra.offsetMs - deltaMs }
        }


{-| `fromBar`（0-based小節番号）から count 小節分を削除する。範囲内に開始するノートは削除、
範囲を跨ぐノートは短縮、範囲後のノートは削除分だけ前へシフトする。セクションの lengthBars は
実際にそのセクションに属していた削除分だけ減らし、0以下になったセクションは削除する。
-}
removeBars : { fromBar : Int, count : Int } -> Project -> Project
removeBars { fromBar, count } project =
    if count <= 0 then
        project

    else
        let
            tl =
                timeline project

            rangeBars =
                List.range fromBar (fromBar + count - 1)
                    |> List.filterMap (\i -> Data.Timeline.barAt i tl)

            startTicks =
                rangeBars |> List.head |> Maybe.map .startTicks |> Maybe.withDefault 0

            endTicks =
                rangeBars
                    |> List.reverse
                    |> List.head
                    |> Maybe.map (\b -> b.startTicks + b.lengthTicks)
                    |> Maybe.withDefault startTicks

            deltaTicks =
                endTicks - startTicks

            removedPerSection =
                rangeBars
                    |> List.filterMap .sectionId
                    |> List.foldl (\sid acc -> Dict.update sid (\c -> Just (Maybe.withDefault 0 c + 1)) acc) Dict.empty

            adjustNote n =
                if n.start >= endTicks then
                    Just { n | start = n.start - deltaTicks }

                else if n.start >= startTicks then
                    Nothing

                else if n.start + n.duration > startTicks then
                    Just { n | duration = startTicks - n.start }

                else
                    Just n

            trimmedProject =
                mapAllNotes (List.filterMap adjustNote) project

            newSections =
                trimmedProject.sections
                    |> List.filterMap
                        (\s ->
                            let
                                removed =
                                    Dict.get s.id removedPerSection |> Maybe.withDefault 0

                                newLen =
                                    s.lengthBars - removed
                            in
                            if newLen > 0 then
                                Just { s | lengthBars = newLen }

                            else
                                Nothing
                        )

            ct =
                trimmedProject.chordTrack

            newChordText =
                removeChordBars fromBar count ct.text

            ra =
                trimmedProject.referenceAudio

            deltaMs =
                round (ticksToMs project.bpm deltaTicks)
        in
        { trimmedProject
            | sections = newSections
            , chordTrack = { ct | text = newChordText }
            , referenceAudio = { ra | offsetMs = ra.offsetMs + deltaMs }
        }
