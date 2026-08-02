module Data.Project exposing
    ( Project
    , addNote
    , addScrap
    , addSection
    , addTrack
    , demo
    , mapNotes
    , moveSection
    , removeNote
    , removeScrap
    , removeSection
    , removeTrack
    , sectionBounds
    , updateChordTrack
    , updateNote
    , updateScrap
    , updateSection
    , updateTrack
    )

import Data.ChordTrack exposing (ChordTrack)
import Data.Note exposing (Note)
import Data.ReferenceAudio exposing (ReferenceAudio)
import Data.Scrap exposing (Scrap)
import Data.Section exposing (Section)
import Data.Time exposing (ppq)
import Data.Track exposing (Instrument(..), Track, TrackKind(..))


type alias Project =
    { name : String
    , bpm : Int
    , tracks : List Track
    , chordTrack : ChordTrack
    , sections : List Section
    , scraps : List Scrap
    , referenceAudio : ReferenceAudio
    , nextId : Int
    }


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
    , bpm = 120
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
        [ { id = 2, name = "Aメロ", lengthBars = 4, memo = "" }
        , { id = 3, name = "サビ", lengthBars = 4, memo = "" }
        ]
    , scraps = []
    , referenceAudio = Data.ReferenceAudio.empty
    , nextId = 100
    }


updateChordTrack : (ChordTrack -> ChordTrack) -> Project -> Project
updateChordTrack f project =
    { project | chordTrack = f project.chordTrack }


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
        section =
            { id = project.nextId
            , name = "セクション " ++ String.fromInt (List.length project.sections + 1)
            , lengthBars = 4
            , memo = ""
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


sectionBounds : Int -> Project -> Maybe { startTicks : Int, endTicks : Int }
sectionBounds sectionId project =
    let
        step section ( acc, found ) =
            case found of
                Just _ ->
                    ( acc, found )

                Nothing ->
                    let
                        end =
                            acc + section.lengthBars * Data.Time.ticksPerBar
                    in
                    if section.id == sectionId then
                        ( end, Just { startTicks = acc, endTicks = end } )

                    else
                        ( end, Nothing )
    in
    project.sections
        |> List.foldl step ( 0, Nothing )
        |> Tuple.second


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
