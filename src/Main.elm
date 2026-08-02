module Main exposing (main)

import Array exposing (Array)
import AudioMsg exposing (AudioEvent(..))
import Browser
import Browser.Events
import Codec.Performance as Performance
import Codec.ProjectJson as ProjectJson
import Data.Chord
import Data.Chord.Detect
import Data.ChordTrack
import Data.DrumPattern
import Data.Note
import Data.Project exposing (Project)
import Data.Time
import Data.Track exposing (TrackKind(..))
import Dict exposing (Dict)
import File
import File.Download
import File.Select
import Html exposing (Html, button, div, h1, input, label, span, text)
import Html.Attributes exposing (disabled, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Midi.Encode
import Ports
import Set exposing (Set)
import Task
import View.Arrange as Arrange
import View.ChordEditor as ChordEditor
import View.DrumEditor as DrumEditor
import View.Keyboard as Keyboard
import View.PianoRoll as PianoRoll
import View.RefAudio as RefAudio
import View.ScrapShelf as ScrapShelf
import View.SectionBar as SectionBar


type PlayState
    = Idle
    | Playing


type DragMode
    = MoveNote
    | ResizeRight


type alias DragInfo =
    { anchorId : Int
    , mode : DragMode
    , startClientX : Float
    , startClientY : Float
    , origNotes : List Data.Note.Note
    , lastPreviewPitch : Int
    }


type DragState
    = NoDrag
    | Dragging DragInfo


type alias RubberBand =
    { originX : Float
    , originY : Float
    , startClientX : Float
    , startClientY : Float
    , curX : Float
    , curY : Float
    }


type alias ClientPos =
    { clientX : Float, clientY : Float }


type alias KeyEvent =
    { key : String
    , ctrl : Bool
    , meta : Bool
    , shift : Bool
    , repeat : Bool
    , targetTag : String
    }


type alias Model =
    { project : Project
    , playState : PlayState
    , playheadTicks : Int
    , selectedTrackId : Int
    , dragState : DragState
    , instrumentLoad : Dict String String
    , selectedSectionId : Maybe Int
    , bpmInput : String
    , selectedNoteIds : Set Int
    , clipboard : List Data.Note.Note
    , rubberBand : Maybe RubberBand
    , showKeyboard : Bool
    , drumFillBars : Int
    , refOffsetInput : String
    , refLoaded : Bool
    , refPeaks : Array Float
    , refPeakDt : Float
    , drumViewRoll : Bool
    , loopMode : LoopMode
    , undoStack : List Project
    , redoStack : List Project
    , editBurst : Bool
    }


type LoopMode
    = NoLoop
    | LoopSong
    | LoopSection


type Msg
    = ClickedPlay
    | ChangedLoopMode String
    | ClickedStop
    | ClickedUndo
    | ClickedRedo
    | ChangedChordVolume String
    | ChangedBpm String
    | BlurredBpm
    | GotAudio AudioEvent
    | PressedEmptyCell { offsetX : Float, offsetY : Float, clientX : Float, clientY : Float, shift : Bool, seekMod : Bool }
    | PressedNote Int Bool { clientX : Float, clientY : Float, shift : Bool }
    | DoubleClickedNote Int
    | RightClickedNote Int
    | DraggedTo ClientPos
    | ReleasedDrag
    | ClickedRuler Float
    | PressedPianoKey Int
    | GotKey KeyEvent
    | SelectedTrack Int
    | ClickedAddTrack
    | ClickedRemoveTrack Int
    | ToggledMute Int
    | ChangedInstrument Int String
    | ChangedVolume Int String
    | ClickedExport
    | ClickedImport
    | GotImportFile File.File
    | GotImportContent String
    | ChangedChordText String
    | ToggledChordMute
    | ClickedConvertChords
    | SelectedSection Int
    | ClickedAddSection
    | ClickedRemoveSection Int
    | ChangedSectionName Int String
    | ChangedSectionBars Int String
    | ChangedSectionMemo Int String
    | MovedSection Int Int
    | ToggledDrumCell Int Int
    | AppliedDrumPreset String
    | ChangedDrumFillBars String
    | ClickedExportMidi
    | ToggledKeyboard
    | ClickedAddScrap
    | ClickedPlaceScrap Int
    | ClickedRemoveScrap Int
    | ChangedScrapName Int String
    | ChangedTrackName Int String
    | ChangedRefOffset String
    | BlurredRefOffset
    | ChangedRefVolume String
    | ToggledRefMute
    | ToggledDrumView


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        project =
            flags
                |> Decode.decodeValue ProjectJson.decoder
                |> Result.withDefault Data.Project.demo
    in
    ( { project = project
      , playState = Idle
      , playheadTicks = 0
      , selectedTrackId = firstTrackId project
      , dragState = NoDrag
      , instrumentLoad = Dict.empty
      , selectedSectionId = Nothing
      , bpmInput = String.fromInt project.bpm
      , selectedNoteIds = Set.empty
      , clipboard = []
      , rubberBand = Nothing
      , showKeyboard = False
      , drumFillBars = 4
      , refOffsetInput = String.fromInt project.referenceAudio.offsetMs
      , refLoaded = False
      , refPeaks = Array.empty
      , refPeakDt = 0.02
      , drumViewRoll = False
      , loopMode = NoLoop
      , undoStack = []
      , redoStack = []
      , editBurst = False
      }
    , Cmd.none
    )


snapRound : Int -> Int
snapRound ticks =
    Basics.round (toFloat ticks / toFloat Data.Time.ticksPerSixteenth) * Data.Time.ticksPerSixteenth


snapFloor : Int -> Int
snapFloor ticks =
    (ticks // Data.Time.ticksPerSixteenth) * Data.Time.ticksPerSixteenth


notesOf : Data.Track.TrackKind -> List Data.Note.Note
notesOf kind =
    case kind of
        NoteTrack notes ->
            notes

        DrumTrack notes ->
            notes


trackNotes : Model -> List Data.Note.Note
trackNotes model =
    model.project.tracks
        |> List.filter (\t -> t.id == model.selectedTrackId)
        |> List.head
        |> Maybe.map (\t -> notesOf t.kind)
        |> Maybe.withDefault []


selectionNotes : Model -> List Data.Note.Note
selectionNotes model =
    trackNotes model
        |> List.filter (\n -> Set.member n.id model.selectedNoteIds)


findNote : Model -> Int -> Maybe Data.Note.Note
findNote model noteId =
    trackNotes model
        |> List.filter (\n -> n.id == noteId)
        |> List.head


selectedInstrumentName : Model -> String
selectedInstrumentName model =
    model.project.tracks
        |> List.filter (\t -> t.id == model.selectedTrackId)
        |> List.head
        |> Maybe.map (\t -> Data.Track.instrumentToString t.instrument)
        |> Maybe.withDefault "synthLead"


selectedTrackKind : Model -> Maybe Data.Track.TrackKind
selectedTrackKind model =
    model.project.tracks
        |> List.filter (\t -> t.id == model.selectedTrackId)
        |> List.head
        |> Maybe.map .kind


convertTrackKind : Data.Track.Instrument -> Data.Track.TrackKind -> Data.Track.TrackKind
convertTrackKind inst kind =
    if inst == Data.Track.DrumKit then
        DrumTrack (notesOf kind)

    else
        NoteTrack (notesOf kind)


markLoading : List String -> Dict String String -> Dict String String
markLoading names dict =
    List.foldl
        (\name acc ->
            if name == "synthLead" || Dict.get name acc == Just "ready" then
                acc

            else
                Dict.insert name "loading" acc
        )
        dict
        names


firstTrackId : Project -> Int
firstTrackId project =
    project.tracks
        |> List.head
        |> Maybe.map .id
        |> Maybe.withDefault 1


songEndTicks : Project -> Int
songEndTicks project =
    let
        sectionsEnd =
            List.foldl (\s acc -> acc + s.lengthBars * Data.Time.ticksPerBar) 0 project.sections

        eventsEnd =
            Performance.toEvents project
                |> List.foldl (\e acc -> Basics.max acc (e.ticks + e.durationTicks)) 0
    in
    Basics.max Data.Time.ticksPerBar (Basics.max sectionsEnd eventsEnd)


totalBarsFor : Project -> Int
totalBarsFor project =
    Basics.max 16 ((songEndTicks project + Data.Time.ticksPerBar - 1) // Data.Time.ticksPerBar)


sectionSpans : Project -> List PianoRoll.SectionSpan
sectionSpans project =
    project.sections
        |> List.foldl
            (\s ( acc, startBar ) ->
                ( acc ++ [ { name = s.name, startBar = startBar, lengthBars = s.lengthBars } ]
                , startBar + s.lengthBars
                )
            )
            ( [], 0 )
        |> Tuple.first


isReleasedDrag : Msg -> Bool
isReleasedDrag msg =
    case msg of
        ReleasedDrag ->
            True

        _ ->
            False


{-| 連続して届く編集（タイピング・スライダー・ドラッグ）はアンドゥ履歴上 1 ステップにまとめる。 -}
isCoalescing : Msg -> Bool
isCoalescing msg =
    case msg of
        ChangedChordText _ ->
            True

        ChangedBpm _ ->
            True

        ChangedSectionName _ _ ->
            True

        ChangedSectionBars _ _ ->
            True

        ChangedSectionMemo _ _ ->
            True

        ChangedTrackName _ _ ->
            True

        ChangedVolume _ _ ->
            True

        ChangedScrapName _ _ ->
            True

        ChangedRefOffset _ ->
            True

        ChangedRefVolume _ ->
            True

        ChangedChordVolume _ ->
            True

        DraggedTo _ ->
            True

        PressedEmptyCell _ ->
            True

        _ ->
            False


isUndoRedo : Msg -> Bool
isUndoRedo msg =
    case msg of
        ClickedUndo ->
            True

        ClickedRedo ->
            True

        GotKey k ->
            (k.ctrl || k.meta) && (k.key == "z" || k.key == "Z" || k.key == "y")

        _ ->
            False


{-| 履歴から project を戻すときに、入力欄や選択状態も追従させる。 -}
restoreProject : Project -> Model -> Model
restoreProject project model =
    { model
        | project = project
        , selectedNoteIds = Set.empty
        , bpmInput = String.fromInt project.bpm
        , refOffsetInput = String.fromInt project.referenceAudio.offsetMs
        , dragState = NoDrag
        , rubberBand = Nothing
    }


applyUndo : Model -> ( Model, Cmd Msg )
applyUndo model =
    case model.undoStack of
        [] ->
            ( model, Cmd.none )

        prev :: rest ->
            ( restoreProject prev
                { model
                    | undoStack = rest
                    , redoStack = model.project :: model.redoStack
                }
            , Cmd.none
            )


applyRedo : Model -> ( Model, Cmd Msg )
applyRedo model =
    case model.redoStack of
        [] ->
            ( model, Cmd.none )

        next :: rest ->
            ( restoreProject next
                { model
                    | redoStack = rest
                    , undoStack = model.project :: model.undoStack
                }
            , Cmd.none
            )


maxUndoSteps : Int
maxUndoSteps =
    100


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( coreModel, cmd ) =
            updateCore msg model

        projectChanged =
            coreModel.project /= model.project

        newModel =
            if isUndoRedo msg then
                { coreModel | editBurst = False }

            else if projectChanged then
                { coreModel
                    | undoStack =
                        if isCoalescing msg && model.editBurst then
                            coreModel.undoStack

                        else
                            List.take maxUndoSteps (model.project :: coreModel.undoStack)
                    , redoStack = []
                    , editBurst = isCoalescing msg
                }

            else if isCoalescing msg then
                coreModel

            else
                { coreModel | editBurst = False }

        stillDragging =
            newModel.dragState /= NoDrag

        saveCmds =
            if projectChanged then
                [ Ports.saveToLocalStorage (ProjectJson.encode newModel.project) ]

            else
                []

        syncCmds =
            if newModel.playState == Playing && not stillDragging && (projectChanged || isReleasedDrag msg) then
                [ Ports.toAudio (Performance.encodeUpdateEvents newModel.project) ]

            else
                []
    in
    ( newModel, Cmd.batch (cmd :: saveCmds ++ syncCmds) )


startPlay : Maybe Performance.Loop -> Int -> Model -> ( Model, Cmd Msg )
startPlay loop startTicks model =
    ( { model
        | playState = Playing
        , instrumentLoad = markLoading (Performance.usedInstrumentNames model.project) model.instrumentLoad
      }
    , Ports.toAudio (Performance.encodePlay { loop = loop, startTicks = startTicks } model.project)
    )


loopModeFromString : String -> LoopMode
loopModeFromString raw =
    case raw of
        "song" ->
            LoopSong

        "section" ->
            LoopSection

        _ ->
            NoLoop


sectionAtTicks : Int -> Project -> Maybe Int
sectionAtTicks ticks project =
    project.sections
        |> List.foldl
            (\s ( start, found ) ->
                let
                    end =
                        start + s.lengthBars * Data.Time.ticksPerBar
                in
                case found of
                    Just _ ->
                        ( end, found )

                    Nothing ->
                        if ticks >= start && ticks < end then
                            ( end, Just s.id )

                        else
                            ( end, Nothing )
            )
            ( 0, Nothing )
        |> Tuple.second


currentLoop : Model -> Maybe Performance.Loop
currentLoop model =
    case model.loopMode of
        NoLoop ->
            Nothing

        LoopSong ->
            Just { startTicks = 0, endTicks = songEndTicks model.project }

        LoopSection ->
            (case model.selectedSectionId of
                Just sid ->
                    Just sid

                Nothing ->
                    sectionAtTicks model.playheadTicks model.project
            )
                |> Maybe.andThen (\sid -> Data.Project.sectionBounds sid model.project)


{-| ▶ と Space の共通入口。現在のループモードに従って再生する。
ループ範囲内に再生ヘッドがあればそこから、外なら範囲の先頭から。
-}
playWithLoop : Model -> ( Model, Cmd Msg )
playWithLoop model =
    case currentLoop model of
        Nothing ->
            startPlay Nothing model.playheadTicks model

        Just loop ->
            let
                start =
                    if model.playheadTicks >= loop.startTicks && model.playheadTicks < loop.endTicks then
                        model.playheadTicks

                    else
                        loop.startTicks
            in
            startPlay (Just loop) start model


{-| 停止しても再生ヘッドは今の位置に残す。先頭に戻すのは停止中のもう一度の■。
-}
stopPlayback : Model -> ( Model, Cmd Msg )
stopPlayback model =
    ( { model | playState = Idle }
    , Ports.toAudio Performance.encodeStop
    )


copySelection : Model -> Model
copySelection model =
    let
        sel =
            selectionNotes model
    in
    case List.minimum (List.map .start sel) of
        Nothing ->
            model

        Just base ->
            { model | clipboard = List.map (\n -> { n | start = n.start - base }) sel }


pasteClipboard : Model -> Model
pasteClipboard model =
    if List.isEmpty model.clipboard then
        model

    else
        let
            base =
                Basics.max 0 (snapFloor model.playheadTicks)

            project =
                model.project

            ( newNotesRev, nextId2 ) =
                List.foldl
                    (\n ( acc, nid ) -> ( { n | id = nid, start = base + n.start } :: acc, nid + 1 ))
                    ( [], project.nextId )
                    model.clipboard

            newNotes =
                List.reverse newNotesRev

            project2 =
                Data.Project.mapNotes model.selectedTrackId
                    (\ns -> newNotes ++ ns)
                    { project | nextId = nextId2 }
        in
        { model
            | project = project2
            , selectedNoteIds = Set.fromList (List.map .id newNotes)
        }


deleteSelection : Model -> Model
deleteSelection model =
    { model
        | project =
            Data.Project.mapNotes model.selectedTrackId
                (List.filter (\n -> not (Set.member n.id model.selectedNoteIds)))
                model.project
        , selectedNoteIds = Set.empty
    }


transposeSelection : KeyEvent -> Model -> ( Model, Cmd Msg )
transposeSelection k model =
    if Set.isEmpty model.selectedNoteIds then
        ( model, Cmd.none )

    else
        let
            step =
                if k.key == "ArrowUp" then
                    1

                else
                    -1

            delta =
                if k.shift then
                    step * 12

                else
                    step

            project2 =
                Data.Project.mapNotes model.selectedTrackId
                    (List.map
                        (\n ->
                            if Set.member n.id model.selectedNoteIds then
                                { n | pitch = clamp PianoRoll.minPitch PianoRoll.maxPitch (n.pitch + delta) }

                            else
                                n
                        )
                    )
                    model.project

            newModel =
                { model | project = project2 }

            previewCmd =
                selectionNotes newModel
                    |> List.map .pitch
                    |> List.maximum
                    |> Maybe.map (Ports.toAudio << Performance.encodePreviewNote (selectedInstrumentName model))
                    |> Maybe.withDefault Cmd.none
        in
        ( newModel, previewCmd )


noteOrder : Data.Note.Note -> ( Int, Int )
noteOrder n =
    ( n.start, n.pitch )


selectAdjacentNote : KeyEvent -> Model -> ( Model, Cmd Msg )
selectAdjacentNote k model =
    let
        sorted =
            trackNotes model |> List.sortBy noteOrder

        selected =
            selectionNotes model |> List.sortBy noteOrder

        candidate =
            if k.key == "ArrowRight" then
                case List.head (List.reverse selected) of
                    Just ref ->
                        sorted
                            |> List.filter (\n -> noteOrder n > noteOrder ref)
                            |> List.head

                    Nothing ->
                        List.head sorted

            else
                case List.head selected of
                    Just ref ->
                        sorted
                            |> List.filter (\n -> noteOrder n < noteOrder ref)
                            |> List.reverse
                            |> List.head

                    Nothing ->
                        List.head (List.reverse sorted)
    in
    case candidate of
        Just note ->
            ( { model | selectedNoteIds = Set.singleton note.id }
            , Ports.toAudio (Performance.encodePreviewNote (selectedInstrumentName model) note.pitch)
            )

        Nothing ->
            ( model, Cmd.none )


nudgeSelection : KeyEvent -> Model -> ( Model, Cmd Msg )
nudgeSelection k model =
    if Set.isEmpty model.selectedNoteIds then
        ( model, Cmd.none )

    else
        let
            step =
                if k.shift then
                    Data.Time.ticksPerBar

                else
                    Data.Time.ticksPerSixteenth

            dir =
                if k.key == "ArrowRight" then
                    1

                else
                    -1

            minStart =
                selectionNotes model
                    |> List.map .start
                    |> List.minimum
                    |> Maybe.withDefault 0

            delta =
                max (step * dir) (negate minStart)
        in
        if delta == 0 then
            ( model, Cmd.none )

        else
            ( { model
                | project =
                    Data.Project.mapNotes model.selectedTrackId
                        (List.map
                            (\n ->
                                if Set.member n.id model.selectedNoteIds then
                                    { n | start = n.start + delta }

                                else
                                    n
                            )
                        )
                        model.project
              }
            , Cmd.none
            )


keyToPitch : String -> Maybe Int
keyToPitch key =
    case key of
        "z" ->
            Just 48

        "s" ->
            Just 49

        "x" ->
            Just 50

        "d" ->
            Just 51

        "c" ->
            Just 52

        "v" ->
            Just 53

        "g" ->
            Just 54

        "b" ->
            Just 55

        "h" ->
            Just 56

        "n" ->
            Just 57

        "j" ->
            Just 58

        "m" ->
            Just 59

        "q" ->
            Just 60

        "2" ->
            Just 61

        "w" ->
            Just 62

        "3" ->
            Just 63

        "e" ->
            Just 64

        "r" ->
            Just 65

        "5" ->
            Just 66

        "t" ->
            Just 67

        "6" ->
            Just 68

        "y" ->
            Just 69

        "7" ->
            Just 70

        "u" ->
            Just 71

        "i" ->
            Just 72

        _ ->
            Nothing


updateCore : Msg -> Model -> ( Model, Cmd Msg )
updateCore msg model =
    case msg of
        ClickedPlay ->
            playWithLoop model

        ChangedLoopMode raw ->
            let
                newModel =
                    { model | loopMode = loopModeFromString raw }
            in
            if model.playState == Playing then
                -- 再生中の切替は現在位置から新しいループ設定で再スタート
                playWithLoop newModel

            else
                ( newModel, Cmd.none )

        ClickedStop ->
            if model.playState == Idle then
                ( { model | playheadTicks = 0 }, Cmd.none )

            else
                stopPlayback model

        ClickedUndo ->
            applyUndo model

        ClickedRedo ->
            applyRedo model

        ChangedChordVolume raw ->
            case String.toInt raw of
                Just vol ->
                    let
                        clamped =
                            clamp 0 100 vol

                        project =
                            model.project

                        ct =
                            project.chordTrack
                    in
                    ( { model | project = { project | chordTrack = { ct | volume = clamped } } }
                    , Ports.toAudio (Performance.encodeSetVolume -1 clamped)
                    )

                Nothing ->
                    ( model, Cmd.none )

        ChangedBpm raw ->
            let
                modelWithInput =
                    { model | bpmInput = raw }
            in
            case String.toInt raw of
                Just bpm ->
                    if bpm >= 30 && bpm <= 300 then
                        let
                            project =
                                model.project
                        in
                        ( { modelWithInput | project = { project | bpm = bpm } }
                        , if model.playState == Playing then
                            Ports.toAudio (Performance.encodeSetBpm bpm)

                          else
                            Cmd.none
                        )

                    else
                        ( modelWithInput, Cmd.none )

                Nothing ->
                    ( modelWithInput, Cmd.none )

        BlurredBpm ->
            ( { model | bpmInput = String.fromInt model.project.bpm }, Cmd.none )

        GotAudio event ->
            case event of
                Playhead ticks ->
                    ( { model | playheadTicks = ticks }, Cmd.none )

                PlaybackStopped ->
                    ( { model | playState = Idle, playheadTicks = 0 }, Cmd.none )

                InstrumentLoaded name ->
                    ( { model | instrumentLoad = Dict.insert name "ready" model.instrumentLoad }, Cmd.none )

                InstrumentLoadFailed name ->
                    ( { model | instrumentLoad = Dict.insert name "failed" model.instrumentLoad }, Cmd.none )

                RefAudioReady info ->
                    let
                        project =
                            model.project

                        ra =
                            project.referenceAudio
                    in
                    ( { model
                        | refLoaded = True
                        , refPeaks = info.peaks
                        , refPeakDt = info.peakDt
                        , project = { project | referenceAudio = { ra | fileName = Just info.name } }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        PressedEmptyCell pos ->
            if pos.seekMod then
                let
                    ticks =
                        Basics.max 0 (snapFloor (PianoRoll.pixelsToTicks pos.offsetX))
                in
                ( { model | playheadTicks = ticks }
                , if model.playState == Playing then
                    Ports.toAudio (Performance.encodeSeek ticks)

                  else
                    Cmd.none
                )

            else if pos.shift then
                ( { model
                    | rubberBand =
                        Just
                            { originX = pos.offsetX
                            , originY = pos.offsetY
                            , startClientX = pos.clientX
                            , startClientY = pos.clientY
                            , curX = pos.offsetX
                            , curY = pos.offsetY
                            }
                  }
                , Cmd.none
                )

            else
                let
                    start =
                        Basics.max 0 (snapFloor (PianoRoll.pixelsToTicks pos.offsetX))

                    pitch =
                        clamp PianoRoll.minPitch PianoRoll.maxPitch (PianoRoll.yToPitch pos.offsetY)

                    note =
                        { id = model.project.nextId
                        , pitch = pitch
                        , start = start
                        , duration = Data.Time.ticksPerSixteenth * 2
                        , velocity = 100
                        }
                in
                ( { model
                    | project = Data.Project.addNote model.selectedTrackId note model.project
                    , selectedNoteIds = Set.singleton note.id
                    , dragState =
                        Dragging
                            { anchorId = note.id
                            , mode = ResizeRight
                            , startClientX = pos.clientX
                            , startClientY = pos.clientY
                            , origNotes = [ note ]
                            , lastPreviewPitch = note.pitch
                            }
                  }
                , Ports.toAudio (Performance.encodePreviewNote (selectedInstrumentName model) note.pitch)
                )

        PressedNote noteId isResize pos ->
            case findNote model noteId of
                Just note ->
                    if pos.shift then
                        ( { model
                            | selectedNoteIds =
                                if Set.member noteId model.selectedNoteIds then
                                    Set.remove noteId model.selectedNoteIds

                                else
                                    Set.insert noteId model.selectedNoteIds
                          }
                        , Cmd.none
                        )

                    else
                        let
                            sel =
                                if Set.member noteId model.selectedNoteIds then
                                    model.selectedNoteIds

                                else
                                    Set.singleton noteId

                            origs =
                                trackNotes model
                                    |> List.filter (\n -> Set.member n.id sel)
                        in
                        ( { model
                            | selectedNoteIds = sel
                            , dragState =
                                Dragging
                                    { anchorId = noteId
                                    , mode =
                                        if isResize then
                                            ResizeRight

                                        else
                                            MoveNote
                                    , startClientX = pos.clientX
                                    , startClientY = pos.clientY
                                    , origNotes = origs
                                    , lastPreviewPitch = note.pitch
                                    }
                          }
                        , Ports.toAudio (Performance.encodePreviewNote (selectedInstrumentName model) note.pitch)
                        )

                Nothing ->
                    ( model, Cmd.none )

        DoubleClickedNote noteId ->
            ( { model
                | project = Data.Project.removeNote model.selectedTrackId noteId model.project
                , selectedNoteIds = Set.remove noteId model.selectedNoteIds
                , dragState = NoDrag
              }
            , Cmd.none
            )

        RightClickedNote noteId ->
            ( { model
                | project = Data.Project.removeNote model.selectedTrackId noteId model.project
                , selectedNoteIds = Set.remove noteId model.selectedNoteIds
              }
            , Cmd.none
            )

        DraggedTo pos ->
            case model.rubberBand of
                Just rb ->
                    ( { model
                        | rubberBand =
                            Just
                                { rb
                                    | curX = rb.originX + (pos.clientX - rb.startClientX)
                                    , curY = rb.originY + (pos.clientY - rb.startClientY)
                                }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    case model.dragState of
                        Dragging d ->
                            dragMove pos d model

                        NoDrag ->
                            ( model, Cmd.none )

        ReleasedDrag ->
            case model.rubberBand of
                Just rb ->
                    let
                        x0 =
                            Basics.min rb.originX rb.curX

                        x1 =
                            Basics.max rb.originX rb.curX

                        y0 =
                            Basics.min rb.originY rb.curY

                        y1 =
                            Basics.max rb.originY rb.curY

                        t0 =
                            PianoRoll.pixelsToTicks x0

                        t1 =
                            PianoRoll.pixelsToTicks x1

                        pLow =
                            PianoRoll.yToPitch y1

                        pHigh =
                            PianoRoll.yToPitch y0

                        sel =
                            trackNotes model
                                |> List.filter
                                    (\n ->
                                        (n.start < t1)
                                            && (n.start + n.duration > t0)
                                            && (n.pitch >= pLow)
                                            && (n.pitch <= pHigh)
                                    )
                                |> List.map .id
                                |> Set.fromList
                    in
                    ( { model | rubberBand = Nothing, dragState = NoDrag, selectedNoteIds = sel }, Cmd.none )

                Nothing ->
                    ( { model | dragState = NoDrag }, Cmd.none )

        ClickedRuler offsetX ->
            let
                ticks =
                    Basics.max 0 (snapFloor (PianoRoll.pixelsToTicks offsetX))
            in
            ( { model | playheadTicks = ticks }
            , if model.playState == Playing then
                Ports.toAudio (Performance.encodeSeek ticks)

              else
                Cmd.none
            )

        PressedPianoKey pitch ->
            ( model
            , Ports.toAudio (Performance.encodePreviewNote (selectedInstrumentName model) pitch)
            )

        GotKey k ->
            if List.member k.targetTag [ "INPUT", "TEXTAREA", "SELECT" ] then
                ( model, Cmd.none )

            else if k.key == " " then
                if model.playState == Playing then
                    stopPlayback model

                else
                    playWithLoop model

            else if (k.ctrl || k.meta) && (k.key == "z" || k.key == "Z") then
                if k.shift then
                    applyRedo model

                else
                    applyUndo model

            else if (k.ctrl || k.meta) && k.key == "y" then
                applyRedo model

            else if (k.ctrl || k.meta) && k.key == "c" then
                ( copySelection model, Cmd.none )

            else if (k.ctrl || k.meta) && k.key == "x" then
                ( deleteSelection (copySelection model), Cmd.none )

            else if (k.ctrl || k.meta) && k.key == "v" then
                ( pasteClipboard model, Cmd.none )

            else if k.key == "Delete" || k.key == "Backspace" then
                ( deleteSelection model, Cmd.none )

            else if k.key == "Escape" then
                ( { model | selectedNoteIds = Set.empty }, Cmd.none )

            else if k.key == "ArrowUp" || k.key == "ArrowDown" then
                transposeSelection k model

            else if k.key == "ArrowLeft" || k.key == "ArrowRight" then
                if k.ctrl || k.meta then
                    nudgeSelection k model

                else
                    selectAdjacentNote k model

            else if model.showKeyboard && not k.repeat && not k.ctrl && not k.meta then
                case keyToPitch k.key of
                    Just pitch ->
                        ( model
                        , Ports.toAudio (Performance.encodePreviewNote (selectedInstrumentName model) pitch)
                        )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        SelectedTrack trackId ->
            ( { model | selectedTrackId = trackId, selectedNoteIds = Set.empty }, Cmd.none )

        ClickedAddTrack ->
            let
                newTrackId =
                    model.project.nextId
            in
            ( { model
                | project = Data.Project.addTrack model.project
                , selectedTrackId = newTrackId
                , selectedNoteIds = Set.empty
              }
            , Cmd.none
            )

        ClickedRemoveTrack trackId ->
            let
                newProject =
                    Data.Project.removeTrack trackId model.project

                newSelected =
                    if model.selectedTrackId == trackId then
                        newProject.tracks
                            |> List.head
                            |> Maybe.map .id
                            |> Maybe.withDefault 0

                    else
                        model.selectedTrackId
            in
            ( { model | project = newProject, selectedTrackId = newSelected, selectedNoteIds = Set.empty }, Cmd.none )

        ToggledMute trackId ->
            let
                newMuted =
                    model.project.tracks
                        |> List.filter (\t -> t.id == trackId)
                        |> List.head
                        |> Maybe.map (\t -> not t.muted)
                        |> Maybe.withDefault False
            in
            ( { model | project = Data.Project.updateTrack trackId (\t -> { t | muted = newMuted }) model.project }
            , Ports.toAudio (Performance.encodeSetMute trackId newMuted)
            )

        ChangedInstrument trackId raw ->
            case Data.Track.instrumentFromString raw of
                Just inst ->
                    ( { model
                        | project = Data.Project.updateTrack trackId (\t -> { t | instrument = inst, kind = convertTrackKind inst t.kind }) model.project
                        , instrumentLoad = markLoading [ raw ] model.instrumentLoad
                      }
                    , if raw == "synthLead" || Dict.get raw model.instrumentLoad == Just "ready" then
                        Cmd.none

                      else
                        Ports.toAudio (Performance.encodeLoadInstruments [ raw ])
                    )

                Nothing ->
                    ( model, Cmd.none )

        ChangedVolume trackId raw ->
            case String.toInt raw of
                Just vol ->
                    let
                        clamped =
                            clamp 0 100 vol
                    in
                    ( { model | project = Data.Project.updateTrack trackId (\t -> { t | volume = clamped }) model.project }
                    , Ports.toAudio (Performance.encodeSetVolume trackId clamped)
                    )

                Nothing ->
                    ( model, Cmd.none )

        ClickedExport ->
            ( model
            , File.Download.string (model.project.name ++ ".json")
                "application/json"
                (Encode.encode 2 (ProjectJson.encode model.project))
            )

        ClickedImport ->
            ( model, File.Select.file [ "application/json" ] GotImportFile )

        GotImportFile file ->
            ( model, Task.perform GotImportContent (File.toString file) )

        GotImportContent content ->
            case Decode.decodeString ProjectJson.decoder content of
                Ok project ->
                    ( { model
                        | project = project
                        , selectedTrackId = firstTrackId project
                        , playState = Idle
                        , playheadTicks = 0
                        , bpmInput = String.fromInt project.bpm
                        , selectedNoteIds = Set.empty
                      }
                    , Ports.toAudio Performance.encodeStop
                    )

                Err _ ->
                    ( model, Cmd.none )

        ChangedChordText textValue ->
            ( { model | project = Data.Project.updateChordTrack (\ct -> { ct | text = textValue }) model.project }
            , Cmd.none
            )

        ToggledChordMute ->
            let
                newMuted =
                    not model.project.chordTrack.muted
            in
            ( { model | project = Data.Project.updateChordTrack (\ct -> { ct | muted = newMuted }) model.project }
            , Ports.toAudio (Performance.encodeSetMute (negate 1) newMuted)
            )

        ClickedConvertChords ->
            let
                project =
                    model.project

                pitchTriples =
                    Data.ChordTrack.resolved project.chordTrack
                        |> List.concatMap
                            (\ev ->
                                Data.Chord.toPitches ev.chord
                                    |> List.map (\p -> ( ev.startTicks, ev.durationTicks, p ))
                            )

                trackId =
                    project.nextId

                ( notesRev, nextId2 ) =
                    List.foldl
                        (\( s, dur, p ) ( acc, nid ) ->
                            ( { id = nid, pitch = p, start = s, duration = dur, velocity = 90 } :: acc
                            , nid + 1
                            )
                        )
                        ( [], trackId + 1 )
                        pitchTriples

                newTrack =
                    { id = trackId
                    , name = "コード"
                    , instrument = project.chordTrack.instrument
                    , muted = False
                    , volume = 100
                    , kind = NoteTrack (List.reverse notesRev)
                    }
            in
            if List.isEmpty pitchTriples then
                ( model, Cmd.none )

            else
                ( { model
                    | project = { project | tracks = project.tracks ++ [ newTrack ], nextId = nextId2 }
                    , selectedTrackId = trackId
                    , selectedNoteIds = Set.empty
                  }
                , Cmd.none
                )

        SelectedSection sectionId ->
            ( { model
                | selectedSectionId =
                    if model.selectedSectionId == Just sectionId then
                        Nothing

                    else
                        Just sectionId
              }
            , Cmd.none
            )

        ClickedAddSection ->
            ( { model | project = Data.Project.addSection model.project }, Cmd.none )

        ClickedRemoveSection sectionId ->
            ( { model
                | project = Data.Project.removeSection sectionId model.project
                , selectedSectionId =
                    if model.selectedSectionId == Just sectionId then
                        Nothing

                    else
                        model.selectedSectionId
              }
            , Cmd.none
            )

        ChangedSectionName sectionId newName ->
            ( { model | project = Data.Project.updateSection sectionId (\s -> { s | name = newName }) model.project }
            , Cmd.none
            )

        ChangedSectionBars sectionId raw ->
            case String.toInt raw of
                Just bars ->
                    ( { model | project = Data.Project.updateSection sectionId (\s -> { s | lengthBars = clamp 1 64 bars }) model.project }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ChangedSectionMemo sectionId memo ->
            ( { model | project = Data.Project.updateSection sectionId (\s -> { s | memo = memo }) model.project }
            , Cmd.none
            )

        MovedSection sectionId delta ->
            ( { model | project = Data.Project.moveSection sectionId delta model.project }, Cmd.none )

        ToggledDrumCell pitch tick ->
            let
                existing =
                    trackNotes model
                        |> List.filter (\n -> n.pitch == pitch && n.start == tick)
                        |> List.head
            in
            case existing of
                Just note ->
                    ( { model | project = Data.Project.removeNote model.selectedTrackId note.id model.project }
                    , Cmd.none
                    )

                Nothing ->
                    let
                        note =
                            { id = model.project.nextId
                            , pitch = pitch
                            , start = tick
                            , duration = Data.Time.ticksPerSixteenth
                            , velocity = 100
                            }
                    in
                    ( { model | project = Data.Project.addNote model.selectedTrackId note model.project }
                    , Ports.toAudio (Performance.encodePreviewNote "drumKit" pitch)
                    )

        AppliedDrumPreset presetName ->
            case Data.DrumPattern.byName presetName of
                Just pattern ->
                    let
                        range =
                            model.selectedSectionId
                                |> Maybe.andThen (\sid -> Data.Project.sectionBounds sid model.project)
                                |> Maybe.withDefault { startTicks = 0, endTicks = model.drumFillBars * Data.Time.ticksPerBar }
                    in
                    ( { model
                        | project =
                            Data.DrumPattern.apply
                                { trackId = model.selectedTrackId
                                , startTicks = range.startTicks
                                , endTicks = range.endTicks
                                }
                                pattern
                                model.project
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ChangedDrumFillBars raw ->
            case String.toInt raw of
                Just bars ->
                    ( { model | drumFillBars = clamp 1 64 bars }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ClickedExportMidi ->
            ( model
            , File.Download.bytes (model.project.name ++ ".mid") "audio/midi" (Midi.Encode.fromProject model.project)
            )

        ToggledKeyboard ->
            ( { model | showKeyboard = not model.showKeyboard }, Cmd.none )

        ClickedAddScrap ->
            let
                sel =
                    selectionNotes model
            in
            case List.minimum (List.map .start sel) of
                Nothing ->
                    ( model, Cmd.none )

                Just base ->
                    ( { model
                        | project =
                            Data.Project.addScrap
                                (List.map (\n -> { n | start = n.start - base }) sel)
                                model.project
                      }
                    , Cmd.none
                    )

        ClickedPlaceScrap scrapId ->
            case model.project.scraps |> List.filter (\s -> s.id == scrapId) |> List.head of
                Nothing ->
                    ( model, Cmd.none )

                Just scrap ->
                    let
                        base =
                            Basics.max 0 (snapFloor model.playheadTicks)

                        project =
                            model.project

                        ( newNotesRev, nextId2 ) =
                            List.foldl
                                (\n ( acc, nid ) -> ( { n | id = nid, start = base + n.start } :: acc, nid + 1 ))
                                ( [], project.nextId )
                                scrap.notes

                        newNotes =
                            List.reverse newNotesRev
                    in
                    ( { model
                        | project =
                            Data.Project.mapNotes model.selectedTrackId
                                (\ns -> newNotes ++ ns)
                                { project | nextId = nextId2 }
                        , selectedNoteIds = Set.fromList (List.map .id newNotes)
                      }
                    , Cmd.none
                    )

        ClickedRemoveScrap scrapId ->
            ( { model | project = Data.Project.removeScrap scrapId model.project }, Cmd.none )

        ChangedScrapName scrapId newName ->
            ( { model | project = Data.Project.updateScrap scrapId (\s -> { s | name = newName }) model.project }, Cmd.none )

        ChangedTrackName trackId newName ->
            ( { model | project = Data.Project.updateTrack trackId (\t -> { t | name = newName }) model.project }, Cmd.none )

        ChangedRefOffset raw ->
            let
                modelWithInput =
                    { model | refOffsetInput = raw }
            in
            case String.toInt raw of
                Just ms ->
                    let
                        project =
                            model.project

                        ra =
                            project.referenceAudio
                    in
                    ( { modelWithInput | project = { project | referenceAudio = { ra | offsetMs = ms } } }
                    , Cmd.none
                    )

                Nothing ->
                    ( modelWithInput, Cmd.none )

        BlurredRefOffset ->
            ( { model | refOffsetInput = String.fromInt model.project.referenceAudio.offsetMs }, Cmd.none )

        ChangedRefVolume raw ->
            case String.toInt raw of
                Just vol ->
                    let
                        project =
                            model.project

                        ra =
                            project.referenceAudio
                    in
                    ( { model | project = { project | referenceAudio = { ra | volume = clamp 0 100 vol } } }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ToggledRefMute ->
            let
                project =
                    model.project

                ra =
                    project.referenceAudio
            in
            ( { model | project = { project | referenceAudio = { ra | muted = not ra.muted } } }
            , Cmd.none
            )

        ToggledDrumView ->
            ( { model | drumViewRoll = not model.drumViewRoll, selectedNoteIds = Set.empty }, Cmd.none )


dragMove : ClientPos -> DragInfo -> Model -> ( Model, Cmd Msg )
dragMove pos d model =
    let
        dticks =
            snapRound (PianoRoll.pixelsToTicks (pos.clientX - d.startClientX))
    in
    case d.mode of
        MoveNote ->
            let
                dpitch =
                    negate (Basics.round ((pos.clientY - d.startClientY) / toFloat PianoRoll.rowHeight))

                project2 =
                    List.foldl
                        (\orig proj ->
                            Data.Project.updateNote model.selectedTrackId
                                orig.id
                                (\n ->
                                    { n
                                        | start = Basics.max 0 (orig.start + dticks)
                                        , pitch = clamp PianoRoll.minPitch PianoRoll.maxPitch (orig.pitch + dpitch)
                                    }
                                )
                                proj
                        )
                        model.project
                        d.origNotes

                anchorPitch =
                    d.origNotes
                        |> List.filter (\n -> n.id == d.anchorId)
                        |> List.head
                        |> Maybe.map (\n -> clamp PianoRoll.minPitch PianoRoll.maxPitch (n.pitch + dpitch))
                        |> Maybe.withDefault d.lastPreviewPitch
            in
            ( { model
                | project = project2
                , dragState = Dragging { d | lastPreviewPitch = anchorPitch }
              }
            , if anchorPitch /= d.lastPreviewPitch then
                Ports.toAudio (Performance.encodePreviewNote (selectedInstrumentName model) anchorPitch)

              else
                Cmd.none
            )

        ResizeRight ->
            let
                project2 =
                    List.foldl
                        (\orig proj ->
                            Data.Project.updateNote model.selectedTrackId
                                orig.id
                                (\n -> { n | duration = Basics.max Data.Time.ticksPerSixteenth (orig.duration + dticks) })
                                proj
                        )
                        model.project
                        d.origNotes
            in
            ( { model | project = project2 }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        bar =
            model.playheadTicks // Data.Time.ticksPerBar + 1

        beat =
            modBy Data.Time.ticksPerBar model.playheadTicks // Data.Time.ppq + 1

        stateLabel =
            case model.playState of
                Idle ->
                    "停止中"

                Playing ->
                    "再生中"

        selectedTrackName =
            model.project.tracks
                |> List.filter (\t -> t.id == model.selectedTrackId)
                |> List.head
                |> Maybe.map .name
                |> Maybe.withDefault "(トラック未選択)"

        selectionInfo =
            let
                sel =
                    selectionNotes model
            in
            if List.length sel >= 2 then
                let
                    detected =
                        Data.Chord.Detect.detect (List.map .pitch sel)
                            |> Maybe.withDefault "?"
                in
                " — 選択 " ++ String.fromInt (List.length sel) ++ "個 / コード判定: " ++ detected

            else if List.length sel == 1 then
                " — 選択 1個"

            else
                ""
    in
    div [ style "font-family" "sans-serif", style "padding" "1rem" ]
        [ h1 [ style "font-size" "1.3rem" ] [ text "音書き otogaki" ]
        , div []
            [ button [ onClick ClickedPlay ] [ text "▶ 再生" ]
            , button [ onClick ClickedStop ] [ text "■ 停止" ]
            , button
                [ onClick ClickedUndo
                , disabled (List.isEmpty model.undoStack)
                , Html.Attributes.title "元に戻す (Ctrl/Cmd+Z)"
                ]
                [ text "↩" ]
            , button
                [ onClick ClickedRedo
                , disabled (List.isEmpty model.redoStack)
                , Html.Attributes.title "やり直し (Ctrl/Cmd+Shift+Z)"
                ]
                [ text "↪" ]
            , label [ style "margin-left" "0.5rem" ]
                [ text "🔁 ループ: "
                , Html.select [ onInput ChangedLoopMode ]
                    [ Html.option [ value "off", Html.Attributes.selected (model.loopMode == NoLoop) ] [ text "オフ" ]
                    , Html.option [ value "song", Html.Attributes.selected (model.loopMode == LoopSong) ] [ text "全体" ]
                    , Html.option [ value "section", Html.Attributes.selected (model.loopMode == LoopSection) ] [ text "セクション" ]
                    ]
                ]
            , label []
                [ text " BPM: "
                , input
                    [ type_ "number"
                    , value model.bpmInput
                    , onInput ChangedBpm
                    , onBlur BlurredBpm
                    , style "width" "4.5rem"
                    ]
                    []
                ]
            , button [ onClick ClickedExport, style "margin-left" "1rem" ] [ text "JSON書出" ]
            , button [ onClick ClickedImport ] [ text "JSON読込" ]
            , button [ onClick ClickedExportMidi ] [ text "MIDI書出" ]
            , text ("  " ++ stateLabel ++ " — " ++ String.fromInt bar ++ " 小節 " ++ String.fromInt beat ++ " 拍目")
            ]
        , div [ style "font-size" "0.75rem", style "color" "#888", style "margin-top" "0.2rem" ]
            [ text "Space: 再生/停止 ・ Ctrl/Cmd+Z: 元に戻す（Shiftでやり直し） ・ ルーラーか Ctrl/Cmd+クリック: 再生位置移動 ・ Shift+ドラッグ: 矩形選択 ・ ↑↓: 半音移動（Shiftでオクターブ） ・ ←→: 隣のノートを選択（Ctrl/Cmdで横移動、+Shiftで1小節） ・ Ctrl/Cmd+C・X・V: コピー・カット・貼付 ・ Delete: 削除 ・ ダブルクリック/右クリック: ノート削除" ]
        , SectionBar.view
            { select = SelectedSection
            , add = ClickedAddSection
            , remove = ClickedRemoveSection
            , rename = ChangedSectionName
            , changeBars = ChangedSectionBars
            , changeMemo = ChangedSectionMemo
            , move = MovedSection
            }
            model.selectedSectionId
            model.project.sections
        , Arrange.view
            { selectTrack = SelectedTrack
            , addTrack = ClickedAddTrack
            , removeTrack = ClickedRemoveTrack
            , toggleMute = ToggledMute
            , changeInstrument = ChangedInstrument
            , changeVolume = ChangedVolume
            , renameTrack = ChangedTrackName
            }
            model.selectedTrackId
            model.instrumentLoad
            model.project.tracks
        , RefAudio.view
            { changedOffset = ChangedRefOffset
            , blurredOffset = BlurredRefOffset
            , changedVolume = ChangedRefVolume
            , toggledMute = ToggledRefMute
            }
            model.refOffsetInput
            model.refLoaded
            model.project.referenceAudio
        , ChordEditor.view
            { changedText = ChangedChordText
            , toggledMute = ToggledChordMute
            , convertToTrack = ClickedConvertChords
            , changedVolume = ChangedChordVolume
            }
            model.playheadTicks
            model.project.chordTrack
        , ScrapShelf.view
            { addFromSelection = ClickedAddScrap
            , place = ClickedPlaceScrap
            , remove = ClickedRemoveScrap
            , rename = ChangedScrapName
            }
            (Set.size model.selectedNoteIds)
            model.project.scraps
        , div [ style "margin-top" "1rem", style "font-size" "0.9rem" ]
            [ text ("編集中: " ++ selectedTrackName ++ selectionInfo) ]
        , let
            pianoRollView =
                PianoRoll.view
                    { pressedEmpty = PressedEmptyCell
                    , pressedNote = PressedNote
                    , doubleClickedNote = DoubleClickedNote
                    , rightClickedNote = RightClickedNote
                    , clickedRuler = ClickedRuler
                    , pressedKey = PressedPianoKey
                    }
                    { notes = trackNotes model
                    , selectedIds = model.selectedNoteIds
                    , playheadTicks = model.playheadTicks
                    , sections = sectionSpans model.project
                    , totalBars = totalBarsFor model.project
                    , rubberBand =
                        model.rubberBand
                            |> Maybe.map
                                (\rb ->
                                    { x = Basics.min rb.originX rb.curX
                                    , y = Basics.min rb.originY rb.curY
                                    , w = abs (rb.curX - rb.originX)
                                    , h = abs (rb.curY - rb.originY)
                                    }
                                )
                    , waveform =
                        if Array.isEmpty model.refPeaks then
                            Nothing

                        else
                            Just
                                { peaks = model.refPeaks
                                , peakDt = model.refPeakDt
                                , secsPerTick = 60 / toFloat (model.project.bpm * Data.Time.ppq)
                                , offsetMs = model.project.referenceAudio.offsetMs
                                }
                    }
          in
          case selectedTrackKind model of
            Just (DrumTrack _) ->
                div []
                    [ button [ onClick ToggledDrumView, style "margin-top" "0.5rem" ]
                        [ text
                            (if model.drumViewRoll then
                                "🥁 ステップグリッドで編集"

                             else
                                "🎹 ピアノロールで編集（選択・移動・コピペ）"
                            )
                        ]
                    , if model.drumViewRoll then
                        pianoRollView

                      else
                        DrumEditor.view
                            { toggledCell = ToggledDrumCell
                            , appliedPreset = AppliedDrumPreset
                            , changedFillBars = ChangedDrumFillBars
                            }
                            model.drumFillBars
                            (trackNotes model)
                            model.playheadTicks
                    ]

            _ ->
                pianoRollView
        , Keyboard.view
            { pressedKey = PressedPianoKey
            , toggled = ToggledKeyboard
            }
            model.showKeyboard
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.fromAudio (AudioMsg.decode >> GotAudio)
        , if model.dragState /= NoDrag || model.rubberBand /= Nothing then
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map DraggedTo clientPosDecoder)
                , Browser.Events.onMouseUp (Decode.succeed ReleasedDrag)
                ]

          else
            Sub.none
        , Browser.Events.onKeyDown (Decode.map GotKey keyEventDecoder)
        ]


clientPosDecoder : Decode.Decoder ClientPos
clientPosDecoder =
    Decode.map2 ClientPos
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


keyEventDecoder : Decode.Decoder KeyEvent
keyEventDecoder =
    Decode.map6 KeyEvent
        (Decode.field "key" Decode.string)
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "metaKey" Decode.bool)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.field "repeat" Decode.bool)
        (Decode.oneOf
            [ Decode.at [ "target", "tagName" ] Decode.string
            , Decode.succeed ""
            ]
        )


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
