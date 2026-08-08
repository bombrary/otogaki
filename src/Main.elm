module Main exposing (main)

import Array exposing (Array)
import AudioMsg exposing (AudioEvent(..))
import Browser
import Browser.Dom
import Browser.Events
import Codec.Performance as Performance
import Codec.ProjectJson as ProjectJson
import Data.Chord
import Data.Chord.Detect
import Data.StrumExpand
import Data.StrumPattern
import Data.GuitarForm
import Data.Voicing
import Data.VoicingPreset
import Data.ChordTrack
import Data.DrumPattern
import Data.Key
import Data.Meter
import Data.Note
import Data.Project exposing (Project)
import Data.Time
import Data.Timeline exposing (Timeline)
import Data.Track exposing (TrackKind(..))
import Dict exposing (Dict)
import File
import File.Download
import File.Select
import Html exposing (Html, button, div, h1, input, label, span, text, textarea)
import Html.Attributes exposing (disabled, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Midi.Encode
import Ports
import Set exposing (Set)
import View.Style as Style
import View.VoicingKeyboard as VoicingKeyboard
import Process
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


type VoicingDragState
    = NoVoicingDrag
    | DraggingVoicingOffsets
        { index : Int
        , startClientY : Float
        , origOffsets : List Int
        , origSelected : Set Int
        , origPicks : Data.GuitarForm.StringPicks
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
    , insertCountInput : String
    , ghostTrackIds : Set Int
    , defaultNoteDuration : Int
    , highlightedPitches : Set Int
    , heldKeyPitches : Set Int
    , followPlayhead : Bool
    , loopRange : Maybe Performance.Loop
    , loopDrag : Maybe { fixedTicks : Int, baseTicks : Int, startClientX : Float, curTicks : Int }
    , lastEditLabel : String
    , pendingSectionDelete : Maybe Int
    , pendingTrackDelete : Maybe Int
    , pendingScrapDelete : Maybe Int
    , editingVoicingIndex : Maybe Int
    , pendingVoicingDelete : Maybe Int
    , chordCopyFeedback : Bool
    , voicingPreviewRoot : Int
    , voicingPresetQuality : String
    , voicingPresetShape : String
    , voicingSelectedOffsets : Set Int
    , voicingDragState : VoicingDragState
    , pianoRollZoom : Int
    , sectionResizeDrag : Maybe { sectionId : Int, startClientX : Float, origLengthBars : Int, curLengthBars : Int }
    , sectionMoveDrag : Maybe { sectionId : Int, lastClientX : Float, accumDx : Float }
    , sectionBarZoom : Int
    , sectionLoopDrag : Maybe { fixedTicks : Int, baseTicks : Int, startClientX : Float, curTicks : Int }
    , leftPaneWidth : Int
    , paneDividerDrag : Maybe { startClientX : Float, origWidth : Int }
    }


type LoopMode
    = NoLoop
    | LoopSong
    | LoopSection
    | LoopRange


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
    | PressedRuler { offsetX : Float, clientX : Float, shift : Bool }
    | PressedPianoKey Int
    | GotKey KeyEvent
    | ReleasedKey String
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
    | ChangedChordInstrument String
    | ToggledChordMute
    | ToggledVoicingEnabled
    | ToggledGuitarFormEnabled
    | ClickedAddVoicing String
    | ClickedVoicingRow Int
    | ChangedVoicingName Int String
    | ClickedPlayVoicing Int
    | ClickedRemoveVoicing Int
    | ChangedVoicingPreviewRoot String
    | ChangedVoicingPresetQuality String
    | ChangedVoicingPresetShape String
    | AppliedVoicingPreset Int
    | PressedVoicingOffset Int Int { clientX : Float, clientY : Float, shift : Bool }
    | DoubleClickedVoicingOffset Int Int
    | PressedFretboardCell Int Int Int
    | DoubleClickedFretboardCell Int Int Int
    | ClickedCopyChordText
    | ResetCopyFeedback
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
    | AppliedStrumPattern String
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
    | ChangedMemo String
    | ChangedSectionKey Int String
    | ChangedSectionMode Int String
    | ChangedSectionMeter Int String
    | TransposedSong Int
    | TransposedSection Int Int
    | ChangedInsertCount String
    | InsertedBarsBeforeSection Int
    | RemovedBarsFromSection Int
    | SeekTo Int
    | SeekPrevSection
    | SeekNextSection
    | ClickedChordAt Int
    | ClickedSeekSectionStart Int
    | ToggledGhostTrack Int
    | ChangedDefaultDuration String
    | ToggledFollowPlayhead
    | GotPianoRollViewport Int (Result Browser.Dom.Error Browser.Dom.Viewport)
    | PressedLoopHandle Bool Float
    | WheelZoomedRuler { deltaY : Float, offsetX : Float }
    | GotPianoRollViewportForZoom { deltaY : Float, offsetX : Float } (Result Browser.Dom.Error Browser.Dom.Viewport)
    | PressedSectionResizeHandle Int Float
    | PressedSectionBlock Int Float
    | WheelZoomedSectionBar { deltaY : Float, offsetX : Float }
    | GotSectionBarViewportForZoom { deltaY : Float, offsetX : Float } (Result Browser.Dom.Error Browser.Dom.Viewport)
    | PressedSectionRuler { offsetX : Float, clientX : Float, shift : Bool }
    | PressedSectionLoopHandle Bool Float
    | PressedPaneDivider Float
    | NoOp


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
      , bpmInput = String.fromFloat project.bpm
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
      , insertCountInput = "1"
      , ghostTrackIds = Set.empty
      , defaultNoteDuration = Data.Time.ticksPerSixteenth * 2
      , highlightedPitches = Set.empty
      , heldKeyPitches = Set.empty
      , followPlayhead = True
      , loopRange = Nothing
      , loopDrag = Nothing
      , lastEditLabel = ""
      , pendingSectionDelete = Nothing
      , pendingTrackDelete = Nothing
      , pendingScrapDelete = Nothing
      , editingVoicingIndex = Nothing
      , pendingVoicingDelete = Nothing
      , chordCopyFeedback = False
      , voicingPreviewRoot = 0
      , voicingPresetQuality = "maj7"
      , voicingPresetShape = "クローズド"
      , voicingSelectedOffsets = Set.empty
      , voicingDragState = NoVoicingDrag
      , pianoRollZoom = PianoRoll.defaultPxPerSixteenth
      , sectionResizeDrag = Nothing
      , sectionMoveDrag = Nothing
      , sectionBarZoom = SectionBar.defaultRegionPxPerBar
      , sectionLoopDrag = Nothing
      , leftPaneWidth = 380
      , paneDividerDrag = Nothing
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


{-| ボイシングトグルが OFF なら辞書を空にし、`toPitchesWith` のフォールバック経路を使わせる。
-}
effectiveVoicings : Model -> List Data.Voicing.Voicing
effectiveVoicings model =
    if model.project.voicingEnabled then
        model.project.voicings

    else
        []


{-| ゴースト表示中のトラック（通常ノート + 慣例の id -1 ならコードトラック）のノートをトラックごとにグループ化して返す。
色分け用にフラット化しない。クリック不可なので id は使われないが型を合わせるため -1 を入れておく。
-}
ghostNoteGroups : Model -> Timeline -> List (List Data.Note.Note)
ghostNoteGroups model timeline =
    let
        trackGroups =
            model.project.tracks
                |> List.filter (\t -> Set.member t.id model.ghostTrackIds)
                |> List.map (\t -> notesOf t.kind)

        chordGroup =
            if Set.member -1 model.ghostTrackIds then
                [ Data.ChordTrack.resolved timeline model.project.chordTrack
                    |> List.concatMap
                        (\ev ->
                            Data.Chord.toPitchesWith (effectiveVoicings model) ev.chord
                                |> List.map
                                    (\p ->
                                        { id = -1
                                        , pitch = p
                                        , start = ev.startTicks
                                        , duration = ev.durationTicks
                                        , velocity = 80
                                        }
                                    )
                        )
                ]

            else
                []
    in
    trackGroups ++ chordGroup


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


selectedTrackInstrument : Model -> Maybe Data.Track.Instrument
selectedTrackInstrument model =
    model.project.tracks
        |> List.filter (\t -> t.id == model.selectedTrackId)
        |> List.head
        |> Maybe.map .instrument


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


{-| 小節計算は Data.Timeline に集約されている。ここは既存呼び出し側を変えないための薄いラッパ。
-}
songEndTicks : Project -> Int
songEndTicks project =
    Data.Timeline.totalTicks (Data.Project.timeline project)


totalBarsFor : Project -> Int
totalBarsFor project =
    Data.Timeline.totalBars (Data.Project.timeline project)


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

        ChangedMemo _ ->
            True

        ChangedInsertCount _ ->
            True

        DraggedTo _ ->
            True

        PressedEmptyCell _ ->
            True

        PressedVoicingOffset _ _ _ ->
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
        , bpmInput = String.fromFloat project.bpm
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


describeMsg : Msg -> String
describeMsg msg =
    case msg of
        ClickedRemoveSection _ ->
            "セクション削除"

        ClickedRemoveTrack _ ->
            "トラック削除"

        DoubleClickedNote _ ->
            "ノート削除"

        RightClickedNote _ ->
            "ノート削除"

        PressedEmptyCell _ ->
            "ノート追加"

        TransposedSong _ ->
            "移調"

        TransposedSection _ _ ->
            "セクション移調"

        _ ->
            "編集"


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
                    , lastEditLabel = describeMsg msg
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

        finalModel =
            if isReleasedDrag msg then
                { newModel | highlightedPitches = Set.empty }

            else
                newModel
    in
    ( finalModel, Cmd.batch (cmd :: saveCmds ++ syncCmds) )


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

        "range" ->
            LoopRange

        _ ->
            NoLoop


sectionAtTicks : Int -> Project -> Maybe Int
sectionAtTicks ticks project =
    Data.Timeline.sectionAt ticks (Data.Project.timeline project)


{-| 左鍵盤のスケールマーカーが参照する基準位置。選択中のセクションがあればその開始位置、なければプレイヘッド位置。
currentLoop の LoopSection 分岐と同じフォールバック。
-}
scaleReferenceTicks : Model -> Int
scaleReferenceTicks model =
    model.selectedSectionId
        |> Maybe.andThen (\sid -> Data.Project.sectionBounds sid model.project)
        |> Maybe.map .startTicks
        |> Maybe.withDefault model.playheadTicks


{-| セクションが始まる 0-based 小節番号。小節挿入・削除の基準点を決めるのに使う。
-}
sectionStartBar : Int -> Project -> Maybe Int
sectionStartBar sectionId project =
    Data.Project.sectionBounds sectionId project
        |> Maybe.map (\b -> (Data.Timeline.ticksToBarBeat b.startTicks (Data.Project.timeline project)).bar - 1)


{-| セクションの小節数を newLenRaw（1..64 にクランプ）に変える。小節数入力（`ChangedSectionBars`）と
リサイズハンドルのドラッグリリース両方から共通で呼ばれる。
-}
resizeSectionBars : Int -> Int -> Model -> Model
resizeSectionBars sectionId newLenRaw model =
    let
        newLen =
            clamp 1 64 newLenRaw

        oldLen =
            model.project.sections
                |> List.filter (\s -> s.id == sectionId)
                |> List.head
                |> Maybe.map .lengthBars
    in
    case ( sectionStartBar sectionId model.project, oldLen ) of
        ( Just startBar, Just old ) ->
            if newLen > old then
                { model | project = Data.Project.insertBars { beforeBar = startBar + old - 1, count = newLen - old } model.project }

            else if newLen < old then
                { model | project = Data.Project.removeBars { fromBar = startBar + newLen, count = old - newLen } model.project }

            else
                model

        _ ->
            { model | project = Data.Project.updateSection sectionId (\s -> { s | lengthBars = newLen }) model.project }


parseInsertCount : String -> Int
parseInsertCount raw =
    String.toInt raw |> Maybe.withDefault 1 |> clamp 1 64


{-| 再生位置を動かす共通入口。再生中なら音のエンジンにも同じ位置を伝える。
複数の場所（ルーラー・ピアノロール・セクションジャンプ・コードクリックなど）から呼ばれる。
-}
seekTo : Int -> Model -> ( Model, Cmd Msg )
seekTo ticks model =
    let
        clamped =
            Basics.max 0 ticks
    in
    ( { model | playheadTicks = clamped }
    , if model.playState == Playing then
        Ports.toAudio (Performance.encodeSeek clamped)

      else
        Cmd.none
    )


{-| 全セクションの開始 tick を並び順のまま。前後セクション移動の探索に使う。
-}
sectionStartTicksList : Project -> List Int
sectionStartTicksList project =
    project.sections
        |> List.filterMap (\s -> Data.Project.sectionBounds s.id project)
        |> List.map .startTicks


{-| 再生ヘッドより前で一番近いセクション開始 tick。無ければ現在位置のまま（先頭より前へは動かない）。
-}
prevSectionStart : Model -> Int
prevSectionStart model =
    sectionStartTicksList model.project
        |> List.filter (\t -> t < model.playheadTicks)
        |> List.maximum
        |> Maybe.withDefault model.playheadTicks


{-| 再生ヘッドより後で一番近いセクション開始 tick。無ければ現在位置のまま（最後のセクションより先へは動かない）。
-}
nextSectionStart : Model -> Int
nextSectionStart model =
    sectionStartTicksList model.project
        |> List.filter (\t -> t > model.playheadTicks)
        |> List.minimum
        |> Maybe.withDefault model.playheadTicks


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

        LoopRange ->
            model.loopRange


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


{-| ループドラッグ確定時の共通ロジック。ピアノロールのルーラーとセクションルーラーのどちらでドラッグしても
同じ最終状態（`loopRange`/`loopMode`）に収束させる。呼び出し側で自分の drag フィールドを `Nothing` に戻すこと。
-}
commitLoopDrag : { fixedTicks : Int, curTicks : Int } -> Model -> ( Model, Cmd Msg )
commitLoopDrag ld model =
    let
        t0 =
            Basics.min ld.fixedTicks ld.curTicks

        t1 =
            Basics.max ld.fixedTicks ld.curTicks

        newModel =
            if t0 == t1 then
                { model | loopRange = Nothing, loopMode = NoLoop }

            else
                { model | loopRange = Just { startTicks = t0, endTicks = t1 }, loopMode = LoopRange }
    in
    if model.playState == Playing then
        playWithLoop newModel

    else
        ( newModel, Cmd.none )


{-| キーボードの `[`/`]` から呼ばれる。現在のループ範囲の開始または終了を再生位置に差し替える。逆転したら正規化し、幅が0になったらループを解除する。
-}
setLoopRangeBound : Bool -> Model -> ( Model, Cmd Msg )
setLoopRangeBound isStart model =
    let
        base =
            model.loopRange
                |> Maybe.withDefault { startTicks = model.playheadTicks, endTicks = model.playheadTicks }

        updated =
            if isStart then
                { base | startTicks = model.playheadTicks }

            else
                { base | endTicks = model.playheadTicks }

        t0 =
            Basics.min updated.startTicks updated.endTicks

        t1 =
            Basics.max updated.startTicks updated.endTicks

        newModel =
            if t0 == t1 then
                { model | loopRange = Nothing, loopMode = NoLoop }

            else
                { model | loopRange = Just { startTicks = t0, endTicks = t1 }, loopMode = LoopRange }
    in
    if model.playState == Playing then
        playWithLoop newModel

    else
        ( newModel, Cmd.none )


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

            previewPitch =
                selectionNotes newModel
                    |> List.map .pitch
                    |> List.maximum

            previewCmd =
                previewPitch
                    |> Maybe.map (Ports.toAudio << Performance.encodePreviewNote (selectedInstrumentName model))
                    |> Maybe.withDefault Cmd.none
        in
        ( { newModel | highlightedPitches = previewPitch |> Maybe.map Set.singleton |> Maybe.withDefault Set.empty }, previewCmd )


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
            ( { model | selectedNoteIds = Set.singleton note.id, highlightedPitches = Set.singleton note.pitch }
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
            case String.toFloat raw of
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
            ( { model | bpmInput = String.fromFloat model.project.bpm }, Cmd.none )

        GotAudio event ->
            case event of
                Playhead ticks ->
                    ( { model | playheadTicks = ticks }
                    , if model.followPlayhead then
                        Task.attempt (GotPianoRollViewport ticks) (Browser.Dom.getViewportOf PianoRoll.pianoRollScrollId)

                      else
                        Cmd.none
                    )

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
                        , project =
                            { project
                                | referenceAudio =
                                    { ra
                                        | fileName = Just info.name
                                        , durationMs = Just (round (info.durationSecs * 1000))
                                    }
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        PressedEmptyCell pos ->
            if pos.seekMod then
                seekTo (snapFloor (PianoRoll.pixelsToTicks model.pianoRollZoom pos.offsetX)) model

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
                        Basics.max 0 (snapFloor (PianoRoll.pixelsToTicks model.pianoRollZoom pos.offsetX))

                    pitch =
                        clamp PianoRoll.minPitch PianoRoll.maxPitch (PianoRoll.yToPitch pos.offsetY)

                    note =
                        { id = model.project.nextId
                        , pitch = pitch
                        , start = start
                        , duration = model.defaultNoteDuration
                        , velocity = 100
                        }
                in
                ( { model
                    | project = Data.Project.addNote model.selectedTrackId note model.project
                    , selectedNoteIds = Set.singleton note.id
                    , highlightedPitches = Set.singleton note.pitch
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
                            , highlightedPitches = Set.singleton note.pitch
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
                            , highlightedPitches = Set.singleton note.pitch
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
            case model.paneDividerDrag of
                Just d ->
                    ( { model | leftPaneWidth = clamp 260 640 (d.origWidth + round (pos.clientX - d.startClientX)) }, Cmd.none )

                Nothing ->
                    case model.sectionLoopDrag of
                        Just ld ->
                            let
                                timeline =
                                    Data.Project.timeline model.project
        
                                baseFractionalBar =
                                    Data.Timeline.ticksToFractionalBar ld.baseTicks timeline
        
                                curFractionalBar =
                                    baseFractionalBar + (pos.clientX - ld.startClientX) / toFloat model.sectionBarZoom
                            in
                            ( { model | sectionLoopDrag = Just { ld | curTicks = Basics.max 0 (Data.Timeline.fractionalBarToTicks curFractionalBar timeline) } }, Cmd.none )
        
                        Nothing ->
                                    case model.sectionResizeDrag of
                                        Just d ->
                                            let
                                                deltaBars =
                                                    round ((pos.clientX - d.startClientX) / toFloat model.sectionBarZoom)
                                            in
                                            ( { model | sectionResizeDrag = Just { d | curLengthBars = clamp 1 64 (d.origLengthBars + deltaBars) } }, Cmd.none )
                        
                                        Nothing ->
                                            case model.sectionMoveDrag of
                                                Just d ->
                                                    let
                                                        accum =
                                                            d.accumDx + (pos.clientX - d.lastClientX)
                        
                                                        currentIndex =
                                                            model.project.sections
                                                                |> List.indexedMap Tuple.pair
                                                                |> List.filter (\( _, s ) -> s.id == d.sectionId)
                                                                |> List.head
                                                                |> Maybe.map Tuple.first
                                                                |> Maybe.withDefault 0
                                                    in
                                                    case SectionBar.sectionDragTargetIndex model.sectionBarZoom model.project.sections currentIndex accum of
                                                        Just ( targetIndex, remaining ) ->
                                                            ( { model
                                                                | project = Data.Project.moveSectionToIndex d.sectionId targetIndex model.project
                                                                , sectionMoveDrag = Just { d | lastClientX = pos.clientX, accumDx = remaining }
                                                              }
                                                            , Cmd.none
                                                            )
                        
                                                        Nothing ->
                                                            ( { model | sectionMoveDrag = Just { d | lastClientX = pos.clientX, accumDx = accum } }, Cmd.none )
                        
                                                Nothing ->
                                                    case model.voicingDragState of
                                                        DraggingVoicingOffsets d ->
                                                            let
                                                                rootPitch =
                                                                    Data.Voicing.anchorPitch + model.voicingPreviewRoot
                        
                                                                maxOffset =
                                                                    VoicingKeyboard.maxPitch - rootPitch
                        
                                                                dpitch =
                                                                    negate (round ((pos.clientY - d.startClientY) / toFloat VoicingKeyboard.rowHeight))
                        
                                                                newOffsets =
                                                                    Data.Voicing.shiftOffsets dpitch maxOffset d.origSelected d.origOffsets
                        
                                                                newSelected =
                                                                    Set.map (\o -> clamp 0 maxOffset (o + dpitch)) d.origSelected
                                                                        |> Set.filter (\o -> List.member o newOffsets)
                        
                                                                newPicks =
                                                                    Data.GuitarForm.shiftPicks dpitch maxOffset d.origSelected d.origPicks
                                                            in
                                                            ( { model
                                                                | project = Data.Project.updateVoicing d.index (\v -> { v | offsets = newOffsets, stringPicks = newPicks }) model.project
                                                                , voicingSelectedOffsets = newSelected
                                                              }
                                                            , Cmd.none
                                                            )
                        
                                                        NoVoicingDrag ->
                                                            case model.loopDrag of
                                                                Just ld ->
                                                                    ( { model
                                                                        | loopDrag =
                                                                            Just
                                                                                { ld
                                                                                    | curTicks =
                                                                                        Basics.max 0 (ld.baseTicks + snapRound (PianoRoll.pixelsToTicks model.pianoRollZoom (pos.clientX - ld.startClientX)))
                                                                                }
                                                                      }
                                                                    , Cmd.none
                                                                    )
                        
                                                                Nothing ->
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
            case model.paneDividerDrag of
                Just _ ->
                    ( { model | paneDividerDrag = Nothing }, Cmd.none )

                Nothing ->
                    case model.sectionLoopDrag of
                        Just ld ->
                            let
                                ( committedModel, cmd ) =
                                    commitLoopDrag { fixedTicks = ld.fixedTicks, curTicks = ld.curTicks } model
                            in
                            ( { committedModel | sectionLoopDrag = Nothing }, cmd )
        
                        Nothing ->
                                    case model.sectionResizeDrag of
                                        Just d ->
                                            ( resizeSectionBars d.sectionId d.curLengthBars { model | sectionResizeDrag = Nothing }, Cmd.none )
        
                                        Nothing ->
                                            if model.sectionMoveDrag /= Nothing then
                                                ( { model | sectionMoveDrag = Nothing }, Cmd.none )
        
                                            else if model.voicingDragState /= NoVoicingDrag then
                                                ( { model | voicingDragState = NoVoicingDrag }, Cmd.none )
        
                                            else
                                            case model.loopDrag of
                                                Just ld ->
                                                    let
                                                        ( committedModel, cmd ) =
                                                            commitLoopDrag { fixedTicks = ld.fixedTicks, curTicks = ld.curTicks } model
                                                    in
                                                    ( { committedModel | loopDrag = Nothing }, cmd )
        
                                                Nothing ->
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
                                                                    PianoRoll.pixelsToTicks model.pianoRollZoom x0
        
                                                                t1 =
                                                                    PianoRoll.pixelsToTicks model.pianoRollZoom x1
        
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

        PressedRuler pos ->
            if pos.shift then
                let
                    anchor =
                        snapRound (PianoRoll.pixelsToTicks model.pianoRollZoom pos.offsetX)
                in
                ( { model | loopDrag = Just { fixedTicks = anchor, baseTicks = anchor, startClientX = pos.clientX, curTicks = anchor } }, Cmd.none )

            else
                seekTo (snapFloor (PianoRoll.pixelsToTicks model.pianoRollZoom pos.offsetX)) model

        PressedLoopHandle isEnd clientX ->
            case model.loopRange of
                Just loop ->
                    let
                        ( fixedTicks, baseTicks ) =
                            if isEnd then
                                ( loop.startTicks, loop.endTicks )

                            else
                                ( loop.endTicks, loop.startTicks )
                    in
                    ( { model | loopDrag = Just { fixedTicks = fixedTicks, baseTicks = baseTicks, startClientX = clientX, curTicks = baseTicks } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        PressedSectionRuler pos ->
            let
                timeline =
                    Data.Project.timeline model.project

                fractionalBar =
                    pos.offsetX / toFloat model.sectionBarZoom
            in
            if pos.shift then
                let
                    anchor =
                        Data.Timeline.fractionalBarToTicks fractionalBar timeline
                in
                ( { model | sectionLoopDrag = Just { fixedTicks = anchor, baseTicks = anchor, startClientX = pos.clientX, curTicks = anchor } }, Cmd.none )

            else
                seekTo (Data.Timeline.fractionalBarToTicks fractionalBar timeline) model

        PressedSectionLoopHandle isEnd clientX ->
            case model.loopRange of
                Just loop ->
                    let
                        ( fixedTicks, baseTicks ) =
                            if isEnd then
                                ( loop.startTicks, loop.endTicks )

                            else
                                ( loop.endTicks, loop.startTicks )
                    in
                    ( { model | sectionLoopDrag = Just { fixedTicks = fixedTicks, baseTicks = baseTicks, startClientX = clientX, curTicks = baseTicks } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        PressedPaneDivider clientX ->
            ( { model | paneDividerDrag = Just { startClientX = clientX, origWidth = model.leftPaneWidth } }, Cmd.none )

        PressedSectionResizeHandle sectionId clientX ->
            let
                origLen =
                    model.project.sections
                        |> List.filter (\s -> s.id == sectionId)
                        |> List.head
                        |> Maybe.map .lengthBars
                        |> Maybe.withDefault 1
            in
            ( { model
                | selectedSectionId = Just sectionId
                , sectionResizeDrag = Just { sectionId = sectionId, startClientX = clientX, origLengthBars = origLen, curLengthBars = origLen }
              }
            , Cmd.none
            )

        PressedSectionBlock sectionId clientX ->
            ( { model
                | selectedSectionId = Just sectionId
                , sectionMoveDrag = Just { sectionId = sectionId, lastClientX = clientX, accumDx = 0 }
              }
            , Cmd.none
            )

        PressedPianoKey pitch ->
            ( { model | highlightedPitches = Set.singleton pitch }
            , Ports.toAudio (Performance.encodePreviewNote (selectedInstrumentName model) pitch)
            )

        GotKey k ->
            if List.member k.targetTag [ "INPUT", "TEXTAREA", "SELECT" ] then
                ( model, Cmd.none )

            else if model.editingVoicingIndex /= Nothing && (k.key == "ArrowUp" || k.key == "ArrowDown") then
                case model.editingVoicingIndex of
                    Just index ->
                        let
                            rootPitch =
                                Data.Voicing.anchorPitch + model.voicingPreviewRoot

                            maxOffset =
                                VoicingKeyboard.maxPitch - rootPitch

                            step =
                                if k.shift then
                                    12

                                else
                                    1

                            delta =
                                if k.key == "ArrowUp" then
                                    step

                                else
                                    negate step
                        in
                        if Set.isEmpty model.voicingSelectedOffsets then
                            ( model, Cmd.none )

                        else
                            let
                                currentVoicing =
                                    List.drop index model.project.voicings |> List.head

                                newOffsets =
                                    Data.Voicing.shiftOffsets delta maxOffset model.voicingSelectedOffsets
                                        (currentVoicing |> Maybe.map .offsets |> Maybe.withDefault [])

                                newSelected =
                                    Set.map (\o -> clamp 0 maxOffset (o + delta)) model.voicingSelectedOffsets
                                        |> Set.filter (\o -> List.member o newOffsets)

                                newPicks =
                                    Data.GuitarForm.shiftPicks delta maxOffset model.voicingSelectedOffsets
                                        (currentVoicing |> Maybe.map .stringPicks |> Maybe.withDefault Set.empty)
                            in
                            ( { model
                                | project = Data.Project.updateVoicing index (\v -> { v | offsets = newOffsets, stringPicks = newPicks }) model.project
                                , voicingSelectedOffsets = newSelected
                              }
                            , Cmd.none
                            )

                    Nothing ->
                        ( model, Cmd.none )

            else if model.editingVoicingIndex /= Nothing && (k.key == "Delete" || k.key == "Backspace") then
                case model.editingVoicingIndex of
                    Just index ->
                        let
                            currentPicks =
                                List.drop index model.project.voicings |> List.head |> Maybe.map .stringPicks |> Maybe.withDefault Set.empty
                        in
                        ( { model
                            | project =
                                Data.Project.updateVoicing index
                                    (\v ->
                                        { v
                                            | offsets = Data.Voicing.removeOffsets model.voicingSelectedOffsets v.offsets
                                            , stringPicks = Data.GuitarForm.removePicks model.voicingSelectedOffsets currentPicks
                                        }
                                    )
                                    model.project
                            , voicingSelectedOffsets = Set.empty
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( model, Cmd.none )

            else if model.editingVoicingIndex /= Nothing && k.key == "Escape" then
                ( { model | voicingSelectedOffsets = Set.empty }, Cmd.none )

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

            else if (k.ctrl || k.meta) && k.shift && (k.key == "a" || k.key == "A") then
                case model.selectedSectionId |> Maybe.andThen (\sid -> Data.Project.sectionBounds sid model.project) of
                    Just bounds ->
                        ( { model
                            | selectedNoteIds =
                                trackNotes model
                                    |> List.filter (\n -> n.start >= bounds.startTicks && n.start < bounds.endTicks)
                                    |> List.map .id
                                    |> Set.fromList
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( model, Cmd.none )

            else if (k.ctrl || k.meta) && (k.key == "a" || k.key == "A") then
                ( { model | selectedNoteIds = Set.fromList (List.map .id (trackNotes model)) }, Cmd.none )

            else if k.key == "Delete" || k.key == "Backspace" then
                ( deleteSelection model, Cmd.none )

            else if k.key == "Escape" then
                ( { model
                    | selectedNoteIds = Set.empty
                    , pendingSectionDelete = Nothing
                    , pendingTrackDelete = Nothing
                    , pendingScrapDelete = Nothing
                  }
                , Cmd.none
                )

            else if k.key == "Home" then
                seekTo 0 model

            else if k.key == "End" then
                seekTo (totalBarsFor model.project * Data.Time.ticksPerBar) model

            else if k.key == "[" then
                setLoopRangeBound True model

            else if k.key == "]" then
                setLoopRangeBound False model

            else if k.key == "n" && not model.showKeyboard then
                let
                    start =
                        Basics.max 0 (snapFloor model.playheadTicks)

                    pitch =
                        model.highlightedPitches |> Set.toList |> List.head |> Maybe.withDefault 60

                    note =
                        { id = model.project.nextId
                        , pitch = pitch
                        , start = start
                        , duration = model.defaultNoteDuration
                        , velocity = 100
                        }
                in
                ( { model
                    | project = Data.Project.addNote model.selectedTrackId note model.project
                    , selectedNoteIds = Set.singleton note.id
                    , highlightedPitches = Set.singleton note.pitch
                  }
                , Ports.toAudio (Performance.encodePreviewNote (selectedInstrumentName model) note.pitch)
                )

            else if not k.ctrl && not k.meta && not k.repeat && not model.showKeyboard && List.member k.key [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ] then
                case String.toInt k.key |> Maybe.andThen (\n -> List.drop (n - 1) model.project.sections |> List.head) of
                    Just section ->
                        case Data.Project.sectionBounds section.id model.project of
                            Just bounds ->
                                seekTo bounds.startTicks model

                            Nothing ->
                                ( model, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

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
                        ( { model | heldKeyPitches = Set.insert pitch model.heldKeyPitches }
                        , Ports.toAudio (Performance.encodePreviewNote (selectedInstrumentName model) pitch)
                        )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        ReleasedKey key ->
            case keyToPitch key of
                Just pitch ->
                    ( { model | heldKeyPitches = Set.remove pitch model.heldKeyPitches }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SelectedTrack trackId ->
            ( { model | selectedTrackId = trackId, selectedNoteIds = Set.empty, pendingTrackDelete = Nothing }, Cmd.none )

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
            if model.pendingTrackDelete == Just trackId then
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
                ( { model
                    | project = newProject
                    , selectedTrackId = newSelected
                    , selectedNoteIds = Set.empty
                    , pendingTrackDelete = Nothing
                  }
                , Cmd.none
                )

            else
                ( { model | pendingTrackDelete = Just trackId }, Cmd.none )

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
                        , bpmInput = String.fromFloat project.bpm
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

        ChangedChordInstrument raw ->
            case Data.Track.instrumentFromString raw of
                Just inst ->
                    ( { model
                        | project = Data.Project.updateChordTrack (\ct -> { ct | instrument = inst }) model.project
                        , instrumentLoad = markLoading [ raw ] model.instrumentLoad
                      }
                    , if raw == "synthLead" || Dict.get raw model.instrumentLoad == Just "ready" then
                        Cmd.none

                      else
                        Ports.toAudio (Performance.encodeLoadInstruments [ raw ])
                    )

                Nothing ->
                    ( model, Cmd.none )

        ToggledChordMute ->
            let
                newMuted =
                    not model.project.chordTrack.muted
            in
            ( { model | project = Data.Project.updateChordTrack (\ct -> { ct | muted = newMuted }) model.project }
            , Ports.toAudio (Performance.encodeSetMute (negate 1) newMuted)
            )

        ToggledVoicingEnabled ->
            let
                p =
                    model.project
            in
            ( { model | project = { p | voicingEnabled = not p.voicingEnabled } }, Cmd.none )

        ToggledGuitarFormEnabled ->
            let
                p =
                    model.project
            in
            ( { model | project = { p | guitarFormEnabled = not p.guitarFormEnabled } }, Cmd.none )

        ClickedAddVoicing name ->
            ( { model
                | project = Data.Project.addVoicing name model.project
                , voicingSelectedOffsets = Set.empty
                , voicingDragState = NoVoicingDrag
              }
            , Cmd.none
            )

        ClickedVoicingRow index ->
            let
                opening =
                    model.editingVoicingIndex /= Just index

                rootPitch =
                    Data.Voicing.anchorPitch + model.voicingPreviewRoot

                voicingOffsets =
                    List.drop index model.project.voicings |> List.head |> Maybe.map .offsets |> Maybe.withDefault []

                displayRootPitch =
                    Data.Voicing.displayRoot rootPitch voicingOffsets

                rowIndexFromTop =
                    VoicingKeyboard.maxPitch - displayRootPitch

                y =
                    toFloat (rowIndexFromTop * VoicingKeyboard.rowHeight)

                target =
                    Basics.max 0 (y - toFloat VoicingKeyboard.containerHeight / 2 + toFloat VoicingKeyboard.rowHeight / 2)

                scrollCmd =
                    if opening then
                        Task.attempt (\_ -> NoOp) (Browser.Dom.setViewportOf VoicingKeyboard.scrollId 0 target)

                    else
                        Cmd.none
            in
            ( { model
                | editingVoicingIndex =
                    if opening then
                        Just index

                    else
                        Nothing
                , pendingVoicingDelete = Nothing
                , voicingSelectedOffsets = Set.empty
                , voicingDragState = NoVoicingDrag
              }
            , scrollCmd
            )

        ChangedVoicingName index newName ->
            ( { model | project = Data.Project.updateVoicing index (\v -> { v | name = newName }) model.project }, Cmd.none )

        PressedVoicingOffset index pitch pos ->
            let
                rootPitch =
                    Data.Voicing.anchorPitch + model.voicingPreviewRoot

                offset =
                    pitch - rootPitch

                currentOffsets =
                    List.drop index model.project.voicings |> List.head |> Maybe.map .offsets |> Maybe.withDefault []
            in
            if offset < 0 && not (List.member offset currentOffsets) then
                -- root より低い空き行。offsets は常に 0 以上の不変式なので新規追加できない
                ( model, Cmd.none )

            else if pos.shift && List.member offset currentOffsets then
                -- 埋まっている行を shift クリック: 複数選択のトグルのみ。ドラッグは開始しない
                ( { model
                    | voicingSelectedOffsets =
                        if Set.member offset model.voicingSelectedOffsets then
                            Set.remove offset model.voicingSelectedOffsets

                        else
                            Set.insert offset model.voicingSelectedOffsets
                  }
                , Cmd.none
                )

            else if List.member offset currentOffsets then
                -- 埋まっている行を素クリック: 選択を維持 or 単独選択に置き換えてドラッグ開始
                let
                    sel =
                        if Set.member offset model.voicingSelectedOffsets then
                            model.voicingSelectedOffsets

                        else
                            Set.singleton offset

                    currentPicks =
                        List.drop index model.project.voicings |> List.head |> Maybe.map .stringPicks |> Maybe.withDefault Set.empty
                in
                ( { model
                    | voicingSelectedOffsets = sel
                    , voicingDragState =
                        DraggingVoicingOffsets
                            { index = index
                            , startClientY = pos.clientY
                            , origOffsets = currentOffsets
                            , origSelected = sel
                            , origPicks = currentPicks
                            }
                  }
                , Ports.toAudio (Performance.encodePreviewNote (Data.Track.instrumentToString model.project.chordTrack.instrument) pitch)
                )

            else
                -- 空いている行をクリック: offset を追加して単独選択にし、その場でドラッグを開始
                let
                    newOffsets =
                        offset :: currentOffsets

                    currentPicks =
                        List.drop index model.project.voicings |> List.head |> Maybe.map .stringPicks |> Maybe.withDefault Set.empty
                in
                ( { model
                    | project = Data.Project.updateVoicing index (\v -> { v | offsets = newOffsets }) model.project
                    , voicingSelectedOffsets = Set.singleton offset
                    , voicingDragState =
                        DraggingVoicingOffsets
                            { index = index
                            , startClientY = pos.clientY
                            , origOffsets = newOffsets
                            , origSelected = Set.singleton offset
                            , origPicks = currentPicks
                            }
                  }
                , Ports.toAudio (Performance.encodePreviewNote (Data.Track.instrumentToString model.project.chordTrack.instrument) pitch)
                )

        DoubleClickedVoicingOffset index pitch ->
            let
                rootPitch =
                    Data.Voicing.anchorPitch + model.voicingPreviewRoot

                offset =
                    pitch - rootPitch
            in
            ( { model
                | project =
                    Data.Project.updateVoicing index
                        (\v ->
                            { v
                                | offsets = List.filter ((/=) offset) v.offsets
                                , stringPicks = Data.GuitarForm.removePicks (Set.singleton offset) v.stringPicks
                            }
                        )
                        model.project
                , voicingSelectedOffsets = Set.remove offset model.voicingSelectedOffsets
              }
            , Cmd.none
            )

        PressedFretboardCell index stringIndex fret ->
            let
                rootPitch =
                    Data.Voicing.anchorPitch + model.voicingPreviewRoot

                openPitch =
                    List.drop stringIndex Data.GuitarForm.openStrings |> List.head |> Maybe.withDefault 0

                pitch =
                    openPitch + fret

                offset =
                    pitch - rootPitch

                currentOffsets =
                    List.drop index model.project.voicings |> List.head |> Maybe.map .offsets |> Maybe.withDefault []

                currentPicks =
                    List.drop index model.project.voicings |> List.head |> Maybe.map .stringPicks |> Maybe.withDefault Set.empty

                pick =
                    ( offset, stringIndex )
            in
            if offset < 0 && not (List.member offset currentOffsets) then
                -- root より低い空きセル。offsets は常に 0 以上の不変式なので新規追加できない
                ( model, Cmd.none )

            else if not (List.member offset currentOffsets) then
                -- 空きセルをクリック: 音を追加し、そのセルを青丸にしてプレビュー発音
                ( { model
                    | project =
                        Data.Project.updateVoicing index
                            (\v -> { v | offsets = offset :: v.offsets, stringPicks = Set.insert pick v.stringPicks })
                            model.project
                    , voicingSelectedOffsets = Set.singleton offset
                  }
                , Ports.toAudio (Performance.encodePreviewNote (Data.Track.instrumentToString model.project.chordTrack.instrument) pitch)
                )

            else if Set.member pick currentPicks then
                -- 青丸をクリック: そのセルだけ外して灰丸に戻す（音は消えない）
                ( { model
                    | project = Data.Project.updateVoicing index (\v -> { v | stringPicks = Set.remove pick v.stringPicks }) model.project
                    , voicingSelectedOffsets = Set.singleton offset
                  }
                , Cmd.none
                )

            else
                -- 灰丸をクリック: そのセルも青丸にする（音は増減しない、発音なし）
                ( { model
                    | project = Data.Project.updateVoicing index (\v -> { v | stringPicks = Set.insert pick v.stringPicks }) model.project
                    , voicingSelectedOffsets = Set.singleton offset
                  }
                , Cmd.none
                )

        DoubleClickedFretboardCell index stringIndex fret ->
            let
                rootPitch =
                    Data.Voicing.anchorPitch + model.voicingPreviewRoot

                openPitch =
                    List.drop stringIndex Data.GuitarForm.openStrings |> List.head |> Maybe.withDefault 0

                pitch =
                    openPitch + fret

                offset =
                    pitch - rootPitch
            in
            ( { model
                | project =
                    Data.Project.updateVoicing index
                        (\v ->
                            { v
                                | offsets = List.filter ((/=) offset) v.offsets
                                , stringPicks = Data.GuitarForm.removePicks (Set.singleton offset) v.stringPicks
                            }
                        )
                        model.project
                , voicingSelectedOffsets = Set.remove offset model.voicingSelectedOffsets
              }
            , Cmd.none
            )

        ClickedPlayVoicing index ->
            case List.drop index model.project.voicings |> List.head of
                Just voicing ->
                    let
                        pitches =
                            Data.Voicing.pitchesFor model.voicingPreviewRoot voicing

                        instrument =
                            Data.Track.instrumentToString model.project.chordTrack.instrument
                    in
                    ( model, Cmd.batch (List.map (Ports.toAudio << Performance.encodePreviewNote instrument) pitches) )

                Nothing ->
                    ( model, Cmd.none )

        ChangedVoicingPreviewRoot str ->
            ( { model | voicingPreviewRoot = String.toInt str |> Maybe.withDefault model.voicingPreviewRoot }, Cmd.none )

        ChangedVoicingPresetQuality name ->
            ( { model | voicingPresetQuality = name }, Cmd.none )

        ChangedVoicingPresetShape name ->
            ( { model | voicingPresetShape = name }, Cmd.none )

        AppliedVoicingPreset index ->
            case ( Data.VoicingPreset.qualityByLabel model.voicingPresetQuality, Data.VoicingPreset.shapeByName model.voicingPresetShape ) of
                ( Just quality, Just shape ) ->
                    ( { model
                        | project =
                            Data.Project.updateVoicing index
                                (\v -> { v | offsets = Data.VoicingPreset.offsetsFor quality shape, stringPicks = Set.empty })
                                model.project
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ClickedRemoveVoicing index ->
            if model.pendingVoicingDelete == Just index then
                ( { model
                    | project = Data.Project.removeVoicing index model.project
                    , pendingVoicingDelete = Nothing
                    , editingVoicingIndex = Nothing
                    , voicingSelectedOffsets = Set.empty
                    , voicingDragState = NoVoicingDrag
                  }
                , Cmd.none
                )

            else
                ( { model | pendingVoicingDelete = Just index }, Cmd.none )

        ClickedCopyChordText ->
            ( { model | chordCopyFeedback = True }
            , Cmd.batch
                [ Ports.copyToClipboard (Data.ChordTrack.toPlainText model.project.chordTrack)
                , Task.perform (\_ -> ResetCopyFeedback) (Process.sleep 2000)
                ]
            )

        ResetCopyFeedback ->
            ( { model | chordCopyFeedback = False }, Cmd.none )

        ClickedConvertChords ->
            let
                project =
                    model.project

                pitchTriples =
                    Data.ChordTrack.resolved (Data.Project.timeline project) project.chordTrack
                        |> List.concatMap
                            (\ev ->
                                Data.Chord.toPitchesWith (effectiveVoicings model) ev.chord
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
                , pendingSectionDelete = Nothing
              }
            , Cmd.none
            )

        ClickedAddSection ->
            ( { model | project = Data.Project.addSection model.project }, Cmd.none )

        ClickedRemoveSection sectionId ->
            if model.pendingSectionDelete == Just sectionId then
                ( { model
                    | project = Data.Project.removeSection sectionId model.project
                    , selectedSectionId =
                        if model.selectedSectionId == Just sectionId then
                            Nothing

                        else
                            model.selectedSectionId
                    , pendingSectionDelete = Nothing
                  }
                , Cmd.none
                )

            else
                ( { model | pendingSectionDelete = Just sectionId }, Cmd.none )

        ChangedSectionName sectionId newName ->
            ( { model | project = Data.Project.updateSection sectionId (\s -> { s | name = newName }) model.project }
            , Cmd.none
            )

        ChangedSectionBars sectionId raw ->
            case String.toInt raw of
                Just bars ->
                    ( resizeSectionBars sectionId bars model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ChangedSectionMemo sectionId memo ->
            ( { model | project = Data.Project.updateSection sectionId (\s -> { s | memo = memo }) model.project }
            , Cmd.none
            )

        ChangedSectionKey sectionId raw ->
            case String.toInt raw of
                Just tonic ->
                    ( { model
                        | project =
                            Data.Project.updateSection sectionId
                                (\s -> { s | key = { tonic = tonic, mode = s.key.mode } })
                                model.project
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ChangedSectionMode sectionId raw ->
            case Data.Key.modeFromString raw of
                Just mode ->
                    ( { model
                        | project =
                            Data.Project.updateSection sectionId
                                (\s -> { s | key = { tonic = s.key.tonic, mode = mode } })
                                model.project
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ChangedSectionMeter sectionId raw ->
            case Data.Meter.fromString raw of
                Just meter ->
                    ( { model | project = Data.Project.updateSection sectionId (\s -> { s | meter = meter }) model.project }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ChangedMemo raw ->
            let
                project =
                    model.project
            in
            ( { model | project = { project | memo = raw } }, Cmd.none )

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

        AppliedStrumPattern patternName ->
            case Data.StrumPattern.byName patternName of
                Just pattern ->
                    let
                        range =
                            model.selectedSectionId
                                |> Maybe.andThen (\sid -> Data.Project.sectionBounds sid model.project)
                                |> Maybe.withDefault { startTicks = 0, endTicks = model.drumFillBars * Data.Time.ticksPerBar }
                    in
                    ( { model
                        | project =
                            Data.StrumExpand.apply
                                { trackId = model.selectedTrackId
                                , startTicks = range.startTicks
                                , endTicks = range.endTicks
                                }
                                model.project.guitarFormEnabled
                                (effectiveVoicings model)
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
            ( { model | showKeyboard = not model.showKeyboard, heldKeyPitches = Set.empty }, Cmd.none )

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
                        , pendingScrapDelete = Nothing
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
                        , pendingScrapDelete = Nothing
                      }
                    , Cmd.none
                    )

        ClickedRemoveScrap scrapId ->
            if model.pendingScrapDelete == Just scrapId then
                ( { model | project = Data.Project.removeScrap scrapId model.project, pendingScrapDelete = Nothing }, Cmd.none )

            else
                ( { model | pendingScrapDelete = Just scrapId }, Cmd.none )

        ChangedScrapName scrapId newName ->
            ( { model | project = Data.Project.updateScrap scrapId (\s -> { s | name = newName }) model.project, pendingScrapDelete = Nothing }, Cmd.none )

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

        TransposedSong delta ->
            let
                withPitches =
                    Data.Project.mapNoteTrackNotes
                        (List.map (\n -> { n | pitch = clamp PianoRoll.minPitch PianoRoll.maxPitch (n.pitch + delta) }))
                        model.project

                withChords =
                    { withPitches | chordTrack = Data.ChordTrack.transpose delta withPitches.chordTrack }

                withKeys =
                    { withChords | sections = List.map (\s -> { s | key = Data.Key.transpose delta s.key }) withChords.sections }
            in
            ( { model | project = withKeys }, Cmd.none )

        TransposedSection sectionId delta ->
            case
                ( Data.Project.sectionBounds sectionId model.project
                , sectionStartBar sectionId model.project
                , List.filter (\s -> s.id == sectionId) model.project.sections |> List.head
                )
            of
                ( Just bounds, Just fromBar, Just section ) ->
                    let
                        withPitches =
                            Data.Project.mapNoteTrackNotes
                                (List.map
                                    (\n ->
                                        if n.start >= bounds.startTicks && n.start < bounds.endTicks then
                                            { n | pitch = clamp PianoRoll.minPitch PianoRoll.maxPitch (n.pitch + delta) }

                                        else
                                            n
                                    )
                                )
                                model.project

                        withChords =
                            { withPitches | chordTrack = Data.ChordTrack.transposeBars fromBar section.lengthBars delta withPitches.chordTrack }

                        withKeys =
                            { withChords
                                | sections =
                                    List.map
                                        (\s ->
                                            if s.id == sectionId then
                                                { s | key = Data.Key.transpose delta s.key }

                                            else
                                                s
                                        )
                                        withChords.sections
                            }
                    in
                    ( { model | project = withKeys }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangedInsertCount raw ->
            ( { model | insertCountInput = raw }, Cmd.none )

        InsertedBarsBeforeSection sectionId ->
            case sectionStartBar sectionId model.project of
                Just beforeBar ->
                    ( { model | project = Data.Project.insertBars { beforeBar = beforeBar, count = parseInsertCount model.insertCountInput } model.project }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        RemovedBarsFromSection sectionId ->
            case sectionStartBar sectionId model.project of
                Just fromBar ->
                    ( { model | project = Data.Project.removeBars { fromBar = fromBar, count = parseInsertCount model.insertCountInput } model.project }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        SeekTo ticks ->
            seekTo ticks model

        SeekPrevSection ->
            seekTo (prevSectionStart model) model

        SeekNextSection ->
            seekTo (nextSectionStart model) model

        ClickedChordAt ticks ->
            seekTo ticks model

        ClickedSeekSectionStart sectionId ->
            case Data.Project.sectionBounds sectionId model.project of
                Just bounds ->
                    seekTo bounds.startTicks model

                Nothing ->
                    ( model, Cmd.none )

        ToggledGhostTrack trackId ->
            ( { model
                | ghostTrackIds =
                    if Set.member trackId model.ghostTrackIds then
                        Set.remove trackId model.ghostTrackIds

                    else
                        Set.insert trackId model.ghostTrackIds
              }
            , Cmd.none
            )

        ChangedDefaultDuration raw ->
            case String.toInt raw of
                Just duration ->
                    ( { model | defaultNoteDuration = duration }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ToggledFollowPlayhead ->
            ( { model | followPlayhead = not model.followPlayhead }, Cmd.none )

        GotPianoRollViewport ticks result ->
            case result of
                Ok viewport ->
                    let
                        playheadPx =
                            PianoRoll.ticksToPixels model.pianoRollZoom ticks

                        visLeft =
                            viewport.viewport.x

                        visRight =
                            viewport.viewport.x + viewport.viewport.width
                    in
                    if playheadPx < visLeft || playheadPx > visRight then
                        ( model, Task.attempt (\_ -> NoOp) (Browser.Dom.setViewportOf PianoRoll.pianoRollScrollId (Basics.max 0 (playheadPx - 40)) 0) )

                    else
                        ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        WheelZoomedRuler w ->
            ( model, Task.attempt (GotPianoRollViewportForZoom w) (Browser.Dom.getViewportOf PianoRoll.pianoRollScrollId) )

        GotPianoRollViewportForZoom w result ->
            case result of
                Ok viewport ->
                    let
                        anchorPx =
                            viewport.viewport.x + w.offsetX

                        anchorTicks =
                            PianoRoll.pixelsToTicks model.pianoRollZoom anchorPx

                        newZoom =
                            PianoRoll.zoomStep w.deltaY model.pianoRollZoom

                        newAnchorPx =
                            PianoRoll.ticksToPixels newZoom anchorTicks

                        newScrollLeft =
                            Basics.max 0 (newAnchorPx - w.offsetX)
                    in
                    ( { model | pianoRollZoom = newZoom }
                    , Task.attempt (\_ -> NoOp) (Browser.Dom.setViewportOf PianoRoll.pianoRollScrollId newScrollLeft 0)
                    )

                Err _ ->
                    ( { model | pianoRollZoom = PianoRoll.zoomStep w.deltaY model.pianoRollZoom }, Cmd.none )

        WheelZoomedSectionBar w ->
            ( model, Task.attempt (GotSectionBarViewportForZoom w) (Browser.Dom.getViewportOf SectionBar.sectionBarScrollId) )

        GotSectionBarViewportForZoom w result ->
            let
                timeline =
                    Data.Project.timeline model.project
            in
            case result of
                Ok viewport ->
                    let
                        anchorPx =
                            viewport.viewport.x + w.offsetX

                        anchorFractionalBar =
                            anchorPx / toFloat model.sectionBarZoom

                        anchorTicks =
                            Data.Timeline.fractionalBarToTicks anchorFractionalBar timeline

                        newZoom =
                            SectionBar.regionZoomStep w.deltaY model.sectionBarZoom

                        newAnchorFractionalBar =
                            Data.Timeline.ticksToFractionalBar anchorTicks timeline

                        newAnchorPx =
                            newAnchorFractionalBar * toFloat newZoom

                        newScrollLeft =
                            Basics.max 0 (newAnchorPx - w.offsetX)
                    in
                    ( { model | sectionBarZoom = newZoom }
                    , Task.attempt (\_ -> NoOp) (Browser.Dom.setViewportOf SectionBar.sectionBarScrollId newScrollLeft 0)
                    )

                Err _ ->
                    ( { model | sectionBarZoom = SectionBar.regionZoomStep w.deltaY model.sectionBarZoom }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


dragMove : ClientPos -> DragInfo -> Model -> ( Model, Cmd Msg )
dragMove pos d model =
    let
        dticks =
            snapRound (PianoRoll.pixelsToTicks model.pianoRollZoom (pos.clientX - d.startClientX))
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
                , highlightedPitches = Set.singleton anchorPitch
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
        timeline =
            Data.Project.timeline model.project

        barBeat =
            Data.Timeline.ticksToBarBeat model.playheadTicks timeline

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

        groupStyle =
            [ style "display" "flex", style "align-items" "center", style "gap" "0.3rem" ]
    in
    div [ style "display" "flex", style "flex-direction" "column", style "height" "100vh", style "font-family" "sans-serif" ]
        [ Style.focusCss
        , div [ style "padding" "0.5rem 1rem 0 1rem", style "flex" "0 0 auto" ]
            [ h1 [ style "font-size" "1.3rem", style "margin" "0 0 0.3rem 0" ] [ text "音書き otogaki" ]
            , div [ style "display" "flex", style "flex-wrap" "wrap", style "gap" "0.5rem", style "align-items" "center" ]
                [ div groupStyle
                    [ button (Style.baseButton ++ [ onClick (SeekTo 0), Html.Attributes.title "曲の先頭へ", Html.Attributes.attribute "aria-label" "曲の先頭へ" ]) [ text "⏮" ]
                    , button (Style.baseButton ++ [ onClick SeekPrevSection, Html.Attributes.title "このセクションの頭へ（連打で前へ遡る）", Html.Attributes.attribute "aria-label" "前のセクションへ" ]) [ text "⏪" ]
                    , button (Style.baseButton ++ [ onClick ClickedPlay, Html.Attributes.title "再生 (Space)" ]) [ text "▶ 再生" ]
                    , button (Style.baseButton ++ [ onClick ClickedStop, Html.Attributes.title "停止 (Space)" ]) [ text "■ 停止" ]
                    , button (Style.baseButton ++ [ onClick SeekNextSection, Html.Attributes.title "次のセクションの頭へ", Html.Attributes.attribute "aria-label" "次のセクションへ" ]) [ text "⏩" ]
                    ]
                , Style.divider
                , div groupStyle
                    [ button
                        (Style.baseButton
                            ++ [ onClick ClickedUndo
                               , disabled (List.isEmpty model.undoStack)
                               , Html.Attributes.attribute "aria-label" "元に戻す"
                               , Html.Attributes.title
                                    ("元に戻す (Ctrl/Cmd+Z)"
                                        ++ (if List.isEmpty model.undoStack then
                                                ""

                                            else
                                                ": " ++ model.lastEditLabel
                                           )
                                    )
                               ]
                        )
                        [ text "↩" ]
                    , button
                        (Style.baseButton
                            ++ [ onClick ClickedRedo
                               , disabled (List.isEmpty model.redoStack)
                               , Html.Attributes.title "やり直し (Ctrl/Cmd+Shift+Z)"
                               , Html.Attributes.attribute "aria-label" "やり直し"
                               ]
                        )
                        [ text "↪" ]
                    ]
                , Style.divider
                , div groupStyle
                    [ label []
                        [ text "🔁 ループ: "
                        , Html.select [ onInput ChangedLoopMode ]
                            [ Html.option [ value "off", Html.Attributes.selected (model.loopMode == NoLoop) ] [ text "オフ" ]
                            , Html.option [ value "song", Html.Attributes.selected (model.loopMode == LoopSong) ] [ text "全体" ]
                            , Html.option [ value "section", Html.Attributes.selected (model.loopMode == LoopSection) ] [ text "セクション" ]
                            , Html.option [ value "range", Html.Attributes.selected (model.loopMode == LoopRange) ] [ text "範囲" ]
                            ]
                        ]
                    , button
                        (Style.toggleButton model.followPlayhead
                            ++ [ onClick ToggledFollowPlayhead
                               , Html.Attributes.title "プレイヘッドが画面外に出たら自動でスクロールする"
                               ]
                        )
                        [ text "📌 追従" ]
                    ]
                , Style.divider
                , div groupStyle
                    [ label []
                        [ text " BPM: "
                        , input
                            [ type_ "number"
                            , Html.Attributes.step "0.1"
                            , value model.bpmInput
                            , onInput ChangedBpm
                            , onBlur BlurredBpm
                            , style "width" "4.5rem"
                            ]
                            []
                        ]
                    , label []
                        [ text "移調: "
                        , button (Style.baseButton ++ [ onClick (TransposedSong -12), Html.Attributes.title "1オクターブ下げる" ]) [ text "-12" ]
                        , button (Style.baseButton ++ [ onClick (TransposedSong -1), Html.Attributes.title "半音下げる" ]) [ text "-1" ]
                        , button (Style.baseButton ++ [ onClick (TransposedSong 1), Html.Attributes.title "半音上げる" ]) [ text "+1" ]
                        , button (Style.baseButton ++ [ onClick (TransposedSong 12), Html.Attributes.title "1オクターブ上げる" ]) [ text "+12" ]
                        ]
                    ]
                , Style.divider
                , div groupStyle
                    [ button (Style.baseButton ++ [ onClick ClickedExport ]) [ text "JSON書出" ]
                    , button (Style.baseButton ++ [ onClick ClickedImport ]) [ text "JSON読込" ]
                    , button (Style.baseButton ++ [ onClick ClickedExportMidi ]) [ text "MIDI書出" ]
                    ]
                , Style.divider
                , div (groupStyle ++ Style.labelText)
                    [ text (stateLabel ++ " — " ++ String.fromInt barBeat.bar ++ " 小節 " ++ String.fromInt barBeat.beat ++ " 拍目") ]
                ]
            , SectionBar.view
                { select = SelectedSection
                , add = ClickedAddSection
                , remove = ClickedRemoveSection
                , rename = ChangedSectionName
                , changeBars = ChangedSectionBars
                , changeMemo = ChangedSectionMemo
                , changeKey = ChangedSectionKey
                , changeMode = ChangedSectionMode
                , changeMeter = ChangedSectionMeter
                , move = MovedSection
                , changedInsertCount = ChangedInsertCount
                , insertBefore = InsertedBarsBeforeSection
                , removeFromStart = RemovedBarsFromSection
                , seekToStart = ClickedSeekSectionStart
                , transpose = TransposedSection
                , pressedBlock = PressedSectionBlock
                , pressedResizeHandle = PressedSectionResizeHandle
                , wheelZoomed = WheelZoomedSectionBar
                , pressedRuler = PressedSectionRuler
                , pressedLoopHandle = PressedSectionLoopHandle
                }
                { pxPerBar = model.sectionBarZoom
                , loopEditable = model.loopMode == LoopRange
                , loop =
                    case model.sectionLoopDrag of
                        Just ld ->
                            Just { startTicks = Basics.min ld.fixedTicks ld.curTicks, endTicks = Basics.max ld.fixedTicks ld.curTicks }

                        Nothing ->
                            currentLoop model
                , playheadTicks = model.playheadTicks
                , ticksToPx = \ticks -> Data.Timeline.ticksToFractionalBar ticks timeline * toFloat model.sectionBarZoom
                }
                model.selectedSectionId
                model.insertCountInput
                model.project.sections
                model.pendingSectionDelete
                (model.sectionResizeDrag |> Maybe.map (\d -> { sectionId = d.sectionId, lengthBars = d.curLengthBars }))
            ]
        , div [ style "flex" "1 1 auto", style "min-height" "0", style "display" "flex", style "overflow" "hidden" ]
            [ div
                [ style "width" (String.fromInt model.leftPaneWidth ++ "px")
                , style "flex" "0 0 auto"
                , style "overflow-y" "auto"
                , style "padding" "0 1rem 1rem 1rem"
                , style "box-sizing" "border-box"
                ]
                [ textarea
                    [ value model.project.memo
                    , onInput ChangedMemo
                    , Html.Attributes.placeholder "曲全体メモ：雰囲気、参考曲、やりたいことなど"
                    , style "width" "98%"
                    , style "min-height" "2.4rem"
                    , style "margin-top" "0.4rem"
                    , style "font-family" "inherit"
                    , style "font-size" "0.85rem"
                    ]
                    []
                , Arrange.view
                    { selectTrack = SelectedTrack
                    , addTrack = ClickedAddTrack
                    , removeTrack = ClickedRemoveTrack
                    , toggleMute = ToggledMute
                    , changeInstrument = ChangedInstrument
                    , changeVolume = ChangedVolume
                    , renameTrack = ChangedTrackName
                    , toggledGhost = ToggledGhostTrack
                    }
                    (totalBarsFor model.project)
                    model.selectedTrackId
                    model.instrumentLoad
                    model.ghostTrackIds
                    model.pendingTrackDelete
                    model.project.tracks
                , ChordEditor.view
                    { changedText = ChangedChordText
                    , toggledMute = ToggledChordMute
                    , convertToTrack = ClickedConvertChords
                    , changedVolume = ChangedChordVolume
                    , clickedChord = ClickedChordAt
                    , toggledGhost = ToggledGhostTrack (negate 1)
                    , changedInstrument = ChangedChordInstrument
                    , toggledVoicingEnabled = ToggledVoicingEnabled
                    , toggledGuitarFormEnabled = ToggledGuitarFormEnabled
                    , clickedCopyText = ClickedCopyChordText
                    , clickedAddVoicing = ClickedAddVoicing
                    , clickedVoicingRow = ClickedVoicingRow
                    , changedVoicingName = ChangedVoicingName
                    , pressedVoicingOffset = PressedVoicingOffset
                    , doubleClickedVoicingOffset = DoubleClickedVoicingOffset
                    , pressedFretboardCell = PressedFretboardCell
                    , doubleClickedFretboardCell = DoubleClickedFretboardCell
                    , clickedPlayVoicing = ClickedPlayVoicing
                    , clickedRemoveVoicing = ClickedRemoveVoicing
                    , changedVoicingPreviewRoot = ChangedVoicingPreviewRoot
                    , changedVoicingPresetQuality = ChangedVoicingPresetQuality
                    , changedVoicingPresetShape = ChangedVoicingPresetShape
                    , appliedVoicingPreset = AppliedVoicingPreset
                    }
                    timeline
                    model.playheadTicks
                    (Set.member (negate 1) model.ghostTrackIds)
                    model.instrumentLoad
                    { voicings = model.project.voicings
                    , enabled = model.project.voicingEnabled
                    , guitarFormEnabled = model.project.guitarFormEnabled
                    , editingIndex = model.editingVoicingIndex
                    , pendingDelete = model.pendingVoicingDelete
                    , copyFeedback = model.chordCopyFeedback
                    , previewRootPc = model.voicingPreviewRoot
                    , presetQualityName = model.voicingPresetQuality
                    , presetShapeName = model.voicingPresetShape
                    }
                    model.project.chordTrack
                , RefAudio.view
                    { changedOffset = ChangedRefOffset
                    , blurredOffset = BlurredRefOffset
                    , changedVolume = ChangedRefVolume
                    , toggledMute = ToggledRefMute
                    }
                    model.refOffsetInput
                    model.refLoaded
                    model.project.referenceAudio
                , ScrapShelf.view
                    { addFromSelection = ClickedAddScrap
                    , place = ClickedPlaceScrap
                    , remove = ClickedRemoveScrap
                    , rename = ChangedScrapName
                    }
                    (Set.size model.selectedNoteIds)
                    model.project.scraps
                    model.pendingScrapDelete
                , div [ style "font-size" "0.75rem", style "color" "#888", style "margin-top" "0.6rem" ]
                    [ text "Space: 再生/停止（ボタンのEnterは別） ・ Ctrl/Cmd+Z: 元に戻す（Shiftでやり直し） ・ ルーラーか Ctrl/Cmd+クリック・コードをクリック: 再生位置移動 ・ Home/End: 曲頭/曲末へシーク ・ ⏮⏪⏩ かセクション編集欄の「先頭へ」: セクション/曲頭へ移動 ・ Shift+ドラッグ: 矩形選択 ・ Ctrl/Cmd+Shift+A: 選択中セクション内のノートを全選択 ・ ルーラーをshift+ドラッグ: ループ範囲を作成、ハンドルをドラッグで伸縮、[/]: ループの開始/終了を再生位置に設定 ・ ↑↓: 半音移動（Shiftでオクターブ） ・ ←→: 隣のノートを選択（Ctrl/Cmdで横移動、+Shiftで1小節） ・ n: 再生位置にノートを追加（鍵盤表示中は無効） ・ Ctrl/Cmd+C・X・V: コピー・カット・貼付 ・ Delete: 削除 ・ ダブルクリック/右クリック: ノート削除 ・ Escape: 選択解除（削除確認待ちも解除）" ]
                ]
            , div
                [ style "width" "6px"
                , style "flex" "0 0 auto"
                , style "cursor" "col-resize"
                , style "background" "#ddd"
                , Html.Events.on "mousedown" (Decode.map PressedPaneDivider (Decode.field "clientX" Decode.float))
                ]
                []
            , div
                [ style "flex" "1 1 auto"
                , style "min-width" "0"
                , style "overflow-y" "auto"
                , style "padding" "0.5rem 1rem 1rem 1rem"
                , style "box-sizing" "border-box"
                ]
                [ div [ style "margin-top" "0", style "font-size" "0.9rem" ]
                    [ text ("編集中: " ++ selectedTrackName ++ selectionInfo) ]
                , let
                    durationSelect =
                        div [ style "margin-top" "0.5rem", style "display" "flex", style "align-items" "center", style "gap" "0.4rem" ]
                            [ span [ style "font-size" "0.85rem" ] [ text "音価（新規配置時の長さ）: " ]
                            , Html.select [ onInput ChangedDefaultDuration ]
                                (List.map
                                    (\( ticks, label_ ) ->
                                        Html.option
                                            [ value (String.fromInt ticks)
                                            , Html.Attributes.selected (ticks == model.defaultNoteDuration)
                                            ]
                                            [ text label_ ]
                                    )
                                    [ ( Data.Time.ticksPerSixteenth, "16分音符" )
                                    , ( Data.Time.ticksPerSixteenth * 2, "8分音符" )
                                    , ( Data.Time.ticksPerSixteenth * 3, "付点8分音符" )
                                    , ( Data.Time.ticksPerSixteenth * 4, "4分音符" )
                                    , ( Data.Time.ticksPerSixteenth * 8, "2分音符" )
                                    , ( Data.Time.ticksPerSixteenth * 16, "全音符" )
                                    ]
                                )
                            ]

                    pianoRollView =
                        div []
                            [ durationSelect
                            , PianoRoll.view
                                { pressedEmpty = PressedEmptyCell
                            , pressedNote = PressedNote
                            , doubleClickedNote = DoubleClickedNote
                            , rightClickedNote = RightClickedNote
                            , pressedRuler = PressedRuler
                            , pressedLoopHandle = PressedLoopHandle
                            , pressedKey = PressedPianoKey
                            , wheelZoomedRuler = WheelZoomedRuler
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
                            , highlightedPitch = Set.union model.highlightedPitches model.heldKeyPitches
                            , scalePitchClasses = Data.Key.scalePitchClasses (Data.Timeline.keyAt (scaleReferenceTicks model) timeline)
                            , loop =
                                case model.loopDrag of
                                    Just ld ->
                                        Just { startTicks = Basics.min ld.fixedTicks ld.curTicks, endTicks = Basics.max ld.fixedTicks ld.curTicks }

                                    Nothing ->
                                        currentLoop model
                            , loopEditable = model.loopMode == LoopRange
                            , waveform =
                                if Array.isEmpty model.refPeaks then
                                    Nothing

                                else
                                    Just
                                        { peaks = model.refPeaks
                                        , peakDt = model.refPeakDt
                                        , secsPerTick = 60 / (model.project.bpm * toFloat Data.Time.ppq)
                                        , offsetMs = model.project.referenceAudio.offsetMs
                                        }
                            , ghostNoteGroups = ghostNoteGroups model timeline
                            , pxPerSixteenth = model.pianoRollZoom
                            }
                            ]
                  in
                  case selectedTrackKind model of
                    Just (DrumTrack _) ->
                        div []
                            [ button (Style.baseButton ++ [ onClick ToggledDrumView, style "margin-top" "0.5rem" ])
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
                                    timeline
                                    (totalBarsFor model.project)
                                    model.drumFillBars
                                    (trackNotes model)
                                    model.playheadTicks
                            ]

                    _ ->
                        if selectedTrackInstrument model == Just Data.Track.AcousticGuitar then
                            div []
                                [ div
                                    [ style "display" "flex"
                                    , style "gap" "2px"
                                    , style "margin-top" "0.5rem"
                                    , style "flex-wrap" "wrap"
                                    , style "align-items" "center"
                                    ]
                                    (span [ style "font-size" "0.75rem", style "color" "#888" ] [ text "ストローク生成:" ]
                                        :: List.map
                                            (\p ->
                                                button
                                                    (Style.baseButton
                                                        ++ [ onClick (AppliedStrumPattern p.name)
                                                           , Html.Attributes.title "選択中セクション（なければ曲全体）にコード進行に沿ったストロークを生成し、既存ノートと置換する"
                                                           ]
                                                    )
                                                    [ text p.name ]
                                            )
                                            Data.StrumPattern.patterns
                                    )
                                , pianoRollView
                                ]

                        else
                            pianoRollView
                , Keyboard.view
                    { pressedKey = PressedPianoKey
                    , toggled = ToggledKeyboard
                    }
                    (Set.union model.highlightedPitches model.heldKeyPitches)
                    model.showKeyboard
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.fromAudio (AudioMsg.decode >> GotAudio)
        , if model.dragState /= NoDrag || model.rubberBand /= Nothing || model.loopDrag /= Nothing || model.voicingDragState /= NoVoicingDrag || model.sectionResizeDrag /= Nothing || model.sectionMoveDrag /= Nothing || model.sectionLoopDrag /= Nothing || model.paneDividerDrag /= Nothing then
            Browser.Events.onMouseMove (Decode.map DraggedTo clientPosDecoder)

          else
            Sub.none
        , Browser.Events.onMouseUp (Decode.succeed ReleasedDrag)
        , Browser.Events.onKeyDown (Decode.map GotKey keyEventDecoder)
        , Browser.Events.onKeyUp (Decode.map ReleasedKey (Decode.field "key" Decode.string))
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
