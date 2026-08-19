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
import Data.Chord.Format as ChordFormat
import Data.Chord.Parser as ChordParser
import Data.ChordSheet
import Data.StrumExpand
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
import Html.Attributes exposing (classList, disabled, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Midi.Encode
import Ports
import Set exposing (Set)
import View.Style as Style
import View.Theme as Theme
import View.VoicingKeyboard as VoicingKeyboard
import Process
import Task
import View.Arrange as Arrange
import View.ChordBlocks as ChordBlocks
import View.ChordEditor as ChordEditor
import View.DrumEditor as DrumEditor
import View.FormPicker as FormPicker
import View.Keyboard as Keyboard
import View.Modal as Modal
import View.Palette as Palette
import View.PianoRoll as PianoRoll
import View.RefAudio as RefAudio
import View.ScaleGuide as ScaleGuide
import View.ScrapShelf as ScrapShelf
import View.HelpPanel as HelpPanel
import View.SectionBar as SectionBar
import View.Toast as Toast


type PlayState
    = Idle
    | Playing


type DragMode
    = MoveNote
    | ResizeLeft
    | ResizeRight


type alias DragInfo =
    { anchorId : Int
    , mode : DragMode
    , startClientX : Float
    , startClientY : Float
    , origNotes : List Data.Note.Note
    , lastPreviewPitch : Int
    , isTouch : Bool
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


{-| コードトークンのドラッグ移動中の状態。origText/origKeys はドラッグ開始時のスナップショットで、
moveTokens は毎 move でこのスナップショットから再計算する（累積誤差を防ぐため）。anchorCenterTicksは
トークンの開始tickではなく中心tick（startTicks + durationTicks // 2）。開始tickを基準にするとticksToBarBeatの
floor丸めの影響で、右方向の小節切り替えにはほぼ1小節分の移動が必要なのに対し、左方向はわずかな移動で
切り替わってしまい左右非対称になる。中心tickを基準にすることで、左右とも半小節分の移動で切り替わる
ようにしている。
-}
type alias ChordDrag =
    { anchorKey : ( Int, Int )
    , anchorCenterTicks : Int
    , startClientX : Float
    , startClientY : Float
    , origText : String
    , origKeys : Set ( Int, Int )
    , lastDeltaBars : Int
    , ghostLabel : String
    }


type alias VoicingDragInfo =
    { index : Int
    , startClientX : Float
    , startClientY : Float
    , origOffsets : List Int
    , origSelected : Set Int
    , origPicks : Data.GuitarForm.StringPicks
    }


type VoicingDragState
    = NoVoicingDrag
    | DraggingVoicingOffsets VoicingDragInfo


{-| ベロシティレーンのバードラッグ中の状態。origVelocities はドラッグ開始時の
(noteId, velocity) スナップショットで、每 move でここから再計算する（累積誤差を防ぐ）。-}
type alias VelocityDrag =
    { startClientY : Float
    , origVelocities : Dict Int Int
    }


type alias ClientPos =
    { clientX : Float, clientY : Float, alt : Bool }


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
    , pendingNoteDrag : Maybe DragInfo
    , pendingEmptyTouch : Maybe { offsetX : Float, offsetY : Float, clientX : Float, clientY : Float }
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
    , chordBlockView : Bool
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
    , metronomeEnabled : Bool
    , metronomeVolume : Int
    , loopRange : Maybe Performance.Loop
    , loopDrag : Maybe LoopDrag
    , lastEditLabel : String
    , pendingSectionDelete : Maybe Int
    , pendingTrackDelete : Maybe Int
    , pendingScrapDelete : Maybe Int
    , pendingNewProject : Bool
    , editingVoicingIndex : Maybe Int
    , pendingVoicingDelete : Maybe Int
    , chordCopyFeedback : Bool
    , chordSheetDraft : Maybe String
    , voicingPreviewRoot : Int
    , voicingPresetQuality : String
    , voicingPresetShape : String
    , voicingSelectedOffsets : Set Int
    , voicingDragState : VoicingDragState
    , pendingVoicingDrag : Maybe VoicingDragInfo
    , pianoRollZoom : Int
    , gridUnit : Data.Time.GridUnit
    , pianoRollScrollX : Float
    , pianoRollViewportWidth : Maybe Float
    , sectionResizeDrag : Maybe { sectionId : Int, startClientX : Float, origLengthBars : Int, curLengthBars : Int }
    , sectionMoveDrag : Maybe { sectionId : Int, lastClientX : Float, accumDx : Float, moved : Bool, wasSelected : Bool }
    , sectionBarZoom : Int
    , sectionLoopDrag : Maybe LoopDrag
    , viewRangeDrag : Maybe { startClientX : Float, pressOffsetX : Float, origScrollX : Float, moved : Bool }
    , leftPaneWidth : Int
    , leftPaneCollapsed : Bool
    , paneDividerDrag : Maybe { startClientX : Float, origWidth : Int }
    , chordProgressionModalOpen : Bool
    , hoveredNote : Maybe { note : Data.Note.Note, x : Float, y : Float }
    , hoveredFretCell : Maybe { pitch : Int, interval : Int, x : Float, y : Float }
    , selectedChordKeys : Set ( Int, Int )
    , chordDrag : Maybe ChordDrag
    , pendingChordDrag : Maybe ChordDrag
    , chordRubberBand : Maybe RubberBand
    , formPicker : Maybe { key : Data.ChordTrack.TokenKey, draft : String, tab : FormPicker.Tab }
    , velocityDrag : Maybe VelocityDrag
    , wavExportModalOpen : Bool
    , wavExportState : WavExportState
    , tool : PianoRoll.Tool
    , cutGuideTicks : Maybe Int
    , themePreference : Theme.ThemePreference
    , guideKeyOverride : Maybe Data.Key.Key
    , windowSize : { width : Int, height : Int }
    , narrowPane : NarrowPane
    , headerMenuOpen : Bool
    , sectionEditPanelOpen : Maybe Bool
    , touchMode : TouchMode
    , touchSnapOff : Bool
    , longPressToken : Int
    , longPress : Maybe { token : Int, target : LongPressTarget }
    , touchTooltipToken : Int
    , lastNoteTap : Maybe { noteId : Int, time : Float, clientX : Float, clientY : Float }
    , toast : Maybe Toast.Toast
    , toastCounter : Int
    , chordSheetError : Maybe Data.ChordSheet.ParseError
    , helpModalOpen : Bool
    , dragCursor : Maybe { x : Float, y : Float }
    }


{-| 狭画面（isNarrowLayout）でどちらのペインを全幅表示するかのタブ選択。NarrowMain = 右ペイン（ピアノロール等の編集エリア）相当でデフォルト、
NarrowSide = 左ペイン（トラック一覧・コード編集・素材）相当。
-}
type NarrowPane
    = NarrowMain
    | NarrowSide


{-| タッチ環境ではShift/Ctrlなどの修飾キーを物理ボタンで押せないので、グローバルな代替モードとして持たせる。
seekModとshiftは既存コードで常に排他分岐なので、この型も排他にする。Altのスナップ無効化は独立なので
touchSnapOff（Modelの別フィールド）で持つ。
-}
type TouchMode
    = TouchNormal
    | TouchSelect
    | TouchSeek


{-| 長押し削除の対象。既存の削除ロジック（RightClickedNote/DoubleClickedVoicingOffset/removeDrumNoteAt）を
引き継ぐのに必要な最小限の情報だけを持たせる。
-}
type LongPressTarget
    = LongPressNote Int
    | LongPressVoicingOffset Int Int
    | LongPressDrumNote { pitch : Int, tick : Int }


{-| ループ範囲ドラッグの進行状態。fixedTicks = 動かさない側の端、
baseTicks = ドラッグ開始時点の可動側の端。ピアノロールのルーラーと
セクションバーの両方で同じ形を使う。
-}
type alias LoopDrag =
    { fixedTicks : Int, baseTicks : Int, startClientX : Float, curTicks : Int }


type LoopMode
    = NoLoop
    | LoopSong
    | LoopSection
    | LoopRange


type WavExportState
    = WavExportIdle
    | WavExportRendering
    | WavExportFailed String


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
    | PressedEmptyCell { offsetX : Float, offsetY : Float, clientX : Float, clientY : Float, shift : Bool, seekMod : Bool, isTouch : Bool }
    | CanceledNotePress
    | PressedNote Int PianoRoll.ResizeHandle { clientX : Float, clientY : Float, shift : Bool, isTouch : Bool, timeStamp : Float }
    | PressedChordToken ( Int, Int ) { clientX : Float, clientY : Float, shift : Bool }
    | PressedChordLane { offsetX : Float, offsetY : Float, clientX : Float, clientY : Float, shift : Bool, seekMod : Bool }
    | DoubleClickedNote Int
    | RightClickedNote Int
    | DraggedTo ClientPos
    | DraggedOverChordBar Int
    | ReleasedDrag
    | PressedRuler { offsetX : Float, clientX : Float, shift : Bool }
    | PressedPianoKey Int
    | PressedVoicingKeyboardKey Int
    | GotKey KeyEvent
    | ReleasedKey String
    | SelectedTrack Int
    | ClickedAddTrack
    | ScrolledPianoRoll { scrollLeft : Float, clientWidth : Float }
    | GotPianoRollViewportMeasured (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ResizedWindow Int Int
    | SelectedNarrowPane NarrowPane
    | ToggledHeaderMenu
    | ToggledSectionEditPanel
    | ToggledLeftPaneCollapsed
    | ClickedRemoveTrack Int
    | ToggledMute Int
    | ChangedInstrument Int String
    | ChangedVolume Int String
    | ClickedNewProject
    | ClickedExport
    | ClickedImport
    | GotImportFile File.File
    | GotImportContent String
    | ChangedChordSheetText String
    | ChangedChordInstrument String
    | ChangedChordRhythm (Maybe String)
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
    | ClickedResetVoicing Int
    | PressedVoicingOffset Int Int { clientX : Float, clientY : Float, shift : Bool, isTouch : Bool, timeStamp : Float }
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
    | PressedDrumCell { pitch : Int, tick : Int, offsetX : Float, offsetY : Float, clientX : Float, clientY : Float, shift : Bool, isTouch : Bool, timeStamp : Float }
    | RightClickedDrumCell { pitch : Int, tick : Int }
    | DoubleClickedDrumCell { pitch : Int, tick : Int }
    | AppliedDrumPreset String
    | ChangedDrumFillBars String
    | ClickedExportMidi
    | ClickedExportWav
    | ClosedWavExportModal
    | ConfirmedExportWav Bool
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
    | ToggledChordBlockView
    | HoveredNote Data.Note.Note Float Float
    | UnhoveredNote
    | HoveredFretCell { pitch : Int, interval : Int, x : Float, y : Float }
    | UnhoveredFretCell
    | ChangedMemo String
    | ChangedSectionKey Int String
    | ChangedSectionMode Int String
    | ChangedSectionMeter Int String
    | TransposedSong Int
    | TransposedSection Int Int
    | ChangedInsertCount String
    | InsertedBarsAtPlayhead
    | RemovedBarsAtPlayhead
    | SeekTo Int
    | SeekPrevSection
    | SeekNextSection
    | ClickedChordAt Int
    | ClickedSeekSectionStart Int
    | ToggledGhostTrack Int
    | ChangedDefaultDuration String
    | ChangedGridUnit String
    | ToggledFollowPlayhead
    | ToggledMetronome
    | ChangedMetronomeVolume String
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
    | ToggledChordProgressionModal
    | DoubleClickedChordToken Data.ChordTrack.TokenKey
    | DoubleClickedChordStripAt Int
    | ClosedFormPicker
    | ChoseChordForm Data.ChordTrack.TokenKey Data.GuitarForm.Candidate
    | ChoseVoicingShape Data.ChordTrack.TokenKey Data.VoicingPreset.Shape
    | ClearedChordVoicing Data.ChordTrack.TokenKey
    | ChangedFormPickerDraft String
    | SubmittedFormPickerDraft
    | SelectedFormPickerTab FormPicker.Tab
    | PressedVelocityBar Int { clientX : Float, clientY : Float }
    | PressedCutAt { offsetX : Float, offsetY : Float, clientX : Float, clientY : Float, shift : Bool, seekMod : Bool, isTouch : Bool }
    | MovedCutGuide { offsetX : Float }
    | ClearedCutGuide
    | SelectedTool PianoRoll.Tool
    | SelectedTouchMode TouchMode
    | ToggledTouchSnapOff
    | LongPressFired Int
    | ExpiredTouchTooltip Int
    | ClickedThemeToggle
    | ChangedGuideKeyTonic String
    | ChangedGuideKeyMode String
    | DismissedToast Int
    | ToggledHelpModal
    | NoOp


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        project =
            flags
                |> Decode.decodeValue ProjectJson.decoder
                |> Result.withDefault Data.Project.demo

        restoredSelectedTrackId =
            flags
                |> Decode.decodeValue ProjectJson.selectedTrackIdDecoder
                |> Result.withDefault Nothing
                |> Maybe.andThen (\tid -> if validTrackId project tid then Just tid else Nothing)

        restoredThemePreference =
            flags
                |> Decode.decodeValue (Decode.field "theme" Decode.string)
                |> Result.toMaybe
                |> Maybe.andThen Theme.themeFromString
                |> Maybe.withDefault Theme.SystemTheme
    in
    ( { project = project
      , playState = Idle
      , playheadTicks = 0
      , selectedTrackId = Maybe.withDefault (firstTrackId project) restoredSelectedTrackId
      , dragState = NoDrag
      , pendingNoteDrag = Nothing
      , pendingEmptyTouch = Nothing
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
      , chordBlockView = False
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
      , metronomeEnabled = False
      , metronomeVolume = 100
      , loopRange = Nothing
      , loopDrag = Nothing
      , lastEditLabel = ""
      , pendingSectionDelete = Nothing
      , pendingTrackDelete = Nothing
      , pendingScrapDelete = Nothing
      , pendingNewProject = False
      , editingVoicingIndex = Nothing
      , pendingVoicingDelete = Nothing
      , chordCopyFeedback = False
      , chordSheetDraft = Nothing
      , voicingPreviewRoot = 0
      , voicingPresetQuality = "maj7"
      , voicingPresetShape = "クローズド"
      , voicingSelectedOffsets = Set.empty
      , voicingDragState = NoVoicingDrag
      , pendingVoicingDrag = Nothing
      , pianoRollZoom = PianoRoll.defaultPxPerSixteenth
      , gridUnit = Data.Time.Sixteenth
      , pianoRollScrollX = 0
      , pianoRollViewportWidth = Nothing
      , sectionResizeDrag = Nothing
      , sectionMoveDrag = Nothing
      , sectionBarZoom = SectionBar.defaultRegionPxPerBar
      , sectionLoopDrag = Nothing
      , viewRangeDrag = Nothing
      , leftPaneWidth = 380
      , leftPaneCollapsed = False
      , paneDividerDrag = Nothing
      , chordProgressionModalOpen = False
      , hoveredNote = Nothing
      , hoveredFretCell = Nothing
      , selectedChordKeys = Set.empty
      , chordDrag = Nothing
      , pendingChordDrag = Nothing
      , chordRubberBand = Nothing
      , formPicker = Nothing
      , velocityDrag = Nothing
      , wavExportModalOpen = False
      , wavExportState = WavExportIdle
      , tool = PianoRoll.PointerTool
      , cutGuideTicks = Nothing
      , themePreference = restoredThemePreference
      , guideKeyOverride = Nothing
      , windowSize = { width = 0, height = 0 }
      , narrowPane = NarrowMain
      , headerMenuOpen = False
      , sectionEditPanelOpen = Nothing
      , touchMode = TouchNormal
      , touchSnapOff = False
      , longPressToken = 0
      , longPress = Nothing
      , touchTooltipToken = 0
      , lastNoteTap = Nothing
      , toast = Nothing
      , toastCounter = 0
      , chordSheetError = Nothing
      , helpModalOpen = False
      , dragCursor = Nothing
      }
    , Task.perform (\vp -> ResizedWindow (round vp.viewport.width) (round vp.viewport.height)) Browser.Dom.getViewport
    )


{-| model.gridUnit に追従した四捨五入スナップ。Data.Time.snapRound のグリッド単位を model から自動で引く。
-}
snapRound : Model -> Int -> Int
snapRound model ticks =
    Data.Time.snapRound (Data.Time.gridTicks model.gridUnit) ticks


{-| model.gridUnit に追従した切り下げスナップ。Data.Time.snapFloor のグリッド単位を model から自動で引く。
-}
snapFloor : Model -> Int -> Int
snapFloor model ticks =
    Data.Time.snapFloor (Data.Time.gridTicks model.gridUnit) ticks


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


{-| ポインターダウンを即座にドラッグ状態（と全画面オーバーレイ）にすると、ダブルクリック/右クリックと
同一要素で共存する操作が壊れるため、pendingNoteDragと同じ考え方で保留状態を経由してしきい値判定を行う。
ノート以外のpendingXxxDragもこの判定を共有する。
-}
dragThreshold : Float
dragThreshold =
    3


{-| タッチでは指先の位置が3px程度容易にブレ、長押し削除（500ms）が完走する前にdragStateへの昇格でlongPressがdisarmされてしまうため、マウスより広いしきい値を使う。
-}
touchDragThreshold : Float
touchDragThreshold =
    10


exceedsDragThreshold : { r | startClientX : Float, startClientY : Float } -> { p | clientX : Float, clientY : Float } -> Bool
exceedsDragThreshold info pos =
    let
        dx =
            pos.clientX - info.startClientX

        dy =
            pos.clientY - info.startClientY
    in
    sqrt (dx * dx + dy * dy) >= dragThreshold


{-| タッチでの長押し削除用タイマーを開始する。500ms後にLongPressFiredが発火し、その時点でlongPressのトークンが
一致していれば（disarmされていなければ）削除を実行する。ResetCopyFeedbackと同型のProcess.sleep+Task.performパターン。
-}
armLongPress : LongPressTarget -> Model -> ( Model, Cmd Msg )
armLongPress target model =
    let
        newToken =
            model.longPressToken + 1
    in
    ( { model | longPressToken = newToken, longPress = Just { token = newToken, target = target } }
    , Task.perform (\_ -> LongPressFired newToken) (Process.sleep 500)
    )


{-| ノートをドラッグ移動/リサイズ状態にする共通処理。PressedNote の非Shift分岐と、PressedEmptyCell が
既存ノート上のクリックを新規作成ではなく選択+移動にフォールバックする場合の両方から呼ばれる。
-}
pressNoteForDrag : Data.Note.Note -> PianoRoll.ResizeHandle -> { r | clientX : Float, clientY : Float, isTouch : Bool } -> Model -> ( Model, Cmd Msg )
pressNoteForDrag note handle pos model =
    let
        sel =
            if Set.member note.id model.selectedNoteIds then
                model.selectedNoteIds

            else
                Set.singleton note.id

        origs =
            trackNotes model
                |> List.filter (\n -> Set.member n.id sel)
    in
    ( { model
        | selectedNoteIds = sel
        , highlightedPitches = Set.singleton note.pitch
        , pendingNoteDrag =
            Just
                { anchorId = note.id
                , mode =
                    case handle of
                        PianoRoll.NoResize ->
                            MoveNote

                        PianoRoll.ResizeLeft ->
                            ResizeLeft

                        PianoRoll.ResizeRight ->
                            ResizeRight
                , startClientX = pos.clientX
                , startClientY = pos.clientY
                , origNotes = origs
                , lastPreviewPitch = note.pitch
                , isTouch = pos.isTouch
                }
      }
    , Ports.toAudio (Performance.encodePreviewNote (selectedInstrumentName model) note.pitch)
    )


{-| 空セルへのノート新規配置のコア処理。addNote + 選択 + ハイライト + プレビュー音を行う。タッチのタップ確定
（ReleasedDrag）とマウスのpointerdown即座配置（PressedEmptyCell）の両方から呼ばれる。
-}
insertNoteAt : { r | offsetX : Float, offsetY : Float } -> Model -> ( Model, Cmd Msg, Data.Note.Note )
insertNoteAt pos model =
    let
        exactTick =
            PianoRoll.pixelsToTicks model.pianoRollZoom pos.offsetX

        hitPitch =
            PianoRoll.yToPitch (isTouchLayout model) pos.offsetY

        start =
            Basics.max 0 (snapFloor model exactTick)

        pitch =
            clamp PianoRoll.minPitch PianoRoll.maxPitch hitPitch

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
    , note
    )


{-| マウス用: insertNoteAt に続けて pendingNoteDrag を ResizeRight で武装し、押したまま右に伸ばして
配置できるようにする（タッチはこの武装をせず pointerup で確定するだけ）。
-}
placeNoteWithResizePending : { r | offsetX : Float, offsetY : Float, clientX : Float, clientY : Float } -> Model -> ( Model, Cmd Msg )
placeNoteWithResizePending pos model =
    let
        ( placed, cmd, note ) =
            insertNoteAt pos model
    in
    ( { placed
        | pendingNoteDrag =
            Just
                { anchorId = note.id
                , mode = ResizeRight
                , startClientX = pos.clientX
                , startClientY = pos.clientY
                , origNotes = [ note ]
                , lastPreviewPitch = note.pitch
                , isTouch = False
                }
      }
    , cmd
    )


{-| ドラムグリッドのセル（pitch, グリッドにスナップした tick）に該当するノートを探す。
-}
findDrumNoteAt : { pitch : Int, tick : Int } -> Model -> Maybe Data.Note.Note
findDrumNoteAt { pitch, tick } model =
    let
        grid =
            Data.Time.gridTicks model.gridUnit
    in
    trackNotes model
        |> List.filter (\n -> n.pitch == pitch && n.start >= tick && n.start < tick + grid)
        |> List.head


{-| ドラムセル上のノートを削除し、選択からも除く。右クリック/ダブルクリック両方から共通で呼ばれる。
-}
removeDrumNoteAt : { pitch : Int, tick : Int } -> Model -> ( Model, Cmd Msg )
removeDrumNoteAt target model =
    case findDrumNoteAt target model of
        Just note ->
            ( { model
                | project = Data.Project.removeNote model.selectedTrackId note.id model.project
                , selectedNoteIds = Set.remove note.id model.selectedNoteIds
              }
            , Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )


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


{-| 保存データの selectedTrackId を復元していいかの検証。-1（コードトラック）は常に許可し、
それ以外は project.tracks に実在する id のみ許可する（削除済みトラックを指さないように）。
-}
validTrackId : Project -> Int -> Bool
validTrackId project trackId =
    trackId == Data.ChordTrack.trackId || List.any (\t -> t.id == trackId) project.tracks


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
        ChangedChordSheetText _ ->
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

        DraggedOverChordBar _ ->
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
        , velocityDrag = Nothing
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
        ClickedNewProject ->
            "新規プロジェクト"

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

        PressedCutAt _ ->
            "ノートカット"

        TransposedSong _ ->
            "移調"

        TransposedSection _ _ ->
            "セクション移調"

        InsertedBarsAtPlayhead ->
            "小節挿入"

        RemovedBarsAtPlayhead ->
            "小節削除"

        ChangedChordRhythm _ ->
            "コードリズム変更"

        _ ->
            "編集"



{-| ドラッグ中のトークンをアンカーの横移動量から小節差に換算して applyChordDragDelta を呼ぶ。ライン表示専用
（ブロック表示は座標が線形でないため DraggedOverChordBar で直接小節を渡す）。
-}
chordDragMove : ClientPos -> ChordDrag -> Model -> ( Model, Cmd Msg )
chordDragMove pos cd model =
    let
        timeline =
            Data.Project.timeline model.project

        dxTicks =
            PianoRoll.pixelsToTicks model.pianoRollZoom (pos.clientX - cd.startClientX)

        targetBar =
            (Data.Timeline.ticksToBarBeat (Basics.max 0 (cd.anchorCenterTicks + dxTicks)) timeline).bar - 1

        deltaBars =
            targetBar - Tuple.first cd.anchorKey
    in
    applyChordDragDelta deltaBars cd model


{-| deltaBars（アンカー小節からの差）を適用して moveTokens を呼ぶ。毎回 origText/origKeys（ドラッグ開始時の
スナップショット）から再計算するので累積誤差は出ない。deltaBars が前回と同じなら何もしない。ライン表示
（chordDragMove）・ブロック表示（DraggedOverChordBar）の両方から共有する。
-}
applyChordDragDelta : Int -> ChordDrag -> Model -> ( Model, Cmd Msg )
applyChordDragDelta deltaBars cd model =
    if deltaBars == cd.lastDeltaBars then
        ( model, Cmd.none )

    else
        let
            timeline =
                Data.Project.timeline model.project

            origTrack =
                model.project.chordTrack

            moveResult =
                Data.ChordTrack.moveTokens timeline deltaBars cd.origKeys { origTrack | text = cd.origText }
        in
        ( { model
            | project = Data.Project.updateChordTrack (\ct -> { ct | text = moveResult.track.text }) model.project
            , selectedChordKeys = moveResult.movedKeys
            , chordDrag = Just { cd | lastDeltaBars = deltaBars }
          }
        , Cmd.none
        )


{-| 矩形選択を開始する共通ヘルパー。空セルの Shift+mousedown（ポインタ/カット両ツール共通）から呼ぶ。
-}
startRubberBand : { a | offsetX : Float, offsetY : Float, clientX : Float, clientY : Float } -> Model -> Model
startRubberBand pos model =
    { model
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


{-| ラバーバンドのドラッグ原点/現在点から矩形（x, y, w, h）へ変換する共通ヘルパー。
PianoRoll用・Drum用の両方で使う。
-}
rubberBandRect : Maybe RubberBand -> Maybe { x : Float, y : Float, w : Float, h : Float }
rubberBandRect band =
    band
        |> Maybe.map
            (\rb ->
                { x = Basics.min rb.originX rb.curX
                , y = Basics.min rb.originY rb.curY
                , w = abs (rb.curX - rb.originX)
                , h = abs (rb.curY - rb.originY)
                }
            )


legacyDraggedTo : ClientPos -> Model -> ( Model, Cmd Msg )
legacyDraggedTo pos model =
    case model.viewRangeDrag of
        Just d ->
            let
                dx =
                    pos.clientX - d.startClientX
            in
            if not d.moved && abs dx < 3 then
                ( model, Cmd.none )

            else
                let
                    timeline =
                        Data.Project.timeline model.project

                    origStartTicks =
                        PianoRoll.pixelsToTicks model.pianoRollZoom d.origScrollX

                    dBar =
                        dx / toFloat model.sectionBarZoom

                    newStartTicks =
                        Data.Timeline.fractionalBarToTicks (Data.Timeline.ticksToFractionalBar origStartTicks timeline + dBar) timeline

                    newScrollX =
                        Basics.max 0 (PianoRoll.ticksToPixels model.pianoRollZoom newStartTicks)
                in
                ( { model | viewRangeDrag = Just { d | moved = True } }
                , Task.attempt (\_ -> NoOp) (Browser.Dom.setViewportOf PianoRoll.pianoRollScrollId newScrollX 0)
                )

        Nothing ->
            case model.paneDividerDrag of
                Just d ->
                    ( { model | leftPaneWidth = clamp 260 640 (d.origWidth + round (pos.clientX - d.startClientX)) }, Cmd.none )

                Nothing ->
                    case model.velocityDrag of
                    Just vd ->
                        let
                            dv =
                                round ((vd.startClientY - pos.clientY) * 127 / toFloat PianoRoll.velocityLaneHeight)

                            apply =
                                List.map
                                    (\n ->
                                        case Dict.get n.id vd.origVelocities of
                                            Just v0 ->
                                                { n | velocity = clamp 0 127 (v0 + dv) }

                                            Nothing ->
                                                n
                                    )
                        in
                        ( { model | project = Data.Project.mapNotes model.selectedTrackId apply model.project }, Cmd.none )

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
                                                            , sectionMoveDrag = Just { d | lastClientX = pos.clientX, accumDx = remaining, moved = True }
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
                                                                Set.map (\o -> clamp -12 maxOffset (o + dpitch)) d.origSelected
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
                                                                                    Basics.max 0 (ld.baseTicks + snapRound model (PianoRoll.pixelsToTicks model.pianoRollZoom (pos.clientX - ld.startClientX)))
                                                                            }
                                                                  }
                                                                , Cmd.none
                                                                )

                                                            Nothing ->
                                                                case model.chordDrag of
                                                                    Just cd ->
                                                                        if model.chordBlockView then
                                                                            {- ブロック表示ではセル座標が線形でないためdeltaBarsの発生源はDraggedOverChordBarのみ。
                                                                               ここでchordDragMoveを呼ぶとpointerenter直後のpointermoveがdeltaBars=0を再計算してswapを打ち消す。 -}
                                                                            ( model, Cmd.none )

                                                                        else
                                                                            chordDragMove pos cd model

                                                                    Nothing ->
                                                                        case model.chordRubberBand of
                                                                            Just crb ->
                                                                                ( { model | chordRubberBand = Just { crb | curX = crb.originX + (pos.clientX - crb.startClientX) } }, Cmd.none )

                                                                            Nothing ->
                                                                                draggedToNoteOrRubberBand pos model


{-| pendingEmptyTouch以外の保留状態（pendingNoteDrag/pendingChordDrag/pendingVoicingDrag）を順に確認し、
どれもなければlegacyReleasedDragに落とす。元々ReleasedDragハンドラ本体だったものを、pendingEmptyTouch分岐を
先頭に追加する際にネストが深くなりすぎないよう外出しした。
-}
releasedDragFallback : Model -> ( Model, Cmd Msg )
releasedDragFallback model =
    case model.pendingNoteDrag of
        Just _ ->
            ( { model | pendingNoteDrag = Nothing, longPress = Nothing }, Cmd.none )

        Nothing ->
            case model.pendingChordDrag of
                Just _ ->
                    ( { model | pendingChordDrag = Nothing, longPress = Nothing }, Cmd.none )

                Nothing ->
                    case model.pendingVoicingDrag of
                        Just _ ->
                            ( { model | pendingVoicingDrag = Nothing, longPress = Nothing }, Cmd.none )

                        Nothing ->
                            legacyReleasedDrag { model | longPress = Nothing }


legacyReleasedDrag : Model -> ( Model, Cmd Msg )
legacyReleasedDrag model =
    case model.viewRangeDrag of
        Just d ->
            if d.moved then
                ( { model | viewRangeDrag = Nothing }, Cmd.none )

            else
                let
                    timeline =
                        Data.Project.timeline model.project

                    fractionalBar =
                        d.pressOffsetX / toFloat model.sectionBarZoom
                in
                seekTo (Data.Timeline.fractionalBarToTicks fractionalBar timeline) { model | viewRangeDrag = Nothing }

        Nothing ->
            case model.paneDividerDrag of
                Just _ ->
                    ( { model | paneDividerDrag = Nothing }, Cmd.none )

                Nothing ->
                    case model.velocityDrag of
                    Just _ ->
                        ( { model | velocityDrag = Nothing }, Cmd.none )

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
                                            ( releaseSectionMoveDrag model, Cmd.none )

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
                                                case model.chordDrag of
                                                    Just _ ->
                                                        ( { model | chordDrag = Nothing }, Cmd.none )

                                                    Nothing ->
                                                        case model.chordRubberBand of
                                                            Just crb ->
                                                                let
                                                                    x0 =
                                                                        Basics.min crb.originX crb.curX

                                                                    x1 =
                                                                        Basics.max crb.originX crb.curX

                                                                    t0 =
                                                                        PianoRoll.pixelsToTicks model.pianoRollZoom x0

                                                                    t1 =
                                                                        PianoRoll.pixelsToTicks model.pianoRollZoom x1

                                                                    timeline =
                                                                        Data.Project.timeline model.project

                                                                    sel =
                                                                        Data.ChordTrack.tokenKeysInTickRange timeline t0 t1 model.project.chordTrack
                                                                in
                                                                ( { model | chordRubberBand = Nothing, selectedChordKeys = sel }, Cmd.none )

                                                            Nothing ->
                                                                releasedDragNoteOrRubberBand model


{-| ノートの矩形選択・ドラッグ移動の mousemove 処理。DraggedTo カスケードの末端（他のドラッグ状態がすべて Nothing）で呼ばれる。
-}
draggedToNoteOrRubberBand : ClientPos -> Model -> ( Model, Cmd Msg )
draggedToNoteOrRubberBand pos model =
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


{-| ノートの矩形選択・ドラッグ移動の mouseup 処理。ReleasedDrag カスケードの末端で呼ばれる。
ドラムトラックが選択中の場合はY軸判定に `DrumEditor.pitchesInYRange` を使い、X軸判定はグリッドに
スナップした `cellStart` を基準にする（`activeCell` の描画基準と一致させるため）。
-}
releasedDragNoteOrRubberBand : Model -> ( Model, Cmd Msg )
releasedDragNoteOrRubberBand model =
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

                sel =
                    case selectedTrackKind model of
                        Just (DrumTrack _) ->
                            let
                                grid =
                                    Data.Time.gridTicks model.gridUnit

                                pitches =
                                    DrumEditor.pitchesInYRange y0 y1
                            in
                            trackNotes model
                                |> List.filter
                                    (\n ->
                                        let
                                            cellStart =
                                                n.start // grid * grid
                                        in
                                        (cellStart < t1)
                                            && (cellStart + grid > t0)
                                            && Set.member n.pitch pitches
                                    )
                                |> List.map .id
                                |> Set.fromList

                        _ ->
                            let
                                pLow =
                                    PianoRoll.yToPitch (isTouchLayout model) y1

                                pHigh =
                                    PianoRoll.yToPitch (isTouchLayout model) y0
                            in
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

{-| DraggedTo の全分岐に共通で、現在のポインタ座標を dragCursor へ反映する。ドラッグゴースト（viewDragGhost）の座標源になる。
-}
withDragCursor : Float -> Float -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withDragCursor x y ( m, cmd ) =
    ( { m | dragCursor = Just { x = x, y = y } }, cmd )


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
            newModel.dragState /= NoDrag || newModel.chordDrag /= Nothing || newModel.chordRubberBand /= Nothing || newModel.velocityDrag /= Nothing

        saveCmds =
            if projectChanged || newModel.selectedTrackId /= model.selectedTrackId then
                [ Ports.saveToLocalStorage (ProjectJson.encodeWith { selectedTrackId = newModel.selectedTrackId } newModel.project) ]

            else
                []

        syncCmds =
            if newModel.playState == Playing && not stillDragging && (projectChanged || isReleasedDrag msg) then
                [ Ports.toAudio (Performance.encodeUpdateEvents { metronome = newModel.metronomeEnabled, metronomeVolume = newModel.metronomeVolume } newModel.project) ]

            else
                []

        -- シートの元データが実際に変わったかの狭い判定。
        -- project 全体（ノート・音量等）の変更では draft を触らない。
        sheetSourceChanged =
            (coreModel.project.sections /= model.project.sections)
                || (coreModel.project.chordTrack.text /= model.project.chordTrack.text)

        isSheetTextEdit =
            case msg of
                ChangedChordSheetText _ ->
                    True

                _ ->
                    False

        finalModel =
            (if isReleasedDrag msg then
                { newModel | highlightedPitches = Set.empty }

             else
                newModel
            )
                |> (if sheetSourceChanged && not isSheetTextEdit then
                        refreshChordSheetDraft

                    else
                        identity
                   )
    in
    ( finalModel, Cmd.batch (cmd :: saveCmds ++ syncCmds) )


startPlay : Maybe Performance.Loop -> Int -> Model -> ( Model, Cmd Msg )
startPlay loop startTicks model =
    let
        instrumentNames =
            if model.metronomeEnabled then
                "drumKit" :: Performance.usedInstrumentNames model.project

            else
                Performance.usedInstrumentNames model.project
    in
    ( { model
        | playState = Playing
        , instrumentLoad = markLoading instrumentNames model.instrumentLoad
      }
    , Ports.toAudio (Performance.encodePlay { loop = loop, startTicks = startTicks, metronome = model.metronomeEnabled, metronomeVolume = model.metronomeVolume } model.project)
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


{-| スケールガイドの実効キー。手動 override があればそれ、無ければ scaleReferenceTicks 位置の
セクションキー（左鍵盤のスケールマーカーと同じ基準）。
-}
effectiveGuideKey : Model -> Data.Key.Key
effectiveGuideKey model =
    model.guideKeyOverride
        |> Maybe.withDefault (Data.Timeline.keyAt (scaleReferenceTicks model) (Data.Project.timeline model.project))


{-| セクションが始まる 0-based 小節番号。小節挿入・削除の基準点を決めるのに使う。
-}
sectionStartBar : Int -> Project -> Maybe Int
sectionStartBar sectionId project =
    Data.Project.sectionBounds sectionId project
        |> Maybe.map (\b -> (Data.Timeline.ticksToBarBeat b.startTicks (Data.Project.timeline project)).bar - 1)


{-| セクションの小節数を newLenRaw（1..64 にクランプ）に変える。小節数入力（`ChangedSectionBars`）と
リサイズハンドルのドラッグリリース両方から共通で呼ばれる。lengthBars を変えるだけで、ノート・コードは
絶対位置に留まる（追従しない）。
-}
resizeSectionBars : Int -> Int -> Model -> Model
resizeSectionBars sectionId newLenRaw model =
    { model | project = Data.Project.updateSection sectionId (\s -> { s | lengthBars = clamp 1 64 newLenRaw }) model.project }


{-| コード進行パネルが開いている（chordSheetDraft が Just の）間、シートの元データ（セクション列または
コードトラック本文）が変わったら textarea の下書きを toSheetText で作り直す。ChangedChordSheetText 自体は
呼び出し側（updateラッパー）で除外する（ユーザーが入力中の生テキスト—コメントや途中の崩れた形式—を
正規化で上書きしないため）。
-}
refreshChordSheetDraft : Model -> Model
refreshChordSheetDraft m =
    case m.chordSheetDraft of
        Just _ ->
            { m | chordSheetDraft = Just (Data.ChordSheet.toSheetText m.project.sections m.project.chordTrack |> Data.ChordTrack.wrapBarLines 4) }

        Nothing ->
            m


parseInsertCount : String -> Int
parseInsertCount raw =
    String.toInt raw |> Maybe.withDefault 1 |> clamp 1 64


{-| プレイヘッドのある 0-based 小節番号。小節挿入・削除の基準点に使う。
ticksToBarBeat は末尾を超えた tick も最後の小節にクランプするので、常に実在する小節番号を返す。
-}
playheadBar : Model -> Int
playheadBar model =
    (Data.Timeline.ticksToBarBeat model.playheadTicks (Data.Project.timeline model.project)).bar - 1


{-| セクションブロックのドラッグ終了時の共通ロジック。実際に並べ替えが起きていなければ
（＝クリックのみと同等）、押した瞬間に既に選択中だったセクションはトグルオフする。
-}
releaseSectionMoveDrag : Model -> Model
releaseSectionMoveDrag model =
    case model.sectionMoveDrag of
        Just d ->
            { model
                | sectionMoveDrag = Nothing
                , selectedSectionId =
                    if not d.moved && d.wasSelected then
                        Nothing

                    else
                        model.selectedSectionId
            }

        Nothing ->
            model


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


{-| ループ端ハンドルのドラッグ開始状態。isEnd なら終端側を動かし始端を固定、そうでなければ逆。
ピアノロールのルーラーとセクションバーの両方の PressedLoopHandle 系ハンドラーが共有する。
-}
loopHandleDrag : Bool -> Float -> Performance.Loop -> LoopDrag
loopHandleDrag isEnd clientX loop =
    let
        ( fixedTicks, baseTicks ) =
            if isEnd then
                ( loop.startTicks, loop.endTicks )

            else
                ( loop.endTicks, loop.startTicks )
    in
    { fixedTicks = fixedTicks, baseTicks = baseTicks, startClientX = clientX, curTicks = baseTicks }


{-| ループ範囲の表示値。ドラッグ中はその暫定範囲（fixed/cur を正順に並べたもの）を、そうでなければ
確定済みの `currentLoop` を返す。ピアノロール・セクションバーの view で共通使用。
-}
displayedLoop : Maybe LoopDrag -> Model -> Maybe Performance.Loop
displayedLoop drag model =
    case drag of
        Just ld ->
            Just { startTicks = Basics.min ld.fixedTicks ld.curTicks, endTicks = Basics.max ld.fixedTicks ld.curTicks }

        Nothing ->
            currentLoop model


{-| アンカー保存ズーム後のスクロール位置補正コマンド。
oldPxToTicks は旧ズームでの px→ticks、newTicksToPx は新ズームでの ticks→px。
ピアノロール・セクションバーの GotXxxViewportForZoom ハンドラーが共通使用。
-}
zoomScrollCmd :
    { scrollId : String
    , offsetX : Float
    , viewportX : Float
    , oldPxToTicks : Float -> Int
    , newTicksToPx : Int -> Float
    }
    -> Cmd Msg
zoomScrollCmd cfg =
    let
        anchorTicks =
            cfg.oldPxToTicks (cfg.viewportX + cfg.offsetX)

        newScrollLeft =
            Basics.max 0 (cfg.newTicksToPx anchorTicks - cfg.offsetX)
    in
    Task.attempt (\_ -> NoOp) (Browser.Dom.setViewportOf cfg.scrollId newScrollLeft 0)


{-| ピアノロールのスクロールコンテナ（`PianoRoll.pianoRollScrollId`）が現在の選択状態で実際にマウントされているか。
コード進行トラックをブロック表示中の時だけ非マウント（リージョン上の可視範囲枠もこの時は非表示になる）。
-}
pianoRollScrollMounted : Model -> Bool
pianoRollScrollMounted model =
    not (model.selectedTrackId == Data.ChordTrack.trackId && model.chordBlockView)


{-| トラック切替等で `PianoRoll.pianoRollScrollId` の DOM が作り直された場合に備え、`model.pianoRollScrollX`
の位置を明示的に復元し、その後再計測する。同種トラック同士の切替でDOMが再利用されるケースでも
同値を再設定するだけで無害。`Process.sleep 0` で一拍待つのは、update 直後のCmd実行がDOM再描画より先行して旧ノード相手に
`setViewportOf` してしまうのを避けるため。マウントされていないときはCmd.none。
-}
restorePianoRollScrollCmd : Model -> Cmd Msg
restorePianoRollScrollCmd model =
    if pianoRollScrollMounted model then
        Process.sleep 0
            |> Task.mapError never
            |> Task.andThen (\_ -> Browser.Dom.setViewportOf PianoRoll.pianoRollScrollId model.pianoRollScrollX 0)
            |> Task.andThen (\_ -> Browser.Dom.getViewportOf PianoRoll.pianoRollScrollId)
            |> Task.attempt GotPianoRollViewportMeasured

    else
        Cmd.none


{-| 削除ボタンの2度押し確認。pending が今回の id と一致していれば confirm（実削除）を、
そうでなければ arm（アーム状態にするだけ）を返す。
-}
confirmTwice : { pending : Maybe id, id : id, arm : ( Model, Cmd Msg ), confirm : ( Model, Cmd Msg ) } -> ( Model, Cmd Msg )
confirmTwice cfg =
    if cfg.pending == Just cfg.id then
        cfg.confirm

    else
        cfg.arm


{-| プロジェクトを丸ごと差し替える（新規作成・JSON読込共通）。選択・ループ範囲・ドラッグ中の pending 系を全てリセットし、再生を止める。
-}
resetToProject : Data.Project.Project -> Maybe Int -> Model -> ( Model, Cmd Msg )
resetToProject project maybeSelectedTrackId model =
    let
        selectedTrackId =
            maybeSelectedTrackId
                |> Maybe.andThen (\tid -> if validTrackId project tid then Just tid else Nothing)
                |> Maybe.withDefault (firstTrackId project)

        newModel =
            { model
                | project = project
                , selectedTrackId = selectedTrackId
                , playState = Idle
                , playheadTicks = 0
                , bpmInput = String.fromFloat project.bpm
                , selectedNoteIds = Set.empty
                , selectedSectionId = Nothing
                , loopRange = Nothing
                , refOffsetInput = String.fromInt project.referenceAudio.offsetMs
                , refLoaded = False
                , refPeaks = Array.empty
                , pendingNewProject = False
                , hoveredNote = Nothing
                , hoveredFretCell = Nothing
                , selectedChordKeys = Set.empty
                , chordDrag = Nothing
                , pendingChordDrag = Nothing
                , chordRubberBand = Nothing
                , pianoRollScrollX = 0
            }
    in
    ( newModel
    , Cmd.batch [ Ports.toAudio Performance.encodeStop, restorePianoRollScrollCmd newModel ]
    )


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
                Basics.max 0 (snapFloor model model.playheadTicks)

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
        , hoveredNote =
            case model.hoveredNote of
                Just h ->
                    if Set.member h.note.id model.selectedNoteIds then
                        Nothing

                    else
                        model.hoveredNote

                Nothing ->
                    Nothing
    }


{-| 削除した noteId が現在の hoveredNote と一致すればツールチップを消す。一致しなければ他のノートへのホバーは保つ。
-}
clearHoveredNote : Int -> Maybe { note : Data.Note.Note, x : Float, y : Float } -> Maybe { note : Data.Note.Note, x : Float, y : Float }
clearHoveredNote noteId hovered =
    case hovered of
        Just h ->
            if h.note.id == noteId then
                Nothing

            else
                hovered

        Nothing ->
            Nothing


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


{-| tick 位置から TokenKey を逆引きした後にも、トークンをダブルクリックしたときも、最終的にはこの関数に集約する。
対象トークンが `TChord` としてパースできている場合だけ FormPicker を開き、draft の初期値は現在のトークン文字列
（`TokenSpan.token`）をそのまま使う。
-}
openFormPickerFor : Model -> Data.ChordTrack.TokenKey -> ( Model, Cmd Msg )
openFormPickerFor model key =
    let
        timeline =
            Data.Project.timeline model.project

        span =
            Data.ChordTrack.tokenSpans timeline model.project.chordTrack
                |> List.filter (\s -> s.key == key)
                |> List.head
    in
    case span of
        Just s ->
            case s.result of
                Ok (Data.ChordTrack.TChord _) ->
                    ( { model | formPicker = Just { key = key, draft = s.token, tab = FormPicker.CandidatesTab } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


{-| FormPicker の draft をトラックへ確定する。`ChordParser.parse` に成功したときだけ `updateToken` で
対象トークンのテキストを draft の生テキストに書き換える。失敗している間（パース不能）は何もしない（モーダルを
閉じたときの黙殺破棄と、ChoseChordForm/ClearedChordVoicing が未確定の draft を安全に無視できることの
両方をこれで兼ねる）。
-}
commitFormPickerDraft : Model -> Model
commitFormPickerDraft model =
    case model.formPicker of
        Nothing ->
            model

        Just fp ->
            let
                timeline =
                    Data.Project.timeline model.project
            in
            case ChordParser.parse fp.draft of
                Ok _ ->
                    { model
                        | project =
                            Data.Project.updateChordTrack
                                (Data.ChordTrack.updateToken timeline fp.key (\_ -> fp.draft))
                                model.project
                    }

                Err _ ->
                    model


{-| formPicker.key の現在のトークン文字列を tokenSpans から引き直して draft に反映する。`@NAME` が付いたり
消えたりした後に draft を追従させるために使う（古い draft のまま適用すると @NAME を剥がす事故を防ぐ）。
formPicker が Nothing なら何もしない。対象トークンが見つからなければ（理論上起こらないが）draft は変えない。
-}
refreshFormPickerDraft : Model -> Model
refreshFormPickerDraft model =
    case model.formPicker of
        Nothing ->
            model

        Just fp ->
            let
                timeline =
                    Data.Project.timeline model.project

                newDraft =
                    Data.ChordTrack.tokenSpans timeline model.project.chordTrack
                        |> List.filter (\s -> s.key == fp.key)
                        |> List.head
                        |> Maybe.map .token
            in
            case newDraft of
                Just token ->
                    { model | formPicker = Just { fp | draft = token } }

                Nothing ->
                    model


{-| ボイシングキーボードを、対象行（displayRoot）が中央付近に来る位置へスクロールさせる Cmd。元々
`ClickedVoicingRow` がタブを開く際に計算していたものを抽出。FormPicker の「手で編集」タブへの自動遷移でも同じスクロールを起こす。
-}
voicingKeyboardScrollCmd : Model -> Int -> Cmd Msg
voicingKeyboardScrollCmd model index =
    let
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
    in
    Task.attempt (\_ -> NoOp) (Browser.Dom.setViewportOf VoicingKeyboard.scrollId 0 target)


{-| FormPicker の「手で編集」タブへ切り替える。編集対象 index は `editingVoicingIndex` のみが持つ（formPicker
側には複製しない）。選択/ドラッグ/削除確認の状態を全てリセットし、既存のボイシング編集ロジック（矢印キー、
Delete、ドラッグ）をそのまま流用できる状態にする。
-}
openEditTab : Int -> Model -> ( Model, Cmd Msg )
openEditTab index model =
    ( { model
        | formPicker = Maybe.map (\fp -> { fp | tab = FormPicker.EditTab }) model.formPicker
        , editingVoicingIndex = Just index
        , voicingSelectedOffsets = Set.empty
        , voicingDragState = NoVoicingDrag
        , pendingVoicingDrag = Nothing
        , pendingVoicingDelete = Nothing
      }
    , voicingKeyboardScrollCmd model index
    )


{-| key のトークンに voicing を登録する（辞書へ insert + トークンの @NAME を更新）。
`registerCandidateForToken` と、手編集タブの自動登録（空 Voicing 作成）の両方がこれを共有する。
-}
insertVoicingForToken : Data.ChordTrack.TokenKey -> Data.Voicing.Voicing -> Model -> Model
insertVoicingForToken key voicing model =
    let
        timeline =
            Data.Project.timeline model.project
    in
    { model
        | project =
            model.project
                |> Data.Project.insertVoicing voicing
                |> Data.Project.updateChordTrack (Data.ChordTrack.updateToken timeline key (Data.ChordTrack.withVoicingName voicing.name))
    }


{-| 登録後の共通後処理：draft を新しいトークン文字列に追従させ、登録した名前で index を逆引きして
手編集タブを開く。ChoseChordForm と自動登録フローの両方がこの尾処理を共有する。
-}
finishRegistrationAndOpenEditTab : String -> Model -> ( Model, Cmd Msg )
finishRegistrationAndOpenEditTab name registeredModel =
    let
        refreshedModel =
            refreshFormPickerDraft registeredModel
    in
    case Data.Project.voicingIndexByName name refreshedModel.project.voicings of
        Just index ->
            openEditTab index refreshedModel

        Nothing ->
            ( refreshedModel, Cmd.none )


{-| key のトークンから、パース済みのコードを逆引きする。トークンが `TChord` としてパースできない、
または該当スパンが無い場合は `Nothing`。
-}
chordForToken : Data.ChordTrack.TokenKey -> Model -> Maybe Data.Chord.Chord
chordForToken key model =
    let
        timeline =
            Data.Project.timeline model.project
    in
    Data.ChordTrack.tokenSpans timeline model.project.chordTrack
        |> List.filter (\s -> s.key == key)
        |> List.head
        |> Maybe.andThen
            (\s ->
                case s.result of
                    Ok (Data.ChordTrack.TChord chord) ->
                        Just chord

                    _ ->
                        Nothing
            )


{-| ボイシング名の共通プレフィックス（root + quality + extensions + bass）。GuitarForm 候補・
VoicingPreset シェイプいずれのサフィックスも、このベース名の後ろに `_` 区切りで付ける。
-}
baseVoicingName : Data.Chord.Chord -> String
baseVoicingName chord =
    ChordFormat.pitchName False chord.root
        ++ ChordFormat.qualitySuffix chord.quality
        ++ Data.GuitarForm.extensionsSuffix chord.extensions
        ++ Data.GuitarForm.bassSuffix chord.bass


{-| 候補カードを選んだときの登録ロジック。元々 `ChoseChordForm` ハンドラ本体にあったものを抽出。
token が `TChord` としてパースできない場合は `Nothing`。
-}
registerCandidateForToken : Data.ChordTrack.TokenKey -> Data.GuitarForm.Candidate -> Model -> Maybe ( Model, String )
registerCandidateForToken key candidate model =
    chordForToken key model
        |> Maybe.map
            (\chord ->
                let
                    rootPitch =
                        Data.Voicing.anchorPitch + modBy 12 chord.root

                    name =
                        baseVoicingName chord ++ "_" ++ Data.GuitarForm.candidateSuffix candidate

                    voicing =
                        Data.StrumExpand.voicingFromForm rootPitch candidate.form name
                in
                ( insertVoicingForToken key voicing model, name )
            )


{-| VoicingPreset のシェイプ（Closed/Drop2/Wide）を選んだときの登録ロジック。`registerCandidateForToken` と
同型（`ChoseVoicingShape` ハンドラから呼ぶ）。token が `TChord` としてパースできない場合は `Nothing`。
-}
registerShapeForToken : Data.ChordTrack.TokenKey -> Data.VoicingPreset.Shape -> Model -> Maybe ( Model, String )
registerShapeForToken key shape model =
    chordForToken key model
        |> Maybe.map
            (\chord ->
                let
                    name =
                        baseVoicingName chord ++ "_" ++ Data.VoicingPreset.shapeSuffix shape

                    voicing =
                        { name = name
                        , offsets = Data.VoicingPreset.offsetsFor chord.quality shape
                        , stringPicks = Set.empty
                        }
                in
                ( insertVoicingForToken key voicing model, name )
            )


{-| 「手で編集」タブを未登録コードで直接開いたときの自動登録。運指の弾きやすさに関わらず常に
VoicingPreset.Closed（シンプルな音の積み重ね）で登録する（手引きやすさを優先し、高ポジションの
GuitarForm 先頭候補を自動登録しない方針）。GuitarForm 候補は「候補から選ぶ」タブ側でのみ提示する。
-}
autoRegisterAndOpenEditTab : Data.ChordTrack.TokenKey -> Data.Chord.Chord -> Model -> ( Model, Cmd Msg )
autoRegisterAndOpenEditTab key chord model =
    let
        name =
            baseVoicingName chord ++ "_" ++ Data.VoicingPreset.shapeSuffix Data.VoicingPreset.Closed

        voicing =
            { name = name
            , offsets = Data.VoicingPreset.offsetsFor chord.quality Data.VoicingPreset.Closed
            , stringPicks = Set.empty
            }
    in
    finishRegistrationAndOpenEditTab name (insertVoicingForToken key voicing model)


showToast : Toast.Tone -> String -> Model -> ( Model, Cmd Msg )
showToast tone message model =
    let
        newId =
            model.toastCounter + 1
    in
    ( { model | toast = Just { id = newId, tone = tone, message = message }, toastCounter = newId }
    , Task.perform (\_ -> DismissedToast newId) (Process.sleep 4000)
    )


isModalOpen : Model -> Bool
isModalOpen model =
    model.formPicker /= Nothing || model.editingVoicingIndex /= Nothing || model.wavExportModalOpen || model.helpModalOpen


{-| 開いているモーダルを優先順（formPicker > editingVoicingIndex > wavExportModalOpen）で一つ閉じる。
既存のクローズ用Msgハンドラを再利用することで、閉じる際の後処理（voicing選択解除等）を二重実装しない。
WAV書出のレンダリング中は閉じない（状態が宙ぶらりになるのを防ぐ）。
-}
closeTopModal : Model -> ( Model, Cmd Msg )
closeTopModal model =
    if model.formPicker /= Nothing then
        updateCore ClosedFormPicker model

    else if model.editingVoicingIndex /= Nothing then
        if Set.isEmpty model.voicingSelectedOffsets then
            updateCore ClosedFormPicker model

        else
            ( { model | voicingSelectedOffsets = Set.empty }, Cmd.none )

    else if model.wavExportModalOpen && model.wavExportState /= WavExportRendering then
        updateCore ClosedWavExportModal model

    else if model.helpModalOpen then
        updateCore ToggledHelpModal model

    else
        ( model, Cmd.none )


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

        ClickedThemeToggle ->
            let
                nextPreference =
                    case model.themePreference of
                        Theme.SystemTheme ->
                            Theme.LightTheme

                        Theme.LightTheme ->
                            Theme.DarkTheme

                        Theme.DarkTheme ->
                            Theme.SystemTheme
            in
            ( { model | themePreference = nextPreference }
            , Ports.setTheme (Theme.themeToString nextPreference)
            )

        ChangedGuideKeyTonic raw ->
            case raw of
                "follow" ->
                    ( { model | guideKeyOverride = Nothing }, Cmd.none )

                _ ->
                    case String.toInt raw of
                        Just tonic ->
                            ( { model | guideKeyOverride = Just { tonic = tonic, mode = (effectiveGuideKey model).mode } }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

        ChangedGuideKeyMode raw ->
            case Data.Key.modeFromString raw of
                Just mode ->
                    ( { model | guideKeyOverride = Just { tonic = (effectiveGuideKey model).tonic, mode = mode } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

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

                WavRenderStarted ->
                    ( model, Cmd.none )

                WavRenderDone ->
                    showToast Toast.Success "WAVを書き出しました" { model | wavExportModalOpen = False, wavExportState = WavExportIdle }

                WavRenderFailed message ->
                    ( { model | wavExportState = WavExportFailed message }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PressedEmptyCell pos ->
            let
                {- 残留したpendingNoteDragがあっても、次のクリックで必ず健全化するように先頭でリセットする。 -}
                model1 =
                    { model | pendingNoteDrag = Nothing, pendingEmptyTouch = Nothing }
            in
            if pos.seekMod || model1.touchMode == TouchSeek then
                seekTo (snapFloor model1 (PianoRoll.pixelsToTicks model1.pianoRollZoom pos.offsetX)) model1

            else if pos.shift || model1.touchMode == TouchSelect then
                ( startRubberBand pos model1, Cmd.none )

            else
                let
                    exactTick =
                        PianoRoll.pixelsToTicks model1.pianoRollZoom pos.offsetX

                    hitPitch =
                        PianoRoll.yToPitch (isTouchLayout model1) pos.offsetY

                    hit =
                        trackNotes model1
                            |> List.filter (\n -> n.pitch == hitPitch && n.start <= exactTick && exactTick < n.start + n.duration)
                            |> List.head
                in
                case hit of
                    Just note ->
                        pressNoteForDrag note PianoRoll.NoResize { clientX = pos.clientX, clientY = pos.clientY, isTouch = pos.isTouch } model1

                    Nothing ->
                        if pos.isTouch then
                            -- 配置はpointerup（タップ確定）まで保留。スワイプならブラウザが
                            -- pointercancelを発火してネイティブスクロールに移行し、配置されない。
                            ( { model1 | pendingEmptyTouch = Just { offsetX = pos.offsetX, offsetY = pos.offsetY, clientX = pos.clientX, clientY = pos.clientY } }
                            , Cmd.none
                            )

                        else
                            placeNoteWithResizePending pos model1

        PressedNote noteId handle pos ->
            let
                {- 残留したpendingNoteDragがあっても、次のクリックで必ず健全化するように先頭でリセットする。
                   pointer capture導入後は原理上残留しないはずだが、多層防御として入れておく。
                -}
                model1 =
                    { model | pendingNoteDrag = Nothing, pendingEmptyTouch = Nothing }
            in
            case findNote model1 noteId of
                Just note ->
                    let
                        {- iOS Safariはタッチではdblclickを発火しないため、300ms以内・24px以内の同一ノートへの
                           再タップを独自にダブルタップとして検出する。長押し削除（500ms）と両立させる。
                        -}
                        isDoubleTap =
                            pos.isTouch
                                && (case model1.lastNoteTap of
                                        Just lt ->
                                            lt.noteId
                                                == noteId
                                                && (pos.timeStamp - lt.time)
                                                < 300
                                                && abs (pos.clientX - lt.clientX)
                                                < 24
                                                && abs (pos.clientY - lt.clientY)
                                                < 24

                                        Nothing ->
                                            False
                                   )
                    in
                    if pos.shift || model1.touchMode == TouchSelect then
                        ( { model1
                            | selectedNoteIds =
                                if Set.member noteId model1.selectedNoteIds then
                                    Set.remove noteId model1.selectedNoteIds

                                else
                                    Set.insert noteId model1.selectedNoteIds
                            , highlightedPitches = Set.singleton note.pitch
                          }
                        , Cmd.none
                        )

                    else if isDoubleTap then
                        ( { model1
                            | project = Data.Project.removeNote model1.selectedTrackId noteId model1.project
                            , selectedNoteIds = Set.remove noteId model1.selectedNoteIds
                            , hoveredNote = clearHoveredNote noteId model1.hoveredNote
                            , pendingNoteDrag = Nothing
                            , longPress = Nothing
                            , lastNoteTap = Nothing
                          }
                        , Cmd.none
                        )

                    else
                        let
                            ( model2, cmd ) =
                                pressNoteForDrag note handle pos model1
                        in
                        if pos.isTouch then
                            let
                                ( model3, armCmd ) =
                                    armLongPress (LongPressNote note.id) model2

                                newTooltipToken =
                                    model3.touchTooltipToken + 1

                                model4 =
                                    { model3
                                        | hoveredNote = Just { note = note, x = pos.clientX, y = pos.clientY }
                                        , touchTooltipToken = newTooltipToken
                                        , lastNoteTap = Just { noteId = noteId, time = pos.timeStamp, clientX = pos.clientX, clientY = pos.clientY }
                                    }

                                tooltipCmd =
                                    Task.perform (\_ -> ExpiredTouchTooltip newTooltipToken) (Process.sleep 1500)
                            in
                            ( model4, Cmd.batch [ cmd, armCmd, tooltipCmd ] )

                        else
                            ( model2, cmd )

                Nothing ->
                    ( model1, Cmd.none )

        SelectedTool t ->
            ( { model | tool = t, cutGuideTicks = Nothing }, Cmd.none )

        SelectedTouchMode m ->
            ( { model | touchMode = m }, Cmd.none )

        ToggledTouchSnapOff ->
            ( { model | touchSnapOff = not model.touchSnapOff }, Cmd.none )

        LongPressFired token ->
            case model.longPress of
                Just lp ->
                    if lp.token == token then
                        let
                            model1 =
                                { model | longPress = Nothing }
                        in
                        case lp.target of
                            LongPressNote noteId ->
                                ( { model1
                                    | project = Data.Project.removeNote model1.selectedTrackId noteId model1.project
                                    , selectedNoteIds = Set.remove noteId model1.selectedNoteIds
                                    , hoveredNote = clearHoveredNote noteId model1.hoveredNote
                                    , pendingNoteDrag = Nothing
                                  }
                                , Cmd.none
                                )

                            LongPressVoicingOffset index pitch ->
                                let
                                    rootPitch =
                                        Data.Voicing.anchorPitch + model1.voicingPreviewRoot

                                    offset =
                                        pitch - rootPitch
                                in
                                ( { model1
                                    | project =
                                        Data.Project.updateVoicing index
                                            (\v ->
                                                { v
                                                    | offsets = List.filter ((/=) offset) v.offsets
                                                    , stringPicks = Data.GuitarForm.removePicks (Set.singleton offset) v.stringPicks
                                                }
                                            )
                                            model1.project
                                    , voicingSelectedOffsets = Set.remove offset model1.voicingSelectedOffsets
                                    , pendingVoicingDrag = Nothing
                                  }
                                , Cmd.none
                                )

                            LongPressDrumNote target ->
                                removeDrumNoteAt target model1

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ExpiredTouchTooltip token ->
            if model.touchTooltipToken == token then
                ( { model | hoveredNote = Nothing, hoveredFretCell = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        MovedCutGuide pos ->
            let
                newGuide =
                    Just (Basics.max 0 (snapRound model (PianoRoll.pixelsToTicks model.pianoRollZoom pos.offsetX)))
            in
            if newGuide == model.cutGuideTicks then
                ( model, Cmd.none )

            else
                ( { model | cutGuideTicks = newGuide }, Cmd.none )

        ClearedCutGuide ->
            ( { model | cutGuideTicks = Nothing }, Cmd.none )

        PressedCutAt pos ->
            if pos.seekMod || model.touchMode == TouchSeek then
                seekTo (snapFloor model (PianoRoll.pixelsToTicks model.pianoRollZoom pos.offsetX)) model

            else if pos.shift || model.touchMode == TouchSelect then
                ( startRubberBand pos model, Cmd.none )

            else
                let
                    tick =
                        Basics.max 0 (snapRound model (PianoRoll.pixelsToTicks model.pianoRollZoom pos.offsetX))

                    targetIds =
                        if Set.isEmpty model.selectedNoteIds then
                            trackNotes model |> List.map .id |> Set.fromList

                        else
                            model.selectedNoteIds

                    result =
                        Data.Project.cutNotesAt { trackId = model.selectedTrackId, tick = tick, targetIds = targetIds } model.project
                in
                ( { model | project = result.project, selectedNoteIds = result.newSelection }, Cmd.none )

        PressedVelocityBar noteId pos ->
            let
                targetIds =
                    if Set.member noteId model.selectedNoteIds then
                        model.selectedNoteIds

                    else
                        Set.singleton noteId

                origs =
                    trackNotes model
                        |> List.filter (\n -> Set.member n.id targetIds)
                        |> List.map (\n -> ( n.id, n.velocity ))
                        |> Dict.fromList
            in
            ( { model
                | selectedNoteIds = targetIds
                , velocityDrag = Just { startClientY = pos.clientY, origVelocities = origs }
              }
            , Cmd.none
            )

        PressedChordToken key pos ->
            let
                {- 残留したpendingChordDragがあっても、次のクリックで必ず健全化するように先頭でリセットする。 -}
                model1 =
                    { model | pendingChordDrag = Nothing }
            in
            if pos.shift || model1.touchMode == TouchSelect then
                ( { model1
                    | selectedChordKeys =
                        if Set.member key model1.selectedChordKeys then
                            Set.remove key model1.selectedChordKeys

                        else
                            Set.insert key model1.selectedChordKeys
                  }
                , Cmd.none
                )

            else
                let
                    sel =
                        if Set.member key model1.selectedChordKeys then
                            model1.selectedChordKeys

                        else
                            Set.singleton key

                    timeline =
                        Data.Project.timeline model1.project

                    anchorSpan =
                        Data.ChordTrack.tokenSpans timeline model1.project.chordTrack
                            |> List.filter (\s -> s.key == key)
                            |> List.head

                    anchorCenterTicks =
                        anchorSpan
                            |> Maybe.map (\s -> s.startTicks + s.durationTicks // 2)
                            |> Maybe.withDefault 0

                    anchorToken =
                        anchorSpan
                            |> Maybe.map .token
                            |> Maybe.withDefault ""

                    ghostLabel =
                        if Set.size sel > 1 then
                            anchorToken ++ " ほか" ++ String.fromInt (Set.size sel - 1) ++ "個"

                        else
                            anchorToken
                in
                ( { model1
                    | selectedChordKeys = sel
                    , pendingChordDrag =
                        Just
                            { anchorKey = key
                            , anchorCenterTicks = anchorCenterTicks
                            , startClientX = pos.clientX
                            , startClientY = pos.clientY
                            , origText = model1.project.chordTrack.text
                            , origKeys = sel
                            , lastDeltaBars = 0
                            , ghostLabel = ghostLabel
                            }
                  }
                , Cmd.none
                )

        PressedChordLane pos ->
            let
                model1 =
                    { model | pendingChordDrag = Nothing }
            in
            if pos.seekMod || model1.touchMode == TouchSeek then
                seekTo (snapFloor model1 (PianoRoll.pixelsToTicks model1.pianoRollZoom pos.offsetX)) model1

            else
                ( { model1
                    | chordRubberBand =
                        Just
                            { originX = pos.offsetX
                            , originY = 0
                            , startClientX = pos.clientX
                            , startClientY = pos.clientY
                            , curX = pos.offsetX
                            , curY = 0
                            }
                    , selectedChordKeys = Set.empty
                  }
                , Cmd.none
                )

        DoubleClickedNote noteId ->
            ( { model
                | project = Data.Project.removeNote model.selectedTrackId noteId model.project
                , selectedNoteIds = Set.remove noteId model.selectedNoteIds
                , dragState = NoDrag
                , hoveredNote = clearHoveredNote noteId model.hoveredNote
              }
            , Cmd.none
            )

        RightClickedNote noteId ->
            ( { model
                | project = Data.Project.removeNote model.selectedTrackId noteId model.project
                , selectedNoteIds = Set.remove noteId model.selectedNoteIds
                , hoveredNote = clearHoveredNote noteId model.hoveredNote
              }
            , Cmd.none
            )

        DraggedTo pos ->
            (case model.pendingNoteDrag of
                Just info ->
                    let
                        dx =
                            pos.clientX - info.startClientX

                        dy =
                            pos.clientY - info.startClientY

                        dist =
                            sqrt (dx * dx + dy * dy)

                        threshold =
                            if info.isTouch then
                                touchDragThreshold

                            else
                                dragThreshold
                    in
                    if dist < threshold then
                        ( model, Cmd.none )

                    else
                        ( { model | pendingNoteDrag = Nothing, dragState = Dragging info, longPress = Nothing }, Cmd.none )

                Nothing ->
                    case model.pendingChordDrag of
                        Just cd ->
                            if exceedsDragThreshold cd pos then
                                ( { model | pendingChordDrag = Nothing, chordDrag = Just cd }, Cmd.none )

                            else
                                ( model, Cmd.none )

                        Nothing ->
                            case model.pendingVoicingDrag of
                                Just vd ->
                                    if exceedsDragThreshold vd pos then
                                        ( { model | pendingVoicingDrag = Nothing, voicingDragState = DraggingVoicingOffsets vd, longPress = Nothing }, Cmd.none )

                                    else
                                        ( model, Cmd.none )

                                Nothing ->
                                    legacyDraggedTo pos model
            )
                |> withDragCursor pos.clientX pos.clientY

        DraggedOverChordBar bar ->
            case model.chordDrag of
                Just cd ->
                    applyChordDragDelta (bar - Tuple.first cd.anchorKey) cd model

                Nothing ->
                    ( model, Cmd.none )

        ReleasedDrag ->
            (case model.pendingEmptyTouch of
                Just p ->
                    {- タッチで空白を押したまま指を動かさず離した（＝スワイプではなくタップ）と判断し、ここで初めてノートを確定配置する。
                       pendingNoteDragは付けず（指は既に離れているため）、押したまま伸ばすジェスチャはタッチでは提供しない。
                    -}
                    let
                        ( placed, cmd, _ ) =
                            insertNoteAt p { model | pendingEmptyTouch = Nothing }
                    in
                    ( placed, cmd )

                Nothing ->
                    releasedDragFallback model
            )
                |> Tuple.mapFirst (\m -> { m | dragCursor = Nothing })

        CanceledNotePress ->
            {- タッチで空白を押したものの指が動いてブラウザがネイティブスクロールに切り替えた（pointercancel）。配置せず保留を
               破棄し、ReleasedDragと同じ事後処理（他の保留状態の解除）に委譲する。
            -}
            updateCore ReleasedDrag { model | pendingEmptyTouch = Nothing }

        PressedRuler pos ->
            if pos.shift || model.touchMode == TouchSelect then
                let
                    anchor =
                        snapRound model (PianoRoll.pixelsToTicks model.pianoRollZoom pos.offsetX)
                in
                ( { model | loopDrag = Just { fixedTicks = anchor, baseTicks = anchor, startClientX = pos.clientX, curTicks = anchor } }, Cmd.none )

            else
                seekTo (snapFloor model (PianoRoll.pixelsToTicks model.pianoRollZoom pos.offsetX)) model

        PressedLoopHandle isEnd clientX ->
            case model.loopRange of
                Just loop ->
                    ( { model | loopDrag = Just (loopHandleDrag isEnd clientX loop) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        PressedSectionRuler pos ->
            let
                timeline =
                    Data.Project.timeline model.project

                fractionalBar =
                    pos.offsetX / toFloat model.sectionBarZoom

                pressTicks =
                    Data.Timeline.fractionalBarToTicks fractionalBar timeline

                insideViewRange =
                    if pianoRollScrollMounted model then
                        model.pianoRollViewportWidth
                            |> Maybe.map (\w -> PianoRoll.visibleTickRange model.pianoRollZoom { scrollX = model.pianoRollScrollX, width = w })
                            |> Maybe.map (\r -> pressTicks >= r.startTicks && pressTicks <= r.endTicks)
                            |> Maybe.withDefault False

                    else
                        False
            in
            if pos.shift || model.touchMode == TouchSelect then
                ( { model | sectionLoopDrag = Just { fixedTicks = pressTicks, baseTicks = pressTicks, startClientX = pos.clientX, curTicks = pressTicks } }, Cmd.none )

            else if insideViewRange then
                ( { model | viewRangeDrag = Just { startClientX = pos.clientX, pressOffsetX = pos.offsetX, origScrollX = model.pianoRollScrollX, moved = False } }, Cmd.none )

            else
                seekTo pressTicks model

        PressedSectionLoopHandle isEnd clientX ->
            case model.loopRange of
                Just loop ->
                    ( { model | sectionLoopDrag = Just (loopHandleDrag isEnd clientX loop) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        PressedPaneDivider clientX ->
            ( { model | paneDividerDrag = Just { startClientX = clientX, origWidth = model.leftPaneWidth } }, Cmd.none )

        ToggledChordProgressionModal ->
            if model.chordProgressionModalOpen then
                ( { model | chordProgressionModalOpen = False, chordSheetDraft = Nothing }, Cmd.none )

            else
                ( { model
                    | chordProgressionModalOpen = True
                    , chordSheetDraft = Just (Data.ChordSheet.toSheetText model.project.sections model.project.chordTrack |> Data.ChordTrack.wrapBarLines 4)
                  }
                , Cmd.none
                )

        DoubleClickedChordToken key ->
            openFormPickerFor model key

        DoubleClickedChordStripAt ticks ->
            let
                timeline =
                    Data.Project.timeline model.project
            in
            case Data.ChordTrack.tokenKeyAtTicks timeline ticks model.project.chordTrack of
                Just key ->
                    openFormPickerFor model key

                Nothing ->
                    ( model, Cmd.none )

        ClosedFormPicker ->
            let
                committedModel =
                    commitFormPickerDraft model
            in
            ( { committedModel
                | formPicker = Nothing
                , hoveredFretCell = Nothing
                , editingVoicingIndex = Nothing
                , voicingSelectedOffsets = Set.empty
                , voicingDragState = NoVoicingDrag
                , pendingVoicingDrag = Nothing
                , pendingVoicingDelete = Nothing
              }
            , Cmd.none
            )

        ChoseChordForm key candidate ->
            let
                committedModel =
                    commitFormPickerDraft model
            in
            case registerCandidateForToken key candidate committedModel of
                Just ( registeredModel, name ) ->
                    finishRegistrationAndOpenEditTab name registeredModel

                Nothing ->
                    ( { committedModel | formPicker = Nothing }, Cmd.none )

        ChoseVoicingShape key shape ->
            let
                committedModel =
                    commitFormPickerDraft model
            in
            case registerShapeForToken key shape committedModel of
                Just ( registeredModel, name ) ->
                    finishRegistrationAndOpenEditTab name registeredModel

                Nothing ->
                    ( { committedModel | formPicker = Nothing }, Cmd.none )

        ClearedChordVoicing key ->
            let
                committedModel =
                    commitFormPickerDraft model

                timeline =
                    Data.Project.timeline committedModel.project

                clearedModel =
                    { committedModel
                        | project = Data.Project.updateChordTrack (Data.ChordTrack.updateToken timeline key Data.ChordTrack.withoutVoicingName) committedModel.project
                      }
                        |> refreshFormPickerDraft

                wasEditTab =
                    case clearedModel.formPicker of
                        Just fp ->
                            fp.tab == FormPicker.EditTab

                        Nothing ->
                            False
            in
            if wasEditTab then
                ( { clearedModel
                    | formPicker = Maybe.map (\fp -> { fp | tab = FormPicker.CandidatesTab }) clearedModel.formPicker
                    , editingVoicingIndex = Nothing
                    , voicingSelectedOffsets = Set.empty
                    , voicingDragState = NoVoicingDrag
                    , pendingVoicingDrag = Nothing
                    , pendingVoicingDelete = Nothing
                  }
                , Cmd.none
                )

            else
                ( clearedModel, Cmd.none )

        ChangedFormPickerDraft str ->
            ( { model | formPicker = Maybe.map (\fp -> { fp | draft = str }) model.formPicker }, Cmd.none )

        SubmittedFormPickerDraft ->
            ( commitFormPickerDraft model, Cmd.none )

        SelectedFormPickerTab tab ->
            let
                committedModel =
                    commitFormPickerDraft model
            in
            case tab of
                FormPicker.CandidatesTab ->
                    ( { committedModel
                        | formPicker = Maybe.map (\fp -> { fp | tab = FormPicker.CandidatesTab }) committedModel.formPicker
                        , editingVoicingIndex = Nothing
                        , voicingSelectedOffsets = Set.empty
                        , voicingDragState = NoVoicingDrag
                        , pendingVoicingDrag = Nothing
                        , pendingVoicingDelete = Nothing
                      }
                    , Cmd.none
                    )

                FormPicker.EditTab ->
                    case committedModel.formPicker of
                        Nothing ->
                            ( committedModel, Cmd.none )

                        Just fp ->
                            case ChordParser.parse fp.draft of
                                Err _ ->
                                    ( committedModel, Cmd.none )

                                Ok chord ->
                                    case chord.voicing |> Maybe.andThen (\name -> Data.Project.voicingIndexByName name committedModel.project.voicings) of
                                        Just index ->
                                            openEditTab index committedModel

                                        Nothing ->
                                            autoRegisterAndOpenEditTab fp.key chord committedModel

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
                , sectionMoveDrag = Just { sectionId = sectionId, lastClientX = clientX, accumDx = 0, moved = False, wasSelected = model.selectedSectionId == Just sectionId }
              }
            , Cmd.none
            )

        PressedPianoKey pitch ->
            ( { model | highlightedPitches = Set.singleton pitch }
            , Ports.toAudio (Performance.encodePreviewNote (selectedInstrumentName model) pitch)
            )

        PressedVoicingKeyboardKey pitch ->
            ( model
            , Ports.toAudio (Performance.encodePreviewNote (Data.Track.instrumentToString model.project.chordTrack.instrument) pitch)
            )

        GotKey k ->
            if k.key == "Escape" && isModalOpen model then
                closeTopModal model

            else if List.member k.targetTag [ "INPUT", "TEXTAREA", "SELECT" ] then
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
                                    Set.map (\o -> clamp -12 maxOffset (o + delta)) model.voicingSelectedOffsets
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

            else if isModalOpen model then
                ( model, Cmd.none )

            else if k.key == "?" then
                updateCore ToggledHelpModal model

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
                if model.selectedTrackId == Data.ChordTrack.trackId then
                    case model.selectedSectionId |> Maybe.andThen (\sid -> Data.Project.sectionBounds sid model.project) of
                        Just bounds ->
                            let
                                timeline =
                                    Data.Project.timeline model.project
                            in
                            ( { model | selectedChordKeys = Data.ChordTrack.tokenKeysInTickRange timeline bounds.startTicks bounds.endTicks model.project.chordTrack }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                else
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
                if model.selectedTrackId == Data.ChordTrack.trackId then
                    let
                        timeline =
                            Data.Project.timeline model.project
                    in
                    ( { model | selectedChordKeys = Data.ChordTrack.tokenSpans timeline model.project.chordTrack |> List.map .key |> Set.fromList }, Cmd.none )

                else
                    ( { model | selectedNoteIds = Set.fromList (List.map .id (trackNotes model)) }, Cmd.none )

            else if k.key == "Delete" || k.key == "Backspace" then
                if model.selectedTrackId == Data.ChordTrack.trackId then
                    let
                        timeline =
                            Data.Project.timeline model.project
                    in
                    ( { model
                        | project = Data.Project.updateChordTrack (Data.ChordTrack.removeTokens timeline model.selectedChordKeys) model.project
                        , selectedChordKeys = Set.empty
                      }
                    , Cmd.none
                    )

                else
                    ( deleteSelection model, Cmd.none )

            else if k.key == "Escape" then
                ( { model
                    | selectedNoteIds = Set.empty
                    , selectedChordKeys = Set.empty
                    , pendingSectionDelete = Nothing
                    , pendingTrackDelete = Nothing
                    , pendingScrapDelete = Nothing
                    , pendingNewProject = False
                    , tool = PianoRoll.PointerTool
                    , cutGuideTicks = Nothing
                    , touchMode = TouchNormal
                  }
                , Cmd.none
                )

            else if k.key == "c" && not k.ctrl && not k.meta && not model.showKeyboard then
                ( { model
                    | tool =
                        if model.tool == PianoRoll.PointerTool then
                            PianoRoll.CutTool

                        else
                            PianoRoll.PointerTool
                    , cutGuideTicks = Nothing
                  }
                , Cmd.none
                )

            else if k.key == "g" && not k.ctrl && not k.meta && not model.showKeyboard then
                if Set.isEmpty model.selectedNoteIds then
                    ( model, Cmd.none )

                else
                    let
                        result =
                            Data.Project.mergeNotes { trackId = model.selectedTrackId, targetIds = model.selectedNoteIds } model.project
                    in
                    ( { model | project = result.project, selectedNoteIds = result.newSelection }, Cmd.none )

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
                        Basics.max 0 (snapFloor model model.playheadTicks)

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
            let
                newModel =
                    { model | selectedTrackId = trackId, selectedNoteIds = Set.empty, selectedChordKeys = Set.empty, pendingTrackDelete = Nothing }
            in
            ( newModel, restorePianoRollScrollCmd newModel )

        ClickedAddTrack ->
            let
                newTrackId =
                    model.project.nextId

                newModel =
                    { model
                        | project = Data.Project.addTrack model.project
                        , selectedTrackId = newTrackId
                        , selectedNoteIds = Set.empty
                    }
            in
            ( newModel, restorePianoRollScrollCmd newModel )

        ClickedRemoveTrack trackId ->
            confirmTwice
                { pending = model.pendingTrackDelete
                , id = trackId
                , arm = ( { model | pendingTrackDelete = Just trackId }, Cmd.none )
                , confirm =
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
                }

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
                    let
                        importedSelectedTrackId =
                            Decode.decodeString ProjectJson.selectedTrackIdDecoder content
                                |> Result.withDefault Nothing
                    in
                    resetToProject project importedSelectedTrackId model

                Err _ ->
                    showToast Toast.Error "JSONの読み込みに失敗しました" model

        ChangedChordSheetText textValue ->
            let
                ( newProject, newError ) =
                    case Data.ChordSheet.parse textValue of
                        Ok blocks ->
                            ( Data.ChordSheet.applyToProjectPreservingIds blocks model.project, Nothing )

                        Err parseError ->
                            ( model.project, Just parseError )
            in
            ( { model | chordSheetDraft = Just textValue, project = newProject, selectedChordKeys = Set.empty, chordSheetError = newError }
            , Cmd.none
            )

        ClickedNewProject ->
            confirmTwice
                { pending = if model.pendingNewProject then Just () else Nothing
                , id = ()
                , arm = ( { model | pendingNewProject = True }, Cmd.none )
                , confirm = resetToProject Data.Project.empty Nothing model
                }

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

        ChangedChordRhythm rhythm ->
            ( { model | project = Data.Project.updateChordTrack (\ct -> { ct | rhythm = rhythm }) model.project }
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
                , pendingVoicingDrag = Nothing
              }
            , Cmd.none
            )

        ClickedVoicingRow index ->
            let
                opening =
                    model.editingVoicingIndex /= Just index

                scrollCmd =
                    if opening then
                        voicingKeyboardScrollCmd model index

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
                , pendingVoicingDrag = Nothing
              }
            , scrollCmd
            )

        ChangedVoicingName index newName ->
            ( { model | project = Data.Project.updateVoicing index (\v -> { v | name = newName }) model.project }, Cmd.none )

        PressedVoicingOffset index pitch pos ->
            let
                {- 残留したpendingVoicingDragがあっても、次のクリックで必ず健全化するように先頭でリセットする。 -}
                model1 =
                    { model | pendingVoicingDrag = Nothing }

                rootPitch =
                    Data.Voicing.anchorPitch + model1.voicingPreviewRoot

                offset =
                    pitch - rootPitch

                currentOffsets =
                    List.drop index model1.project.voicings |> List.head |> Maybe.map .offsets |> Maybe.withDefault []
            in
            if offset < 0 && not (List.member offset currentOffsets) then
                -- root より低い空き行。offsets は常に 0 以上の不変式なので新規追加できない
                ( model1, Cmd.none )

            else if (pos.shift || model1.touchMode == TouchSelect) && List.member offset currentOffsets then
                -- 埋まっている行を shift クリック: 複数選択のトグルのみ。ドラッグは開始しない
                ( { model1
                    | voicingSelectedOffsets =
                        if Set.member offset model1.voicingSelectedOffsets then
                            Set.remove offset model1.voicingSelectedOffsets

                        else
                            Set.insert offset model1.voicingSelectedOffsets
                  }
                , Cmd.none
                )

            else if List.member offset currentOffsets then
                -- 埋まっている行を素クリック: 選択を維持 or 単独選択に置き換えて保留状態でドラッグ開始を待つ
                let
                    sel =
                        if Set.member offset model1.voicingSelectedOffsets then
                            model1.voicingSelectedOffsets

                        else
                            Set.singleton offset

                    currentPicks =
                        List.drop index model1.project.voicings |> List.head |> Maybe.map .stringPicks |> Maybe.withDefault Set.empty
                in
                let
                    ( model2, cmd ) =
                        ( { model1
                            | voicingSelectedOffsets = sel
                            , pendingVoicingDrag =
                                Just
                                    { index = index
                                    , startClientX = pos.clientX
                                    , startClientY = pos.clientY
                                    , origOffsets = currentOffsets
                                    , origSelected = sel
                                    , origPicks = currentPicks
                                    }
                          }
                        , Ports.toAudio (Performance.encodePreviewNote (Data.Track.instrumentToString model1.project.chordTrack.instrument) pitch)
                        )
                in
                if pos.isTouch then
                    let
                        ( model3, armCmd ) =
                            armLongPress (LongPressVoicingOffset index pitch) model2

                        newTooltipToken =
                            model3.touchTooltipToken + 1

                        model4 =
                            { model3
                                | hoveredFretCell = Just { pitch = pitch, interval = modBy 12 offset, x = pos.clientX, y = pos.clientY }
                                , touchTooltipToken = newTooltipToken
                            }

                        tooltipCmd =
                            Task.perform (\_ -> ExpiredTouchTooltip newTooltipToken) (Process.sleep 1500)
                    in
                    ( model4, Cmd.batch [ cmd, armCmd, tooltipCmd ] )

                else
                    ( model2, cmd )

            else
                -- 空いている行をクリック: offset を追加して単独選択にし、その場で保留状態でドラッグ開始を待つ（ノートのPressedEmptyCellと同様、追加自体は1クリックで完結させる）
                let
                    newOffsets =
                        offset :: currentOffsets

                    currentPicks =
                        List.drop index model1.project.voicings |> List.head |> Maybe.map .stringPicks |> Maybe.withDefault Set.empty
                in
                ( { model1
                    | project = Data.Project.updateVoicing index (\v -> { v | offsets = newOffsets }) model1.project
                    , voicingSelectedOffsets = Set.singleton offset
                    , pendingVoicingDrag =
                        Just
                            { index = index
                            , startClientX = pos.clientX
                            , startClientY = pos.clientY
                            , origOffsets = newOffsets
                            , origSelected = Set.singleton offset
                            , origPicks = currentPicks
                            }
                  }
                , Ports.toAudio (Performance.encodePreviewNote (Data.Track.instrumentToString model1.project.chordTrack.instrument) pitch)
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

        ClickedResetVoicing index ->
            ( { model
                | project = Data.Project.updateVoicing index (\v -> { v | offsets = [], stringPicks = Set.empty }) model.project
                , voicingSelectedOffsets = Set.empty
                , voicingDragState = NoVoicingDrag
                , pendingVoicingDrag = Nothing
              }
            , Cmd.none
            )

        ClickedRemoveVoicing index ->
            confirmTwice
                { pending = model.pendingVoicingDelete
                , id = index
                , arm = ( { model | pendingVoicingDelete = Just index }, Cmd.none )
                , confirm =
                    ( { model
                        | project = Data.Project.removeVoicing index model.project
                        , pendingVoicingDelete = Nothing
                        , editingVoicingIndex = Nothing
                        , voicingSelectedOffsets = Set.empty
                        , voicingDragState = NoVoicingDrag
                        , pendingVoicingDrag = Nothing
                      }
                    , Cmd.none
                    )
                }

        ClickedCopyChordText ->
            ( { model | chordCopyFeedback = True }
            , Cmd.batch
                [ Ports.copyToClipboard (Data.ChordTrack.toPlainText model.project.chordTrack |> Data.ChordTrack.wrapBarLines 4)
                , Task.perform (\_ -> ResetCopyFeedback) (Process.sleep 2000)
                ]
            )

        ResetCopyFeedback ->
            ( { model | chordCopyFeedback = False }, Cmd.none )

        ClickedConvertChords ->
            let
                project =
                    model.project

                {- ピアノロールプレビューと完全に同じ previewNotes を使うことで、リズムプリセット（ストローク/
                   アルペジオ）が選ばれていればそのまま MIDI 化される。未選択（ベタうち）なら従来どおりブロックコードになる。
                -}
                previewedNotes =
                    Data.StrumExpand.previewNotes project.guitarFormEnabled (effectiveVoicings model) (Data.Project.timeline project) project.chordTrack

                trackId =
                    project.nextId

                ( notesRev, nextId2 ) =
                    List.foldl
                        (\n ( acc, nid ) ->
                            ( { id = nid, pitch = n.pitch, start = n.start, duration = n.duration, velocity = n.velocity } :: acc
                            , nid + 1
                            )
                        )
                        ( [], trackId + 1 )
                        previewedNotes

                newTrack =
                    { id = trackId
                    , name = "コード"
                    , instrument = project.chordTrack.instrument
                    , muted = False
                    , volume = 100
                    , kind = NoteTrack (List.reverse notesRev)
                    }
            in
            if List.isEmpty previewedNotes then
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
            confirmTwice
                { pending = model.pendingSectionDelete
                , id = sectionId
                , arm = ( { model | pendingSectionDelete = Just sectionId }, Cmd.none )
                , confirm =
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
                }

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

        PressedDrumCell { pitch, tick, offsetX, offsetY, clientX, clientY, shift, isTouch } ->
            let
                effShift =
                    shift || model.touchMode == TouchSelect
            in
            case ( findDrumNoteAt { pitch = pitch, tick = tick } model, effShift ) of
                ( Just note, True ) ->
                    ( { model
                        | selectedNoteIds =
                            if Set.member note.id model.selectedNoteIds then
                                Set.remove note.id model.selectedNoteIds

                            else
                                Set.insert note.id model.selectedNoteIds
                      }
                    , Cmd.none
                    )

                ( Nothing, True ) ->
                    ( { model
                        | rubberBand =
                            Just
                                { originX = offsetX
                                , originY = offsetY
                                , startClientX = clientX
                                , startClientY = clientY
                                , curX = offsetX
                                , curY = offsetY
                                }
                      }
                    , Cmd.none
                    )

                ( Just note, False ) ->
                    if isTouch then
                        armLongPress (LongPressDrumNote { pitch = pitch, tick = tick }) { model | selectedNoteIds = Set.singleton note.id }

                    else
                        ( { model | selectedNoteIds = Set.singleton note.id }, Cmd.none )

                ( Nothing, False ) ->
                    let
                        grid =
                            Data.Time.gridTicks model.gridUnit

                        note =
                            { id = model.project.nextId
                            , pitch = pitch
                            , start = tick
                            , duration = grid
                            , velocity = 100
                            }
                    in
                    ( { model
                        | project = Data.Project.addNote model.selectedTrackId note model.project
                        , selectedNoteIds = Set.singleton note.id
                      }
                    , Ports.toAudio (Performance.encodePreviewNote "drumKit" pitch)
                    )

        RightClickedDrumCell target ->
            removeDrumNoteAt target model

        DoubleClickedDrumCell target ->
            removeDrumNoteAt target model

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

        ClickedExportWav ->
            ( { model | wavExportModalOpen = True, wavExportState = WavExportIdle }, Cmd.none )

        ClosedWavExportModal ->
            ( { model | wavExportModalOpen = False }, Cmd.none )

        ConfirmedExportWav useLoopRange ->
            let
                loop =
                    if useLoopRange then
                        currentLoop model

                    else
                        Nothing
            in
            ( { model | wavExportState = WavExportRendering }
            , Ports.toAudio (Performance.encodeRenderWav { loop = loop, fileName = model.project.name ++ ".wav" } model.project)
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
                            Basics.max 0 (snapFloor model model.playheadTicks)

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
            confirmTwice
                { pending = model.pendingScrapDelete
                , id = scrapId
                , arm = ( { model | pendingScrapDelete = Just scrapId }, Cmd.none )
                , confirm = ( { model | project = Data.Project.removeScrap scrapId model.project, pendingScrapDelete = Nothing }, Cmd.none )
                }

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
            let
                newModel =
                    { model | drumViewRoll = not model.drumViewRoll, selectedNoteIds = Set.empty }
            in
            ( newModel, restorePianoRollScrollCmd newModel )

        ToggledChordBlockView ->
            let
                newModel =
                    { model | chordBlockView = not model.chordBlockView }
            in
            ( newModel, restorePianoRollScrollCmd newModel )

        HoveredNote note x y ->
            ( { model | hoveredNote = Just { note = note, x = x, y = y } }, Cmd.none )

        UnhoveredNote ->
            ( { model | hoveredNote = Nothing }, Cmd.none )

        HoveredFretCell h ->
            ( { model | hoveredFretCell = Just h }, Cmd.none )

        UnhoveredFretCell ->
            ( { model | hoveredFretCell = Nothing }, Cmd.none )

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

        InsertedBarsAtPlayhead ->
            ( { model | project = Data.Project.insertBars { beforeBar = playheadBar model, count = parseInsertCount model.insertCountInput } model.project }
            , Cmd.none
            )

        RemovedBarsAtPlayhead ->
            ( { model | project = Data.Project.removeBars { fromBar = playheadBar model, count = parseInsertCount model.insertCountInput } model.project }
            , Cmd.none
            )

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

        ChangedGridUnit raw ->
            case Data.Time.gridUnitFromString raw of
                Just unit ->
                    ( { model | gridUnit = unit }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ToggledFollowPlayhead ->
            ( { model | followPlayhead = not model.followPlayhead }, Cmd.none )

        ToggledMetronome ->
            let
                newModel =
                    { model | metronomeEnabled = not model.metronomeEnabled }
            in
            ( newModel
            , if model.playState == Playing then
                Ports.toAudio (Performance.encodeUpdateEvents { metronome = newModel.metronomeEnabled, metronomeVolume = newModel.metronomeVolume } newModel.project)

              else
                Cmd.none
            )

        ChangedMetronomeVolume raw ->
            case String.toInt raw of
                Just vol ->
                    let
                        clamped =
                            clamp 0 100 vol
                    in
                    ( { model | metronomeVolume = clamped }
                    , Ports.toAudio (Performance.encodeSetVolume Performance.metronomeTrackId clamped)
                    )

                Nothing ->
                    ( model, Cmd.none )

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
                        newZoom =
                            PianoRoll.zoomStep w.deltaY model.pianoRollZoom
                    in
                    ( { model | pianoRollZoom = newZoom }
                    , zoomScrollCmd
                        { scrollId = PianoRoll.pianoRollScrollId
                        , offsetX = w.offsetX
                        , viewportX = viewport.viewport.x
                        , oldPxToTicks = PianoRoll.pixelsToTicks model.pianoRollZoom
                        , newTicksToPx = PianoRoll.ticksToPixels newZoom
                        }
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
                        newZoom =
                            SectionBar.regionZoomStep w.deltaY model.sectionBarZoom
                    in
                    ( { model | sectionBarZoom = newZoom }
                    , zoomScrollCmd
                        { scrollId = SectionBar.sectionBarScrollId
                        , offsetX = w.offsetX
                        , viewportX = viewport.viewport.x
                        , oldPxToTicks = \px -> Data.Timeline.fractionalBarToTicks (px / toFloat model.sectionBarZoom) timeline
                        , newTicksToPx = \ticks -> Data.Timeline.ticksToFractionalBar ticks timeline * toFloat newZoom
                        }
                    )

                Err _ ->
                    ( { model | sectionBarZoom = SectionBar.regionZoomStep w.deltaY model.sectionBarZoom }, Cmd.none )

        ScrolledPianoRoll v ->
            ( { model | pianoRollScrollX = v.scrollLeft, pianoRollViewportWidth = Just v.clientWidth }, Cmd.none )

        GotPianoRollViewportMeasured result ->
            case result of
                Ok viewport ->
                    ( { model | pianoRollScrollX = viewport.viewport.x, pianoRollViewportWidth = Just viewport.viewport.width }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ResizedWindow w h ->
            let
                model1 =
                    { model | windowSize = { width = w, height = h } }

                {- 狭画面に遷移した時は、進行中のペイン幅ドラッグをリセットする（ディバイダーを描画しなくなるための保険）。 -}
                model2 =
                    if isSinglePaneLayout model1 then
                        { model1 | paneDividerDrag = Nothing }

                    else
                        model1
            in
            if pianoRollScrollMounted model2 then
                ( model2, Task.attempt GotPianoRollViewportMeasured (Browser.Dom.getViewportOf PianoRoll.pianoRollScrollId) )

            else
                ( model2, Cmd.none )

        SelectedNarrowPane pane ->
            ( { model | narrowPane = pane }, Cmd.none )

        ToggledHeaderMenu ->
            ( { model | headerMenuOpen = not model.headerMenuOpen }, Cmd.none )

        ToggledSectionEditPanel ->
            let
                effective =
                    Maybe.withDefault (not (isShortViewport model)) model.sectionEditPanelOpen
            in
            ( { model | sectionEditPanelOpen = Just (not effective) }, Cmd.none )

        ToggledLeftPaneCollapsed ->
            ( { model | leftPaneCollapsed = not model.leftPaneCollapsed }, Cmd.none )

        DismissedToast id ->
            case model.toast of
                Just t ->
                    if t.id == id then
                        ( { model | toast = Nothing }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ToggledHelpModal ->
            ( { model | helpModalOpen = not model.helpModalOpen }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


dragMove : ClientPos -> DragInfo -> Model -> ( Model, Cmd Msg )
dragMove pos d model =
    let
        rawDticks =
            PianoRoll.pixelsToTicks model.pianoRollZoom (pos.clientX - d.startClientX)

        snapIfNotAlt ticks =
            if pos.alt || model.touchSnapOff then
                ticks

            else
                snapRound model ticks

        minDuration =
            if pos.alt || model.touchSnapOff then
                1

            else
                Data.Time.gridTicks model.gridUnit
    in
    case d.mode of
        MoveNote ->
            let
                dpitch =
                    negate (Basics.round ((pos.clientY - d.startClientY) / toFloat (PianoRoll.rowHeight (isTouchLayout model))))

                project2 =
                    List.foldl
                        (\orig proj ->
                            Data.Project.updateNote model.selectedTrackId
                                orig.id
                                (\n ->
                                    { n
                                        | start = Basics.max 0 (snapIfNotAlt (orig.start + rawDticks))
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

        ResizeLeft ->
            let
                project2 =
                    List.foldl
                        (\orig proj ->
                            Data.Project.updateNote model.selectedTrackId
                                orig.id
                                (\n ->
                                    let
                                        endTicks =
                                            orig.start + orig.duration

                                        newStart =
                                            clamp 0 (endTicks - minDuration) (snapIfNotAlt (orig.start + rawDticks))
                                    in
                                    { n | start = newStart, duration = endTicks - newStart }
                                )
                                proj
                        )
                        model.project
                        d.origNotes
            in
            ( { model | project = project2 }, Cmd.none )

        ResizeRight ->
            let
                project2 =
                    List.foldl
                        (\orig proj ->
                            Data.Project.updateNote model.selectedTrackId
                                orig.id
                                (\n -> { n | duration = Basics.max minDuration (snapIfNotAlt (orig.duration + rawDticks)) })
                                proj
                        )
                        model.project
                        d.origNotes
            in
            ( { model | project = project2 }, Cmd.none )


{-| 手動テーマトグルのボタンラベル。現在の状態を絵文字付きで表示する。
-}
themeToggleLabel : Theme.ThemePreference -> String
themeToggleLabel pref =
    case pref of
        Theme.SystemTheme ->
            "🖥️ OS設定"

        Theme.LightTheme ->
            "☀️ ライト"

        Theme.DarkTheme ->
            "🌙 ダーク"


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
            if model.selectedTrackId == Data.ChordTrack.trackId then
                "コード進行"

            else
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

        chordEditorConfig =
            { changedChordSheetText = ChangedChordSheetText
            , convertToTrack = ClickedConvertChords
            , changedRhythm = ChangedChordRhythm
            , toggledVoicingEnabled = ToggledVoicingEnabled
            , toggledGuitarFormEnabled = ToggledGuitarFormEnabled
            , clickedCopyText = ClickedCopyChordText
            , clickedAddVoicing = ClickedAddVoicing
            , clickedVoicingRow = ClickedVoicingRow
            , changedVoicingName = ChangedVoicingName
            , pressedVoicingOffset = PressedVoicingOffset
            , draggedWhilePressingVoicingOffset = DraggedTo
            , releasedVoicingOffsetPress = ReleasedDrag
            , doubleClickedVoicingOffset = DoubleClickedVoicingOffset
            , pressedVoicingKeyboardKey = PressedVoicingKeyboardKey
            , pressedFretboardCell = PressedFretboardCell
            , doubleClickedFretboardCell = DoubleClickedFretboardCell
            , clickedPlayVoicing = ClickedPlayVoicing
            , clickedRemoveVoicing = ClickedRemoveVoicing
            , changedVoicingPreviewRoot = ChangedVoicingPreviewRoot
            , changedVoicingPresetQuality = ChangedVoicingPresetQuality
            , changedVoicingPresetShape = ChangedVoicingPresetShape
            , appliedVoicingPreset = AppliedVoicingPreset
            , clickedResetVoicing = ClickedResetVoicing
            , hoveredFretCell = HoveredFretCell
            , unhoveredFretCell = UnhoveredFretCell
            }

        voicingState =
            { voicings = model.project.voicings
            , enabled = model.project.voicingEnabled
            , guitarFormEnabled = model.project.guitarFormEnabled
            , editingIndex = model.editingVoicingIndex
            , pendingDelete = model.pendingVoicingDelete
            , copyFeedback = model.chordCopyFeedback
            , previewRootPc = model.voicingPreviewRoot
            , presetQualityName = model.voicingPresetQuality
            , presetShapeName = model.voicingPresetShape
            , selectedOffsets = model.voicingSelectedOffsets
            }

        chordSpans =
            Data.ChordTrack.namedSpans timeline model.project.chordTrack

        refWaveform =
            if Array.isEmpty model.refPeaks then
                Nothing

            else
                Just
                    { peaks = model.refPeaks
                    , peakDt = model.refPeakDt
                    , secsPerTick = 60 / (model.project.bpm * toFloat Data.Time.ppq)
                    , offsetMs = model.project.referenceAudio.offsetMs
                    }

        leftPaneChildren =
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
                , chordRow =
                    { select = SelectedTrack Data.ChordTrack.trackId
                    , toggleMute = ToggledChordMute
                    , changeInstrument = ChangedChordInstrument
                    , changeVolume = ChangedChordVolume
                    , toggledGhost = ToggledGhostTrack Data.ChordTrack.trackId
                    }
                }
                (totalBarsFor model.project)
                model.selectedTrackId
                model.instrumentLoad
                model.ghostTrackIds
                model.pendingTrackDelete
                model.project.chordTrack
                model.project.tracks
            , ChordEditor.view
                chordEditorConfig
                timeline
                model.playheadTicks
                voicingState
                model.project.chordTrack
            , ScaleGuide.view
                { changedTonic = ChangedGuideKeyTonic
                , changedMode = ChangedGuideKeyMode
                }
                { effectiveKey = effectiveGuideKey model
                , override = model.guideKeyOverride
                }
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
            , div [ style "font-size" "0.75rem", style "color" Theme.onSurfaceVariant, style "margin-top" "0.6rem", style "display" "flex", style "align-items" "center", style "gap" "0.5rem", style "flex-wrap" "wrap" ]
                [ button (Style.baseButton ++ [ onClick ToggledHelpModal ]) [ text "? ショートカット一覧" ]
                , text "ルーラーやコードのクリックで再生位置を移動、ダブルクリック/右クリックでノート削除"
                ]
            ]

        rightPaneChildren =
            [ let
                durationSelect =
                    div [ style "display" "flex", style "align-items" "center", style "gap" "0.4rem" ]
                        [ span [ style "font-size" "0.85rem", Html.Attributes.title "新規配置時のノートの長さ" ] [ text "音価: " ]
                        , Html.select [ onInput ChangedDefaultDuration ]
                            (List.map
                                (\( ticks, label_ ) ->
                                    Html.option
                                        [ value (String.fromInt ticks)
                                        , Html.Attributes.selected (ticks == model.defaultNoteDuration)
                                        ]
                                        [ text label_ ]
                                )
                                [ ( Data.Time.gridTicks Data.Time.SixteenthTriplet, "三連 16分音符" )
                                , ( Data.Time.ticksPerSixteenth, "16分音符" )
                                , ( Data.Time.gridTicks Data.Time.EighthTriplet, "三連 8分音符" )
                                , ( Data.Time.ticksPerSixteenth * 2, "8分音符" )
                                , ( Data.Time.ppq * 2 // 3, "三連 4分音符" )
                                , ( Data.Time.ticksPerSixteenth * 3, "付点8分音符" )
                                , ( Data.Time.ticksPerSixteenth * 4, "4分音符" )
                                , ( Data.Time.ticksPerSixteenth * 8, "2分音符" )
                                , ( Data.Time.ticksPerSixteenth * 16, "全音符" )
                                ]
                            )
                        ]

                gridSelect =
                    div [ style "display" "flex", style "align-items" "center", style "gap" "0.4rem" ]
                        [ span [ style "font-size" "0.85rem" ] [ text "グリッド: " ]
                        , Html.select [ onInput ChangedGridUnit ]
                            (List.map
                                (\unit ->
                                    Html.option
                                        [ value (Data.Time.gridUnitToString unit)
                                        , Html.Attributes.selected (unit == model.gridUnit)
                                        ]
                                        [ text (Data.Time.gridLabel unit) ]
                                )
                                [ Data.Time.Sixteenth, Data.Time.EighthTriplet, Data.Time.SixteenthTriplet ]
                            )
                        ]

                pianoRollConfig =
                    { pressedEmpty =
                        case model.tool of
                            PianoRoll.PointerTool ->
                                PressedEmptyCell

                            PianoRoll.CutTool ->
                                PressedCutAt
                    , pressedNote = PressedNote
                    , draggedWhilePressingNote = \pos -> DraggedTo { clientX = pos.clientX, clientY = pos.clientY, alt = pos.alt }
                    , releasedNotePress = ReleasedDrag
                    , canceledNotePress = CanceledNotePress
                    , doubleClickedNote = DoubleClickedNote
                    , rightClickedNote = RightClickedNote
                    , pressedRuler = PressedRuler
                    , pressedLoopHandle = PressedLoopHandle
                    , pressedKey = PressedPianoKey
                    , wheelZoomedRuler = WheelZoomedRuler
                    , clickedChord = ClickedChordAt
                    , doubleClickedChord = DoubleClickedChordStripAt
                    , hoveredNote = HoveredNote
                    , unhoveredNote = UnhoveredNote
                    , scrolled = ScrolledPianoRoll
                    , pressedVelocityBar = PressedVelocityBar
                    , movedCutGuide = MovedCutGuide
                    , clearedCutGuide = ClearedCutGuide
                    }

                pianoRollOpts =
                    { notes = trackNotes model
                    , selectedIds = model.selectedNoteIds
                    , playheadTicks = model.playheadTicks
                    , sections = sectionSpans model.project
                    , totalBars = totalBarsFor model.project
                    , rubberBand = rubberBandRect model.rubberBand
                    , highlightedPitch = Set.union model.highlightedPitches model.heldKeyPitches
                    , scalePitchClasses = Data.Key.scalePitchClasses (Data.Timeline.keyAt (scaleReferenceTicks model) timeline)
                    , loop = displayedLoop model.loopDrag model
                    , loopEditable = model.loopMode == LoopRange
                    , waveform = refWaveform
                    , ghostNoteGroups = ghostNoteGroups model timeline
                    , pxPerSixteenth = model.pianoRollZoom
                    , gridUnit = model.gridUnit
                    , chordSpans = chordSpans
                    , tool = model.tool
                    , cutGuideTicks = model.cutGuideTicks
                    , isNarrow = isTouchLayout model
                    , gridTouchAction =
                        if model.tool == PianoRoll.PointerTool && model.touchMode == TouchNormal then
                            "pan-x pan-y"
                            -- 空白スワイプをネイティブスクロールに委譲（ピンチズームは除外）

                        else
                            "none"
                            -- TouchSelect（矩形選択ドラッグ）・TouchSeek・CutToolは従来通り
                    }

                toolToggle =
                    div [ style "display" "flex", style "align-items" "center", style "gap" "0.4rem" ]
                        [ button
                            (Style.toggleButton (model.tool == PianoRoll.PointerTool)
                                ++ [ onClick (SelectedTool PianoRoll.PointerTool)
                                   , Html.Attributes.title "選択・移動・リサイズ（c でカットと切替、Escape でも戻る）"
                                   ]
                            )
                            [ text "🖱 選択" ]
                        , button
                            (Style.toggleButton (model.tool == PianoRoll.CutTool)
                                ++ [ onClick (SelectedTool PianoRoll.CutTool)
                                   , Html.Attributes.title "クリック位置でノートを分割（c で切替）"
                                   ]
                            )
                            [ text "✂ カット" ]
                        ]

                {- ホイールが使えないタッチ環境向けのズームボタン。既存のWheelZoomedRulerを直接呼び、offsetXは0（左端を基準にズーム）。
                   デスクトップでも無害なので常時表示。 -}
                pianoRollZoomButtons =
                    div [ style "display" "flex", style "align-items" "center", style "gap" "0.3rem" ]
                        [ button
                            (Style.baseButton
                                ++ [ onClick (WheelZoomedRuler { deltaY = 100, offsetX = 0 })
                                   , Html.Attributes.title "ピアノロールを縮小"
                                   ]
                            )
                            [ text "🔍－" ]
                        , button
                            (Style.baseButton
                                ++ [ onClick (WheelZoomedRuler { deltaY = -100, offsetX = 0 })
                                   , Html.Attributes.title "ピアノロールを拡大"
                                   ]
                            )
                            [ text "🔍＋" ]
                        ]

                editToolbar =
                    div
                        [ style "display" "flex"
                        , style "flex-wrap" "wrap"
                        , style "align-items" "center"
                        , style "column-gap" "0.9rem"
                        , style "row-gap" "0.3rem"
                        , style "margin-top" "0.4rem"
                        ]
                        [ span [ style "font-size" "0.9rem" ] [ text ("編集中: " ++ selectedTrackName ++ selectionInfo) ], durationSelect, gridSelect, toolToggle, pianoRollZoomButtons, touchModeToggleView model ]

                pianoRollView =
                    div [] [ editToolbar, PianoRoll.view pianoRollConfig pianoRollOpts ]

                chordParseErrors =
                    Data.ChordTrack.cells timeline model.project.chordTrack
                        |> List.concatMap
                            (\cell ->
                                cell.chords
                                    |> List.filterMap
                                        (\c ->
                                            case c.result of
                                                Err reason ->
                                                    Just ("小節" ++ String.fromInt (cell.barIndex + 1) ++ ": \"" ++ c.token ++ "\" を解釈できません（" ++ reason ++ "）")

                                                Ok _ ->
                                                    Nothing
                                        )
                            )

                chordTrackMainView =
                    div []
                        [ div [ style "margin-top" "0.5rem", style "display" "flex", style "align-items" "center", style "flex-wrap" "wrap", style "gap" "0.3rem" ]
                            [ span [ style "font-size" "0.9rem" ] [ text ("編集中: " ++ selectedTrackName ++ selectionInfo) ]
                            , button
                                (Style.baseButton
                                    ++ [ onClick ToggledChordProgressionModal
                                       , Html.Attributes.title "表示/非表示を切替え"
                                       ]
                                )
                                [ text
                                    (if model.chordProgressionModalOpen then
                                        "✦ コード進行を閉じる"

                                     else
                                        "✦ コード進行を編集"
                                    )
                                ]
                            , div [ style "display" "flex", style "gap" "0.2rem", style "margin-left" "0.4rem" ]
                                [ button
                                    (Style.toggleButton (not model.chordBlockView)
                                        ++ (if model.chordBlockView then
                                                [ onClick ToggledChordBlockView ]

                                            else
                                                []
                                           )
                                        ++ [ Html.Attributes.title "ライン表示" ]
                                    )
                                    [ text "📈 ライン" ]
                                , button
                                    (Style.toggleButton model.chordBlockView
                                        ++ (if model.chordBlockView then
                                                []

                                            else
                                                [ onClick ToggledChordBlockView ]
                                           )
                                        ++ [ Html.Attributes.title "ブロック表示" ]
                                    )
                                    [ text "📦 ブロック" ]
                                ]
                            , Style.divider
                            , span [ style "font-size" "0.75rem", style "color" Theme.onSurfaceVariant ] [ text "リズム:" ]
                            , ChordEditor.rhythmSelect chordEditorConfig model.project.chordTrack.rhythm
                            ]
                        , if model.chordProgressionModalOpen then
                            ChordEditor.progressionEditorView chordEditorConfig (Maybe.withDefault "" model.chordSheetDraft) model.chordSheetError

                          else
                            text ""
                        , if List.isEmpty chordParseErrors then
                            text ""

                          else
                            div [ style "margin-top" "0.4rem", style "font-size" "0.75rem", style "color" Theme.error ]
                                (List.map (\msg -> div [] [ text msg ]) chordParseErrors)
                        , if model.chordBlockView then
                            ChordBlocks.view
                                { clickedChord = pianoRollConfig.clickedChord
                                , doubleClickedToken = DoubleClickedChordToken
                                , pressedToken = PressedChordToken
                                , draggedWhilePressing = DraggedTo
                                , draggedOverBar = DraggedOverChordBar
                                , releasedPress = ReleasedDrag
                                }
                                timeline
                                model.playheadTicks
                                model.selectedChordKeys
                                model.project.chordTrack

                          else
                            PianoRoll.chordTrackView pianoRollConfig
                                pianoRollOpts
                                (Just (Data.StrumExpand.previewNotes model.project.guitarFormEnabled (effectiveVoicings model) timeline model.project.chordTrack))
                                { config =
                                    { pressedToken = PressedChordToken
                                    , pressedLane = PressedChordLane
                                    , doubleClickedToken = DoubleClickedChordToken
                                    , draggedWhilePressingToken = DraggedTo
                                    , releasedTokenPress = ReleasedDrag
                                    }
                                , tokenSpans = Data.ChordTrack.tokenSpans timeline model.project.chordTrack
                                , selectedKeys = model.selectedChordKeys
                                , rubberBand =
                                    model.chordRubberBand
                                        |> Maybe.map (\crb -> { x = Basics.min crb.originX crb.curX, w = abs (crb.curX - crb.originX) })
                                }
                        ]
              in
              if model.selectedTrackId == Data.ChordTrack.trackId then chordTrackMainView else case selectedTrackKind model of
                Just (DrumTrack _) ->
                    div []
                        [ div [ style "display" "flex", style "gap" "0.2rem", style "margin-top" "0.5rem" ]
                            [ button
                                (Style.toggleButton model.drumViewRoll
                                    ++ (if model.drumViewRoll then
                                            []

                                        else
                                            [ onClick ToggledDrumView ]
                                       )
                                    ++ [ Html.Attributes.title "ピアノロールで編集（選択・移動・コピペ）" ]
                                )
                                [ text "🎹 ピアノロール" ]
                            , button
                                (Style.toggleButton (not model.drumViewRoll)
                                    ++ (if model.drumViewRoll then
                                            [ onClick ToggledDrumView ]

                                        else
                                            []
                                       )
                                    ++ [ Html.Attributes.title "ステップグリッドで編集" ]
                                )
                                [ text "🥁 ステップグリッド" ]
                            ]
                        , if model.drumViewRoll then
                            pianoRollView

                          else
                            div []
                                [ div [ style "display" "flex", style "flex-wrap" "wrap", style "align-items" "center", style "column-gap" "0.9rem", style "row-gap" "0.3rem" ]
                                    [ span [ style "font-size" "0.9rem" ] [ text ("編集中: " ++ selectedTrackName ++ selectionInfo) ], gridSelect, touchModeToggleView model ]
                                , DrumEditor.view
                                    { pressedCell = PressedDrumCell
                                    , draggedWhilePressingCell = DraggedTo
                                    , releasedCellPress = ReleasedDrag
                                    , rightClickedCell = RightClickedDrumCell
                                    , doubleClickedCell = DoubleClickedDrumCell
                                    , pressedVelocityBar = PressedVelocityBar
                                    , appliedPreset = AppliedDrumPreset
                                    , changedFillBars = ChangedDrumFillBars
                                    , pressedRuler = PressedRuler
                                    , pressedLoopHandle = PressedLoopHandle
                                    , wheelZoomedRuler = WheelZoomedRuler
                                    , scrolled = ScrolledPianoRoll
                                    }
                                    { sections = sectionSpans model.project
                                    , totalBars = totalBarsFor model.project
                                    , fillBars = model.drumFillBars
                                    , notes = trackNotes model
                                    , selectedIds = model.selectedNoteIds
                                    , playheadTicks = model.playheadTicks
                                    , pxPerSixteenth = model.pianoRollZoom
                                    , gridUnit = model.gridUnit
                                    , loop = displayedLoop model.loopDrag model
                                    , loopEditable = model.loopMode == LoopRange
                                    , rubberBand = rubberBandRect model.rubberBand
                                    }
                                ]
                        ]

                _ ->
                    pianoRollView
            , Keyboard.view
                { pressedKey = PressedPianoKey
                , toggled = ToggledKeyboard
                }
                (Set.union model.highlightedPitches model.heldKeyPitches)
                model.showKeyboard
            ]

        headerMenuToggle =
            button
                (Style.toggleButton model.headerMenuOpen
                    ++ [ onClick ToggledHeaderMenu
                       , Html.Attributes.title "その他の操作（テーマ・ループ・BPM・小節・ファイル）"
                       , Html.Attributes.attribute "aria-label" "その他の操作メニュー"
                       ]
                )
                [ text "☰" ]

        transportGroup =
            div groupStyle
                [ button (Style.baseButton ++ [ onClick (SeekTo 0), Html.Attributes.title "曲の先頭へ", Html.Attributes.attribute "aria-label" "曲の先頭へ" ]) [ text "⏮" ]
                , button (Style.baseButton ++ [ onClick SeekPrevSection, Html.Attributes.title "このセクションの頭へ（連打で前へ遡る）", Html.Attributes.attribute "aria-label" "前のセクションへ" ]) [ text "⏪" ]
                , button (Style.toggleButton (model.playState == Playing) ++ [ onClick ClickedPlay, Html.Attributes.title "再生 (Space)" ]) [ text "▶ 再生" ]
                , button (Style.baseButton ++ [ onClick ClickedStop, Html.Attributes.title "停止 (Space)" ]) [ text "■ 停止" ]
                , button (Style.baseButton ++ [ onClick SeekNextSection, Html.Attributes.title "次のセクションの頭へ", Html.Attributes.attribute "aria-label" "次のセクションへ" ]) [ text "⏩" ]
                , button
                    (Style.toggleButton model.metronomeEnabled
                        ++ [ onClick ToggledMetronome
                           , Html.Attributes.title "メトロノーム（各拍でクリック音、小節頭はアクセント）"
                           ]
                    )
                    [ text "🕰 クリック" ]
                , Html.input
                    [ Html.Attributes.type_ "range"
                    , Html.Attributes.min "0"
                    , Html.Attributes.max "100"
                    , Html.Attributes.value (String.fromInt model.metronomeVolume)
                    , onInput ChangedMetronomeVolume
                    , Html.Attributes.style "width" "70px"
                    , Html.Attributes.title ("メトロノーム音量 " ++ String.fromInt model.metronomeVolume)
                    ]
                    []
                ]

        undoGroup =
            div groupStyle
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

        themeGroup =
            div groupStyle
                [ button
                    (Style.baseButton
                        ++ [ onClick ClickedThemeToggle
                           , Html.Attributes.title "テーマ切替（OS設定→ライト→ダーク→OS設定）"
                           , Html.Attributes.attribute "aria-label" "テーマ切替"
                           ]
                    )
                    [ text (themeToggleLabel model.themePreference) ]
                ]

        loopGroup =
            div groupStyle
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

        bpmGroup =
            div groupStyle
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

        barsGroup =
            div groupStyle
                [ label []
                    [ text "小節: "
                    , input
                        [ type_ "number"
                        , value model.insertCountInput
                        , onInput ChangedInsertCount
                        , style "width" "3.5rem"
                        ]
                        []
                    ]
                , button
                    (Style.baseButton
                        ++ [ onClick InsertedBarsAtPlayhead
                           , Html.Attributes.title "再生位置のある小節の前に指定数の小節を挿入する（コード進行の改行は崩れることがあります）"
                           ]
                    )
                    [ text "+ 挿入" ]
                , button
                    (Style.dangerButton
                        ++ [ onClick RemovedBarsAtPlayhead
                           , Html.Attributes.title "再生位置のある小節から指定数の小節を削除する（Ctrl/Cmd+Zで戻せます）"
                           , Html.Attributes.attribute "aria-label" "再生位置から小節を削除"
                           ]
                    )
                    [ text "✂ 小節削除" ]
                ]

        fileGroup =
            div groupStyle
                [ button
                    (Style.baseButton
                        ++ [ onClick ClickedNewProject
                           , Html.Attributes.title "現在の内容を破棄して新規作成（もう一度押すと確定。Ctrl/Cmd+Zで戻せます）"
                           , Html.Attributes.style "min-width" "6.5rem"
                           , Html.Attributes.style "text-align" "center"
                           ]
                        ++ (if model.pendingNewProject then
                                [ Html.Attributes.attribute "aria-label" "もう一度押すと新規作成を確定" ]

                            else
                                []
                           )
                    )
                    [ text
                        (if model.pendingNewProject then
                            "本当に新規？"

                         else
                            "新規"
                        )
                    ]
                , button (Style.baseButton ++ [ onClick ClickedExport ]) [ text "JSON書出" ]
                , button (Style.baseButton ++ [ onClick ClickedImport ]) [ text "JSON読込" ]
                , button (Style.baseButton ++ [ onClick ClickedExportMidi ]) [ text "MIDI書出" ]
                , button (Style.baseButton ++ [ onClick ClickedExportWav ]) [ text "WAV書出" ]
                ]

        statusGroup =
            div (groupStyle ++ Style.labelText)
                [ text (stateLabel ++ " — " ++ String.fromInt barBeat.bar ++ " 小節 " ++ String.fromInt barBeat.beat ++ " 拍目") ]
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "height" "100vh"
        , classList [ ( "touch-ui", isTouchLayout model ) ]
        ]
        [ Style.focusCss
        , Palette.globalCss
        , div
            [ style "padding"
                (if isShortViewport model then
                    "0.25rem 0.75rem 0 0.75rem"

                 else
                    "0.5rem 1rem 0 1rem"
                )
            , style "flex" "0 0 auto"
            ]
            [ if isShortViewport model then
                text ""

              else
                h1 [ style "font-size" "1.3rem", style "margin" "0 0 0.3rem 0" ] [ text "音書き otogaki" ]
            , div [ style "display" "flex", style "flex-wrap" "wrap", style "gap" "0.5rem", style "align-items" "center" ]
                (if isCompactHeader model then
                    [ transportGroup
                    , Style.divider
                    , undoGroup
                    , Style.divider
                    , headerMenuToggle
                    , statusGroup
                    ]
                        ++ (if model.headerMenuOpen then
                                [ div
                                    ([ style "display" "flex", style "flex-direction" "column", style "gap" "0.5rem", style "flex" "1 1 100%", style "padding" "0.5rem", style "background" Theme.surfaceContainerHigh, style "border-radius" Theme.shapeS ]
                                        ++ (if isShortViewport model then
                                                [ style "max-height" "45vh", style "overflow-y" "auto" ]

                                            else
                                                []
                                           )
                                    )
                                    [ themeGroup
                                    , loopGroup
                                    , bpmGroup
                                    , barsGroup
                                    , fileGroup
                                    ]
                                ]

                            else
                                []
                           )

                 else
                    [ transportGroup
                    , Style.divider
                    , undoGroup
                    , Style.divider
                    , themeGroup
                    , Style.divider
                    , loopGroup
                    , Style.divider
                    , bpmGroup
                    , Style.divider
                    , barsGroup
                    , Style.divider
                    , fileGroup
                    , Style.divider
                    , statusGroup
                    ]
                )
            , SectionBar.view
                { isNarrow = isTouchLayout model
                , isShort = isShortViewport model
                , editPanelOpen = Maybe.withDefault (not (isShortViewport model)) model.sectionEditPanelOpen
                }
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
                , seekToStart = ClickedSeekSectionStart
                , transpose = TransposedSection
                , pressedBlock = PressedSectionBlock
                , pressedResizeHandle = PressedSectionResizeHandle
                , wheelZoomed = WheelZoomedSectionBar
                , pressedRuler = PressedSectionRuler
                , pressedLoopHandle = PressedSectionLoopHandle
                , clickedChord = ClickedChordAt
                , doubleClickedChord = DoubleClickedChordStripAt
                , toggledEditPanel = ToggledSectionEditPanel
                }
                { pxPerBar = model.sectionBarZoom
                , loopEditable = model.loopMode == LoopRange
                , loop = displayedLoop model.sectionLoopDrag model
                , playheadTicks = model.playheadTicks
                , ticksToPx = \ticks -> Data.Timeline.ticksToFractionalBar ticks timeline * toFloat model.sectionBarZoom
                , viewRange =
                    if pianoRollScrollMounted model then
                        model.pianoRollViewportWidth
                            |> Maybe.map (\w -> PianoRoll.visibleTickRange model.pianoRollZoom { scrollX = model.pianoRollScrollX, width = w })

                    else
                        Nothing
                }
                chordSpans
                refWaveform
                { playingIndex = sectionAtTicks model.playheadTicks model.project
                }
                model.selectedSectionId
                model.project.sections
                model.pendingSectionDelete
                (model.sectionResizeDrag |> Maybe.map (\d -> { sectionId = d.sectionId, lengthBars = d.curLengthBars }))
            ]
        , if isSinglePaneLayout model then
            div [ style "flex" "1 1 auto", style "min-height" "0", style "display" "flex", style "flex-direction" "column", style "overflow" "hidden" ]
                [ narrowTabBar model.narrowPane
                , div [ style "flex" "1 1 auto", style "min-height" "0", style "overflow-y" "auto", style "padding" "0.5rem 1rem 1rem 1rem", style "box-sizing" "border-box" ]
                    (case model.narrowPane of
                        NarrowSide ->
                            leftPaneChildren

                        NarrowMain ->
                            rightPaneChildren
                    )
                ]

          else if model.leftPaneCollapsed then
            div [ style "flex" "1 1 auto", style "min-height" "0", style "display" "flex", style "overflow" "hidden" ]
                [ div
                    [ style "flex" "0 0 auto"
                    , style "width" "28px"
                    , style "display" "flex"
                    , style "align-items" "flex-start"
                    , style "justify-content" "center"
                    , style "padding-top" "0.3rem"
                    , style "background" Theme.surfaceContainerHigh
                    ]
                    [ button
                        (Style.baseButton
                            ++ [ onClick ToggledLeftPaneCollapsed
                               , style "padding" "0.3rem 0.2rem"
                               , Html.Attributes.title "サイドバーを開く"
                               ]
                        )
                        [ text "▶" ]
                    ]
                , div
                    [ style "flex" "1 1 auto"
                    , style "min-width" "0"
                    , style "overflow-y" "auto"
                    , style "padding" "0.5rem 1rem 1rem 1rem"
                    , style "box-sizing" "border-box"
                    ]
                    rightPaneChildren
                ]

          else
            div [ style "flex" "1 1 auto", style "min-height" "0", style "display" "flex", style "overflow" "hidden" ]
                [ div
                    [ style "width" (String.fromInt model.leftPaneWidth ++ "px")
                    , style "flex" "0 0 auto"
                    , style "overflow-y" "auto"
                    , style "overflow-x" "auto"
                    , style "padding" "0 1rem 1rem 1rem"
                    , style "box-sizing" "border-box"
                    ]
                    (div [ style "display" "flex", style "justify-content" "flex-end", style "margin" "0.3rem 0 0 0" ]
                        [ button
                            (Style.baseButton
                                ++ [ onClick ToggledLeftPaneCollapsed
                                   , Html.Attributes.title "サイドバーをたたむ"
                                   ]
                            )
                            [ text "◀" ]
                        ]
                        :: leftPaneChildren
                    )
                , div
                    [ style "width" "10px"
                    , style "flex" "0 0 auto"
                    , style "cursor" "col-resize"
                    , style "background" Theme.outlineVariant
                    , style "touch-action" "none"
                    , Html.Events.on "pointerdown" (Decode.map PressedPaneDivider (Decode.field "clientX" Decode.float))
                    ]
                    []
                , div
                    [ style "flex" "1 1 auto"
                    , style "min-width" "0"
                    , style "overflow-y" "auto"
                    , style "padding" "0.5rem 1rem 1rem 1rem"
                    , style "box-sizing" "border-box"
                    ]
                    rightPaneChildren
                ]
        , if model.wavExportModalOpen then
            Modal.view { onClose = ClosedWavExportModal, noOp = NoOp }
                [ div [ style "min-width" "20rem" ]
                    (case model.wavExportState of
                        WavExportIdle ->
                            [ Html.h2 [] [ text "WAV書き出し" ]
                            , div [ style "display" "flex", style "flex-direction" "column", style "gap" "0.5rem" ]
                                [ button (Style.baseButton ++ [ onClick (ConfirmedExportWav False) ]) [ text "プロジェクト全体を書き出す" ]
                                , button
                                    (Style.baseButton
                                        ++ [ onClick (ConfirmedExportWav True)
                                           , disabled (currentLoop model == Nothing)
                                           ]
                                    )
                                    [ text "ループ範囲を書き出す" ]
                                ]
                            ]

                        WavExportRendering ->
                            [ Html.h2 [] [ text "WAV書き出し" ]
                            , div [] [ text "書き出し中…" ]
                            ]

                        WavExportFailed message ->
                            [ Html.h2 [] [ text "WAV書き出し" ]
                            , div [ style "color" Theme.error ] [ text ("書き出しに失敗しました: " ++ message) ]
                            , button (Style.baseButton ++ [ onClick ClickedExportWav ]) [ text "再試行" ]
                            ]
                    )
                ]

          else
            text ""
        , if model.helpModalOpen then
            Modal.view { onClose = ToggledHelpModal, noOp = NoOp } [ HelpPanel.view ]

          else
            text ""
        , case ( model.formPicker, model.editingVoicingIndex ) of
            ( Nothing, Just index ) ->
                case List.drop index model.project.voicings |> List.head of
                    Just voicing ->
                        Modal.view { onClose = ClickedVoicingRow index, noOp = NoOp }
                            [ ChordEditor.voicingEditorView chordEditorConfig voicingState index voicing ]

                    Nothing ->
                        text ""

            _ ->
                text ""
        , case model.formPicker of
            Just fp ->
                let
                    maybeChord =
                        Data.ChordTrack.tokenSpans timeline model.project.chordTrack
                            |> List.filter (\s -> s.key == fp.key)
                            |> List.head
                            |> Maybe.andThen
                                (\s ->
                                    case s.result of
                                        Ok (Data.ChordTrack.TChord chord) ->
                                            Just chord

                                        _ ->
                                            Nothing
                                )

                    editorHtml =
                        model.editingVoicingIndex
                            |> Maybe.andThen (\index -> List.drop index model.project.voicings |> List.head |> Maybe.map (\voicing -> ( index, voicing )))
                            |> Maybe.map (\( index, voicing ) -> ChordEditor.voicingEditorView chordEditorConfig voicingState index voicing)
                in
                case maybeChord of
                    Just _ ->
                        Modal.view { onClose = ClosedFormPicker, noOp = NoOp }
                            [ FormPicker.view
                                { chose = ChoseChordForm fp.key
                                , choseShape = ChoseVoicingShape fp.key
                                , cleared = ClearedChordVoicing fp.key
                                , hoveredFret = HoveredFretCell
                                , unhoveredFret = UnhoveredFretCell
                                , changedDraft = ChangedFormPickerDraft
                                , submittedDraft = SubmittedFormPickerDraft
                                , selectedTab = SelectedFormPickerTab
                                }
                                { draft = fp.draft, tab = fp.tab, editor = editorHtml }
                            ]

                    Nothing ->
                        text ""

            Nothing ->
                text ""
        , hoveredNoteTooltipView model timeline
        , hoveredFretCellTooltipView model
        , if isDragging model then
            viewDragOverlay

          else
            text ""
        , viewDragGhost model
        , case model.toast of
            Just t ->
                Toast.view DismissedToast t

            Nothing ->
                text ""
        ]


{-| ポインター化したドラッグ中だけ全画面に重ねる透明のdiv。ElmのBrowser.Eventsには
pointermove/pointerupの購読が存在しないため、代わりにこの要素自体にpointermove/pointerup/pointercancelを
張ってドラッグ中のポインターを追跡する。既存のDraggedTo/ReleasedDragをそのまま再利用するので、
update側の処理は一切変えなくてよい。
-}
viewDragOverlay : Html Msg
viewDragOverlay =
    div
        [ style "position" "fixed"
        , style "inset" "0"
        , style "z-index" Theme.zDragOverlay
        , style "touch-action" "none"
        , style "cursor" "grabbing"
        , Html.Events.on "pointermove" (Decode.map DraggedTo clientPosDecoder)
        , Html.Events.on "pointerup" (Decode.succeed ReleasedDrag)
        , Html.Events.on "pointercancel" (Decode.succeed ReleasedDrag)
        ]
        []


{-| ドラッグ中の対象をマウスに追従するチップ。pointer-events: none でクリック/pointerenterを吐かず、
下のDOM要素（ChordBlocksのセルの pointerenter など）にイベントを透過させる。dragCursor と ghostContent が
両方 Just のときだけ描画されるので、ドラッグ終了後に残らない。
-}
viewDragGhost : Model -> Html Msg
viewDragGhost model =
    case ( model.dragCursor, ghostContent model ) of
        ( Just pos, Just label ) ->
            div
                [ style "position" "fixed"
                , style "left" (String.fromFloat (pos.x + 12) ++ "px")
                , style "top" (String.fromFloat (pos.y + 12) ++ "px")
                , style "background" Theme.inverseSurface
                , style "color" Theme.inverseOnSurface
                , style "padding" "0.35rem 0.55rem"
                , style "border-radius" Theme.shapeXS
                , style "font-size" "0.75rem"
                , style "font-weight" "600"
                , style "pointer-events" "none"
                , style "z-index" Theme.zDragGhost
                , style "white-space" "nowrap"
                , style "box-shadow" Theme.elevation2
                ]
                [ text label ]

        _ ->
            text ""


{-| 現在ドラッグ中の対象の表示ラベル。各種ドラッグ状態が増えるたびここに分岐を足す。
-}
ghostContent : Model -> Maybe String
ghostContent model =
    case model.chordDrag of
        Just cd ->
            Just cd.ghostLabel

        Nothing ->
            Nothing


{-| ノートホバー時のツールチップ。ホバー中ノートと同じトラック（通常ピアノロールなら選択中トラック、
コード進行トラックのMIDIプレビューならプレビューノート列）から、ホバー中ノートの開始時刻を含む区間を
持つノートを集めてコードを自動判別する。単音（1個）の場合はコード行を出さない。
-}
hoveredNoteTooltipView : Model -> Timeline -> Html Msg
hoveredNoteTooltipView model timeline =
    case model.hoveredNote of
        Nothing ->
            text ""

        Just h ->
            let
                candidateLists =
                    [ trackNotes model
                    , Data.StrumExpand.previewNotes model.project.guitarFormEnabled (effectiveVoicings model) timeline model.project.chordTrack
                    ]

                siblings =
                    candidateLists
                        |> List.filter (List.member h.note)
                        |> List.head
                        |> Maybe.withDefault [ h.note ]

                simultaneous =
                    siblings
                        |> List.filter (\n -> n.start <= h.note.start && h.note.start < n.start + n.duration)

                chordLabel =
                    if List.length simultaneous >= 2 then
                        Data.Chord.Detect.detect (List.map .pitch simultaneous)

                    else
                        Nothing

                velocityPct =
                    toFloat h.note.velocity / 127 * 100

                velocityText =
                    String.fromFloat (toFloat (round (velocityPct * 10)) / 10) ++ "%"
            in
            div
                [ style "position" "fixed"
                , style "left" (String.fromFloat (h.x + 12) ++ "px")
                , style "top" (String.fromFloat (h.y + 12) ++ "px")
                , style "background" Theme.inverseSurface
                , style "color" Theme.inverseOnSurface
                , style "padding" "0.35rem 0.55rem"
                , style "border-radius" Theme.shapeXS
                , style "font-size" "0.75rem"
                , style "pointer-events" "none"
                , style "z-index" "1000"
                , style "white-space" "nowrap"
                ]
                (div []
                    [ text
                        (Data.Note.pitchLabel h.note.pitch
                            ++ ": 長さ: "
                            ++ Data.Time.formatDuration h.note.duration
                            ++ " | ベロシティー: "
                            ++ velocityText
                        )
                    ]
                    :: (case chordLabel of
                            Just name ->
                                [ div [] [ text ("コード: " ++ name) ] ]

                            Nothing ->
                                []
                       )
                )


{-| 指板セルホバー時のツールチップ。音名（`Data.Note.pitchLabel`）・度数（`ChordFormat.degreeLabel`）・半音数を並べて表示する（例：「E 4 ・ 3 (+4)」）。
ノートホバーツールチップと同じ位置規則（`h.x + 12` / `h.y + 12`、position: fixed）。
-}
hoveredFretCellTooltipView : Model -> Html Msg
hoveredFretCellTooltipView model =
    case model.hoveredFretCell of
        Nothing ->
            text ""

        Just h ->
            div
                [ style "position" "fixed"
                , style "left" (String.fromFloat (h.x + 12) ++ "px")
                , style "top" (String.fromFloat (h.y + 12) ++ "px")
                , style "background" Theme.inverseSurface
                , style "color" Theme.inverseOnSurface
                , style "padding" "0.35rem 0.55rem"
                , style "border-radius" Theme.shapeXS
                , style "font-size" "0.75rem"
                , style "pointer-events" "none"
                , style "z-index" "1000"
                , style "white-space" "nowrap"
                ]
                [ text
                    (Data.Note.pitchLabel h.pitch
                        ++ " ・ "
                        ++ ChordFormat.degreeLabel h.interval
                        ++ " (+"
                        ++ String.fromInt h.interval
                        ++ ")"
                    )
                ]


{-| ビューポートの高さが低いか（横向きフォンを想定）。高さ500px未満を基準とする。
横向きフォンのCSS高さは実質~300-430、タブレット横の高さ下限（iPad mini）が744、デスクトップの実用
ウィンドウ高はほぼ常に≥600なので、その間の空白帯に500を置く。小型デスクトップウィンドウを極端に低くした場合も
誤爆しうるが、ハンバーガーメニュー経由で全機能到達可能なので機能喪失はない（graceful degradation）。
-}
isShortViewport : Model -> Bool
isShortViewport model =
    model.windowSize.height > 0 && model.windowSize.height < 500


{-| ヘッダーツールバーをハンバーガーに折りたたむか。幅800px未満（従来の基準）または低高さ
（isShortViewport）のときに折りたたむ。横向きフォン（幅は広くても高さが低い）をカバーするため、
幅基準のみだった従来より広い条件でコンパクト化する。bodyの2ペイン/1ペイン判定（isSinglePaneLayout）とは独立。
-}
isCompactHeader : Model -> Bool
isCompactHeader model =
    (model.windowSize.width > 0 && model.windowSize.width < 800) || isShortViewport model


{-| タッチ寸法（PianoRoll rowHeight拡大、SectionBarハンドル拡大、.m3-btnパディング拡大）を適用するか。
幅1200px未満を基準とする—全フォン、iPad縦（744-1024）、iPad横11"以下（1024-1194）をカバーする。
既知の限界: iPad Pro 12.9"/13"横（~1366）は幅のみでは判別できずマウス寸法のままになる。
1280-1536px級のラップトップを誤爆させないためのトレードオフとして受容。width == 0（起動直後）は広い扱い。
-}
isTouchLayout : Model -> Bool
isTouchLayout model =
    model.windowSize.width > 0 && model.windowSize.width < 1200


{-| 2ペインレイアウト（左ペイン+ディバイダー+右ペイン）を成立させるには幅が足りないか。
2ペインの最低成立幅 ≈ leftPaneWidth初期値380 + ディバイダー10 + メイン編集部~600 ≈ 990に基づき
幅1000を固定定数とする（leftPaneWidth依存の動的式はドラッグで広げた瞬間に1ペインに崩落する
自己破壊的ヒステリシスになるため不採用）。width == 0（起動直後）は2ペイン成立扱い（False）。
-}
isSinglePaneLayout : Model -> Bool
isSinglePaneLayout model =
    model.windowSize.width > 0 && model.windowSize.width < 1000


{-| Shift/Alt/Ctrlなどの物理修飾キーを押せないタッチ環境向けの代替モード切替ボタン。
グローバルなModel状態なので、複数のエディタに同じものを複数回描画しても安全。
デスクトップでも便利なので常時表示。
-}
touchModeToggleView : Model -> Html Msg
touchModeToggleView model =
    div [ style "display" "flex", style "align-items" "center", style "gap" "0.3rem", style "flex-wrap" "wrap" ]
        [ span [ style "font-size" "0.85rem", Html.Attributes.title "Shift/Alt/Ctrlなどの物理修飾キーの代替操作" ] [ text "修飾: " ]
        , button
            (Style.toggleButton (model.touchMode == TouchNormal)
                ++ [ onClick (SelectedTouchMode TouchNormal)
                   , Html.Attributes.title "通常操作"
                   ]
            )
            [ text "通常" ]
        , button
            (Style.toggleButton (model.touchMode == TouchSelect)
                ++ [ onClick (SelectedTouchMode TouchSelect)
                   , Html.Attributes.title "タップで複数選択・ドラッグでループ/矩形選択（Shift相当）"
                   ]
            )
            [ text "選択" ]
        , button
            (Style.toggleButton (model.touchMode == TouchSeek)
                ++ [ onClick (SelectedTouchMode TouchSeek)
                   , Html.Attributes.title "空白タップでシーク（Ctrl相当）"
                   ]
            )
            [ text "シーク" ]
        , button
            (Style.toggleButton model.touchSnapOff
                ++ [ onClick ToggledTouchSnapOff
                   , Html.Attributes.title "ドラッグ中のスナップを無効化（Alt相当）"
                   ]
            )
            [ text "スナップOFF" ]
        ]


{-| 狭画面でのタブ切替バー。「編集」（NarrowMain、右ペイン相当）と「トラック・素材」（NarrowSide、左ペイン相当）の2つを
切り替える。FormPicker.elmのタブバーと同様、Style.toggleButtonを使う。
-}
narrowTabBar : NarrowPane -> Html Msg
narrowTabBar current =
    div [ style "display" "flex", style "gap" "0.3rem", style "padding" "0.3rem 1rem", style "flex" "0 0 auto" ]
        [ button
            (Style.toggleButton (current == NarrowMain) ++ [ onClick (SelectedNarrowPane NarrowMain) ])
            [ text "編集" ]
        , button
            (Style.toggleButton (current == NarrowSide) ++ [ onClick (SelectedNarrowPane NarrowSide) ])
            [ text "トラック・素材" ]
        ]


{-| いずれかのドラッグが進行中か。trueの間だけviewDragOverlayを出してpointermove/pointerupを拾う。
全ドラッグ種がpointerdown化済みなので、不要になったマウスイベント購読は削除済み。
-}
isDragging : Model -> Bool
isDragging model =
    model.paneDividerDrag
        /= Nothing
        || model.dragState
        /= NoDrag
        || model.rubberBand
        /= Nothing
        || model.loopDrag
        /= Nothing
        || model.voicingDragState
        /= NoVoicingDrag
        || model.sectionResizeDrag
        /= Nothing
        || model.sectionMoveDrag
        /= Nothing
        || model.sectionLoopDrag
        /= Nothing
        || (model.chordDrag /= Nothing && not model.chordBlockView)
        || model.chordRubberBand
        /= Nothing
        || model.velocityDrag
        /= Nothing
        || model.viewRangeDrag
        /= Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.fromAudio (AudioMsg.decode >> GotAudio)
        , Browser.Events.onKeyDown (Decode.map GotKey keyEventDecoder)
        , Browser.Events.onKeyUp (Decode.map ReleasedKey (Decode.field "key" Decode.string))
        , Browser.Events.onResize ResizedWindow
        ]


clientPosDecoder : Decode.Decoder ClientPos
clientPosDecoder =
    Decode.map3 ClientPos
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
        (Decode.field "altKey" Decode.bool)


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
