module View.PianoRoll exposing
    ( Config
    , Dims
    , ResizeHandle(..)
    , RulerHandlers
    , RulerOpts
    , SectionSpan
    , Tool(..)
    , centerScrollTop
    , chordTrackView
    , defaultPxPerSixteenth
    , frameContentHeight
    , headerHeight
    , isEditable
    , isSelectable
    , keyColumnWidth
    , maxPitch
    , maxPxPerSixteenth
    , medianPitch
    , minPitch
    , minPxPerSixteenth
    , noteMoveDecoder
    , noteTopLeft
    , pianoRollScrollId
    , pixelsToTicks
    , playheadLine
    , rowHeight
    , rubberBandView
    , rulerHeight
    , rulerViewWith
    , scrollDecoder
    , sectionTintWithHeight
    , ticksToPixels
    , velocityLaneHeight
    , velocityLaneViewWith
    , view
    , visibleTickRange
    , yToPitch
    , zoomStep
    )

import Array exposing (Array)
import Data.ChordTrack exposing (ChordSpan, TokenKey, TokenSpan)
import Data.Note exposing (Note)
import Data.Time
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events
import Json.Decode as Decode
import Set exposing (Set)
import Svg
import Svg.Attributes as SA
import Svg.Lazy
import View.ChordLane
import View.ChordStrip as ChordStrip
import View.Palette as Palette
import View.Style as Style
import View.Theme as Theme
import View.Zoom


type alias Config msg =
    { pressedEmpty : { offsetX : Float, offsetY : Float, clientX : Float, clientY : Float, shift : Bool, seekMod : Bool, isTouch : Bool } -> msg
    , pressedNote : Int -> ResizeHandle -> { clientX : Float, clientY : Float, shift : Bool, isTouch : Bool, timeStamp : Float } -> msg
    , draggedWhilePressingNote : { clientX : Float, clientY : Float, alt : Bool } -> msg
    , releasedNotePress : msg
    , canceledNotePress : msg
    , doubleClickedNote : Int -> msg
    , rightClickedNote : Int -> msg
    , pressedRuler : { offsetX : Float, clientX : Float, shift : Bool, isTouch : Bool } -> msg
    , pressedLoopHandle : Bool -> Float -> msg
    , pressedKey : Int -> msg
    , wheelZoomedRuler : { deltaY : Float, offsetX : Float } -> msg
    , clickedChord : Int -> msg
    , doubleClickedChord : Int -> msg
    , hoveredNote : Note -> Float -> Float -> msg
    , unhoveredNote : msg
    , scrolled : { scrollLeft : Float, scrollTop : Float, clientWidth : Float } -> msg
    , pressedVelocityBar : Int -> { clientX : Float, clientY : Float } -> msg
    , movedCutGuide : { offsetX : Float } -> msg
    , clearedCutGuide : msg
    }


{-| ノート矩形のどの部分を掴んだか。NoResize = 本体（移動）、
ResizeLeft = 左端ハンドル（開始位置変更・終了固定）、ResizeRight = 右端ハンドル（長さ変更）。
-}
type ResizeHandle
    = NoResize
    | ResizeLeft
    | ResizeRight


{-| ピアノロールの操作モード。PointerTool = 選択・移動・リサイズ（既定）、CutTool = クリック位置で選択ノートを分割する。
LockTool = 編集を全て止める閲覧モード。スクロールとノートのタップ選択（+長押し矩形選択）だけ残す。
-}
type Tool
    = PointerTool
    | CutTool
    | LockTool


{-| ノートをつかんで動かす・長さを変える・削除できるか。PointerTool だけ True。 -}
isEditable : Tool -> Bool
isEditable tool =
    tool == PointerTool


{-| ノートをタップ・クリックで選択できるか。ロック中も選択だけは残す。 -}
isSelectable : Tool -> Bool
isSelectable tool =
    tool == PointerTool || tool == LockTool


type alias SectionSpan =
    { name : String
    , startBar : Int
    , lengthBars : Int
    }


type alias ViewOpts =
    { notes : List Note
    , selectedIds : Set Int
    , playheadTicks : Int
    , sections : List SectionSpan
    , totalBars : Int
    , rubberBand : Maybe { x : Float, y : Float, w : Float, h : Float }
    , waveform : Maybe Waveform
    , ghostNoteGroups : List (List Note)
    , highlightedPitch : Set Int
    , scalePitchClasses : Set Int
    , loop : Maybe { startTicks : Int, endTicks : Int }
    , loopEditable : Bool
    , pxPerSixteenth : Int
    , gridUnit : Data.Time.GridUnit
    , chordSpans : List ChordSpan
    , tool : Tool
    , cutGuideTicks : Maybe Int
    , isNarrow : Bool
    , gridTouchAction : String
    , scrollLock : Bool
    }


type alias Waveform =
    { peaks : Array Float
    , peakDt : Float
    , secsPerTick : Float
    , offsetMs : Int
    }


waveHeight : Int
waveHeight =
    44


{-| ベロシティレーンの高さ(px)。Main.elm 側のドラッグ量→velocity換算にも使う。
-}
velocityLaneHeight : Int
velocityLaneHeight =
    60


{-| ノート1行の高さ(px)。狭画面（isNarrow）時はタップしやすさのため拡大する。
-}
rowHeight : Bool -> Int
rowHeight isNarrow =
    if isNarrow then
        20

    else
        14


{-| ズーム機構導入前のデフォルト値。`pianoRollZoom` の初期値に使う。
-}
defaultPxPerSixteenth : Int
defaultPxPerSixteenth =
    20


minPxPerSixteenth : Int
minPxPerSixteenth =
    6


maxPxPerSixteenth : Int
maxPxPerSixteenth =
    120


{-| ルーラーのホイールズームの1ステップ。範囲は `minPxPerSixteenth`〜`maxPxPerSixteenth` にクランプする。
実装は `View.Zoom.step` に委譲。
-}
zoomStep : Float -> Int -> Int
zoomStep deltaY current =
    View.Zoom.step { min = minPxPerSixteenth, max = maxPxPerSixteenth } deltaY current


minPitch : Int
minPitch =
    12


maxPitch : Int
maxPitch =
    84


rulerHeight : Int
rulerHeight =
    36


{-| プレイヘッド追従スクロールで Browser.Dom から参照するスクロールコンテナの id。
-}
pianoRollScrollId : String
pianoRollScrollId =
    "piano-roll-scroll"


{-| 左固定列（鍵盤列 / ラベル列）の幅(px)。可視範囲の計算ではこの分を clientWidth から引く必要がある。
-}
keyColumnWidth : Int
keyColumnWidth =
    44


barWidth : Int -> Int
barWidth pxPerSixteenth =
    16 * pxPerSixteenth


pixelsToTicks : Int -> Float -> Int
pixelsToTicks pxPerSixteenth px =
    round (px * toFloat Data.Time.ticksPerSixteenth / toFloat pxPerSixteenth)


ticksToPixels : Int -> Int -> Float
ticksToPixels pxPerSixteenth ticks =
    toFloat ticks * toFloat pxPerSixteenth / toFloat Data.Time.ticksPerSixteenth


{-| スクロールコンテナの scroll イベントから scrollLeft・scrollTop・clientWidth を取り出して msg に変換するデコーダー。
-}
scrollDecoder : ({ scrollLeft : Float, scrollTop : Float, clientWidth : Float } -> msg) -> Decode.Decoder msg
scrollDecoder toMsg =
    Decode.map3 (\l t w -> toMsg { scrollLeft = l, scrollTop = t, clientWidth = w })
        (Decode.at [ "target", "scrollLeft" ] Decode.float)
        (Decode.at [ "target", "scrollTop" ] Decode.float)
        (Decode.at [ "target", "clientWidth" ] Decode.float)


{-| 現在のスクロール位置と表示幅から、可視な ticks 範囲を求める。リージョン上の可視範囲枠の計算に使う。
-}
visibleTickRange : Int -> { scrollX : Float, width : Float } -> { startTicks : Int, endTicks : Int }
visibleTickRange pxPerSixteenth vp =
    { startTicks = pixelsToTicks pxPerSixteenth vp.scrollX
    , endTicks = pixelsToTicks pxPerSixteenth (vp.scrollX + vp.width)
    }


yToPitch : Bool -> Float -> Int
yToPitch isNarrow y =
    maxPitch - floor (y / toFloat (rowHeight isNarrow))


gridWidth : Int -> Int -> Int
gridWidth pxPerSixteenth totalBars =
    totalBars * barWidth pxPerSixteenth


gridHeight : Bool -> Int
gridHeight isNarrow =
    (maxPitch - minPitch + 1) * rowHeight isNarrow


{-| ピアノロール枠の高さ計算に必要な「どの帯があるか」の情報。view と Main.elm の初期センタリング計算の
両方が同じ値を参照できるようにするのが目的（二重管理を避ける）。
-}
type alias Dims =
    { isNarrow : Bool
    , hasChords : Bool
    , hasWave : Bool
    , hasVelocityLane : Bool
    }


{-| ヘッダ帯（ルーラー + コード帯 + 波形）の高さ(px)。
-}
headerHeight : Dims -> Int
headerHeight dims =
    rulerHeight
        + (if dims.hasChords then
            ChordStrip.height

           else
            0
          )
        + (if dims.hasWave then
            waveHeight

           else
            0
          )


{-| ピアノロール枠の内容全体（ヘッダ + グリッド + Velレーン）の高さ(px)。枠の max-height に使う。
-}
frameContentHeight : Dims -> Int
frameContentHeight dims =
    headerHeight dims
        + gridHeight dims.isNarrow
        + (if dims.hasVelocityLane then
            velocityLaneHeight

           else
            0
          )


{-| ノート列の中央値の音高。空なら C4(60)。ピアノロールの初期センタリング対象を決めるのに使う
（src/Main.elm の initialCenterPitch）。偶数個のときは上寄りの中央（ソート後 n//2 番目）を採る。
-}
medianPitch : List Int -> Int
medianPitch pitches =
    let
        sorted =
            List.sort pitches

        n =
            List.length sorted
    in
    (if n == 0 then
        60

     else
        List.drop (n // 2) sorted |> List.head |> Maybe.withDefault 60
    )
        |> clamp minPitch maxPitch


{-| 指定の音高の行が、ヘッダ帯と Vel レーンを除いた「見えているグリッド領域」の中央に来る scrollTop。
起動時の初期センタリングで使う（src/Main.elm の centerPianoRollCmd）。
-}
centerScrollTop : Dims -> Float -> Int -> Float
centerScrollTop dims viewportHeight pitch =
    let
        rowH =
            toFloat (rowHeight dims.isNarrow)

        rowTop =
            toFloat (maxPitch - clamp minPitch maxPitch pitch) * rowH

        velH =
            if dims.hasVelocityLane then
                toFloat velocityLaneHeight

            else
                0

        gridVpH =
            Basics.max rowH (viewportHeight - toFloat (headerHeight dims) - velH)
    in
    Basics.max 0 (rowTop + rowH / 2 - gridVpH / 2)


type alias FrameOpts msg =
    { scrollId : String
    , ariaLabel : String
    , scrolled : { scrollLeft : Float, scrollTop : Float, clientWidth : Float } -> msg
    , leftWidth : Int
    , contentWidth : Int
    , maxHeight : Maybe Int
    , corner : Html msg
    , header : List (Html msg)
    , leftRows : List (Html msg)
    , body : Html msg
    , footer : Maybe { left : Html msg, cell : Html msg }
    }


pxStr : Int -> String
pxStr n =
    String.fromInt n ++ "px"


{-| 鍵盤列/ラベル列を sticky-left、ルーラー/コード帯/波形を sticky-top で固定した専用の縦スクロール枠。
ピアノロール本体と、コード進行トラックの鍵盤列あり表示で共有する。
sticky を持つのは全部 Html.div（SVG には効かせない）。corner は親のヘッダ行が既に sticky top なので left だけでよい。
-}
scrollFrame : FrameOpts msg -> Html msg
scrollFrame f =
    Html.div
        [ HA.id f.scrollId
        -- pr-frame の min-height は CSS 側（Theme.cssRules）で定義。インラインだと
        -- .pr-col > .pr-fill の min-height: 0 に勝ってしまい、鍵盤パネル等を開いた時に外側ペインへ溢れる。
        , HA.class "pr-fill pr-frame"
        , HA.style "overflow" "auto"
        -- iOS のスクロールチェーン（この枠が端に達したときに外側ペインへスクロールが伝播する現象）を抑える。
        , HA.style "overscroll-behavior" "contain"
        , HA.style "border" ("1px solid " ++ Theme.outlineVariant)
        , HA.style "box-sizing" "border-box"
        , HA.style "margin-top" "1rem"
        , HA.style "background" Theme.surface
        , HA.style "max-height"
            (case f.maxHeight of
                Just h ->
                    pxStr h

                Nothing ->
                    "none"
            )
        , HA.tabindex 0
        , HA.attribute "aria-label" f.ariaLabel
        , Html.Events.on "scroll" (scrollDecoder f.scrolled)
        ]
        [ Html.div
            [ HA.style "width" (pxStr f.contentWidth)
            , HA.style "position" "relative"
            ]
            ([ Html.div
                [ HA.style "display" "flex"
                , HA.style "position" "sticky"
                , HA.style "top" "0"
                , HA.style "z-index" "3"
                , HA.style "background" Theme.surfaceContainer
                ]
                [ Html.div
                    [ HA.style "flex" ("0 0 " ++ pxStr f.leftWidth)
                    , HA.style "position" "sticky"
                    , HA.style "left" "0"
                    , HA.style "z-index" "1"
                    , HA.style "background" Theme.surfaceContainer
                    , HA.style "border-right" ("1px solid " ++ Theme.outline)
                    , HA.style "border-bottom" ("1px solid " ++ Theme.outlineVariant)
                    , HA.style "box-sizing" "border-box"
                    ]
                    [ f.corner ]
                , Html.div [ HA.style "flex" "0 0 auto" ] f.header
                ]
             , Html.div [ HA.style "display" "flex" ]
                [ Html.div
                    [ HA.style "flex" ("0 0 " ++ pxStr f.leftWidth)
                    , HA.style "position" "sticky"
                    , HA.style "left" "0"
                    , HA.style "z-index" "2"
                    , HA.style "background" Theme.surfaceContainerLow
                    , HA.style "border-right" ("1px solid " ++ Theme.outline)
                    , HA.style "box-sizing" "border-box"
                    ]
                    f.leftRows
                , Html.div [ HA.style "flex" "0 0 auto" ] [ f.body ]
                ]
             ]
                ++ (case f.footer of
                        Nothing ->
                            []

                        Just ft ->
                            [ Html.div
                                [ HA.style "display" "flex"
                                , HA.style "position" "sticky"
                                , HA.style "bottom" "0"
                                , HA.style "z-index" "3"
                                , HA.style "background" Theme.surfaceContainerLow
                                ]
                                [ Html.div
                                    [ HA.style "flex" ("0 0 " ++ pxStr f.leftWidth)
                                    , HA.style "position" "sticky"
                                    , HA.style "left" "0"
                                    , HA.style "z-index" "1"
                                    , HA.style "background" Theme.surfaceContainerLow
                                    , HA.style "border-right" ("1px solid " ++ Theme.outline)
                                    , HA.style "box-sizing" "border-box"
                                    ]
                                    [ ft.left ]
                                , Html.div [ HA.style "flex" "0 0 auto" ] [ ft.cell ]
                                ]
                            ]
                   )
            )
        ]


view : Config msg -> ViewOpts -> Html msg
view config opts =
    let
        dims =
            { isNarrow = opts.isNarrow, hasChords = not (List.isEmpty opts.chordSpans), hasWave = opts.waveform /= Nothing, hasVelocityLane = True }
    in
    scrollFrame
        { scrollId = pianoRollScrollId
        , ariaLabel = "ピアノロール（矢印キーでノートを選択・移動）"
        , scrolled = config.scrolled
        , leftWidth = keyColumnWidth
        , contentWidth = keyColumnWidth + gridWidth opts.pxPerSixteenth opts.totalBars
        , maxHeight = Just (frameContentHeight dims)
        , corner = Html.text ""
        , header = [ rulerView config opts, chordStripView config opts, waveformView opts ]
        , leftRows =
            List.range minPitch maxPitch
                |> List.reverse
                |> List.map (keyRow config opts.highlightedPitch opts.scalePitchClasses opts.isNarrow)
        , body = gridView config opts
        , footer = Just { left = velLabelCell, cell = velocityLaneView config opts }
        }


{-| 共通の View.ChordStrip を、このモジュールの坐標系（pxPerSixteenth ベースの線形変換）で呼び出すラッパー。
-}
chordStripView : Config msg -> ViewOpts -> Html msg
chordStripView config opts =
    chordStripViewWithHeight config opts ChordStrip.height


chordStripViewWithHeight : Config msg -> ViewOpts -> Int -> Html msg
chordStripViewWithHeight config opts stripHeight =
    ChordStrip.view
        { clickedChord = config.clickedChord, doubleClickedChord = Just config.doubleClickedChord }
        { ticksToX = ticksToPixels opts.pxPerSixteenth
        , width = gridWidth opts.pxPerSixteenth opts.totalBars
        , playheadTicks = opts.playheadTicks
        , height = stripHeight
        }
        opts.chordSpans


type alias ChordLaneOpts msg =
    { config : View.ChordLane.Config msg
    , tokenSpans : List TokenSpan
    , selectedKeys : Set TokenKey
    , rubberBand : Maybe { x : Float, w : Float }
    , dragActive : Bool
    }


{-| トラック一覧で「コード進行」行を選択した時のメインペイン用ビュー。通常のピアノロールと違い鍵盤列・波形・ノートグリッドは含めず、
ルーラー（小節番号・セクション帯・レープ帯・プレイヘッド）と、トークン単位で選択・ドラッグできる
`View.ChordLane` を並べて横一直線に伸ばす。`pianoRollScrollId` を再利用することで、通常のピアノロールと同時に
マウントされない前提で、追従スクロール・ホイールズーム・ループ編集がそのまま動く。
-}
chordTrackView : Config msg -> ViewOpts -> Maybe (List Note) -> ChordLaneOpts msg -> Html msg
chordTrackView config opts previewNotes chordLane =
    let
        laneHtml height =
            View.ChordLane.view chordLane.config
                { ticksToX = ticksToPixels opts.pxPerSixteenth
                , width = gridWidth opts.pxPerSixteenth opts.totalBars
                , height = height
                , playheadTicks = opts.playheadTicks
                , rubberBand = chordLane.rubberBand
                , selectedKeys = chordLane.selectedKeys
                , dragActive = chordLane.dragActive
                }
                chordLane.tokenSpans
    in
    case previewNotes of
        Nothing ->
            Html.div
                [ HA.style "margin-top" "1rem"
                , HA.style "border" ("1px solid " ++ Theme.outlineVariant)
                ]
                [ Html.div
                    [ HA.id pianoRollScrollId
                    , HA.style "overflow-x" "auto"
                    , HA.tabindex 0
                    , HA.attribute "aria-label" "コード進行トラック"
                    , Html.Events.on "scroll" (scrollDecoder config.scrolled)
                    ]
                    [ rulerView config opts
                    , laneHtml 36
                    ]
                ]

        Just notes ->
            let
                dims =
                    { isNarrow = opts.isNarrow, hasChords = True, hasWave = opts.waveform /= Nothing, hasVelocityLane = True }
            in
            scrollFrame
                { scrollId = pianoRollScrollId
                , ariaLabel = "コード進行トラック"
                , scrolled = config.scrolled
                , leftWidth = keyColumnWidth
                , contentWidth = keyColumnWidth + gridWidth opts.pxPerSixteenth opts.totalBars
                , maxHeight = Just (frameContentHeight dims)
                , corner = Html.text ""
                , header = [ rulerView config opts, laneHtml ChordStrip.height, waveformView opts ]
                , leftRows =
                    List.range minPitch maxPitch
                        |> List.reverse
                        |> List.map (keyRow config opts.highlightedPitch opts.scalePitchClasses opts.isNarrow)
                , body = chordTrackNoteGrid config opts notes
                , footer =
                    Just
                        { left = Html.text ""
                        , cell =
                            velocityLaneViewReadOnly
                                { pxPerSixteenth = opts.pxPerSixteenth
                                , totalBars = opts.totalBars
                                , notes = notes
                                , selectedIds = Set.empty
                                , playheadTicks = opts.playheadTicks
                                }
                        }
                }


{-| コード進行トラックのMIDIプレビュー用の読み取り専用ノートグリッド。通常の `gridView` と違い
mousedown/click 系のハンドラを一切持たず、ノートの新規作成・選択・ドラッグは一切できない（`hoverableGhostNoteView` を
流用しており、ホバーのみ受け取る）。
-}
chordTrackNoteGrid : Config msg -> ViewOpts -> List Note -> Html msg
chordTrackNoteGrid config opts previewNotesList =
    Svg.svg
        [ SA.width (String.fromInt (gridWidth opts.pxPerSixteenth opts.totalBars))
        , SA.height (String.fromInt (gridHeight opts.isNarrow))
        , SA.viewBox ("0 0 " ++ String.fromInt (gridWidth opts.pxPerSixteenth opts.totalBars) ++ " " ++ String.fromInt (gridHeight opts.isNarrow))
        , HA.style "display" "block"
        ]
        (rowBackgrounds opts.isNarrow opts.pxPerSixteenth opts.totalBars
            ++ List.concat (List.indexedMap (sectionTint opts.isNarrow opts.pxPerSixteenth) opts.sections)
            ++ verticalLines opts.isNarrow opts.gridUnit opts.pxPerSixteenth opts.totalBars
            ++ List.map (hoverableGhostNoteView config opts.isNarrow opts.pxPerSixteenth) previewNotesList
            ++ loopLinesView opts.pxPerSixteenth (gridHeight opts.isNarrow) opts.loop
            ++ [ playheadLine opts.pxPerSixteenth (gridHeight opts.isNarrow) opts.playheadTicks ]
        )


waveformView : ViewOpts -> Html msg
waveformView opts =
    case opts.waveform of
        Nothing ->
            Html.text ""

        Just w ->
            Svg.Lazy.lazy6 waveStrip w.peaks w.peakDt w.secsPerTick w.offsetMs opts.totalBars opts.pxPerSixteenth


{-| 参考オーディオの波形帯。BPM とオフセットに合わせてタイムライン上に描く。
-}
waveStrip : Array Float -> Float -> Float -> Int -> Int -> Int -> Svg.Svg msg
waveStrip peaks peakDt secsPerTick offsetMs totalBars pxPerSixteenth =
    let
        width =
            gridWidth pxPerSixteenth totalBars

        colStep =
            3

        mid =
            toFloat waveHeight / 2

        ampAt x =
            let
                sec =
                    toFloat (pixelsToTicks pxPerSixteenth (toFloat x)) * secsPerTick + toFloat offsetMs / 1000
            in
            if sec < 0 then
                0

            else
                Array.get (floor (sec / peakDt)) peaks |> Maybe.withDefault 0

        seg i =
            let
                x =
                    i * colStep

                h =
                    Basics.max 0.5 (ampAt x * (mid - 2))
            in
            "M" ++ String.fromInt x ++ " " ++ String.fromFloat (mid - h) ++ "L" ++ String.fromInt x ++ " " ++ String.fromFloat (mid + h)

        d =
            List.range 0 (width // colStep)
                |> List.map seg
                |> String.join ""

        barLines =
            List.range 0 totalBars
                |> List.map
                    (\i ->
                        Svg.line
                            [ SA.x1 (String.fromInt (i * barWidth pxPerSixteenth))
                            , SA.y1 "0"
                            , SA.x2 (String.fromInt (i * barWidth pxPerSixteenth))
                            , SA.y2 (String.fromInt waveHeight)
                            , SA.stroke Theme.outlineVariant
                            ]
                            []
                    )
    in
    Svg.svg
        [ SA.width (String.fromInt width)
        , SA.height (String.fromInt waveHeight)
        , SA.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt waveHeight)
        , HA.style "display" "block"
        , HA.style "background" Theme.surfaceContainerLow
        , HA.style "border-bottom" ("1px solid " ++ Theme.outlineVariant)
        ]
        (barLines
            ++ [ Svg.path [ SA.d d, SA.stroke (Theme.withAlpha 0.55 Theme.primary), SA.strokeWidth "2" ] [] ]
        )


{-| 左下の Vel ラベルセル。scrollFrame の footer.left に渡す。
-}
velLabelCell : Html msg
velLabelCell =
    Html.div
        [ HA.style "height" (String.fromInt velocityLaneHeight ++ "px")
        , HA.style "box-sizing" "border-box"
        , HA.style "font-size" "9px"
        , HA.style "color" Theme.onSurfaceVariant
        , HA.style "padding" "2px 3px"
        , HA.style "text-align" "right"
        , HA.title "ベロシティ（音の強さ）。バーを上下ドラッグで変えられます"
        ]
        [ Html.text "Vel" ]


keyRow : Config msg -> Set Int -> Set Int -> Bool -> Int -> Html msg
keyRow config highlightedPitch scalePitchClasses isNarrow pitch =
    let
        isBlack =
            List.member (modBy 12 pitch) [ 1, 3, 6, 8, 10 ]

        isHighlighted =
            Set.member pitch highlightedPitch

        inScale =
            Set.member (modBy 12 pitch) scalePitchClasses

        label =
            if modBy 12 pitch == 0 then
                "C" ++ String.fromInt (pitch // 12 - 1)

            else
                ""
    in
    Html.div
        [ HA.style "height" (String.fromInt (rowHeight isNarrow) ++ "px")
        , HA.style "box-sizing" "border-box"
        , HA.style "position" "relative"
        , HA.style "background"
            (if isHighlighted then
                Style.colorHighlight

             else if isBlack then
                Theme.keyBlack

             else
                Theme.keyWhite
            )
        , HA.style "color"
            (if isHighlighted then
                Theme.keyBlack

             else if isBlack then
                Theme.keyWhite

             else
                Theme.keyBlack
            )
        , HA.style "border-bottom" ("1px solid " ++ Theme.outlineVariant)
        , HA.style "font-size" "9px"
        , HA.style "line-height" (String.fromInt (rowHeight isNarrow - 1) ++ "px")
        , HA.style "text-align" "right"
        , HA.style "padding-right" "3px"
        , HA.style "cursor" "pointer"
        , HA.style "user-select" "none"
        , HA.style "touch-action" "none"
        , Html.Events.on "pointerdown" (Decode.succeed (config.pressedKey pitch))
        ]
        (Html.text label
            :: (if inScale then
                    [ Html.div
                        [ HA.style "position" "absolute"
                        , HA.style "left" "3px"
                        , HA.style "top" "50%"
                        , HA.style "transform" "translateY(-50%)"
                        , HA.style "width" "6px"
                        , HA.style "height" "6px"
                        , HA.style "border-radius" "50%"
                        , HA.style "background" Theme.primary
                        , HA.style "pointer-events" "none"
                        ]
                        []
                    ]

                else
                    []
               )
        )


type alias RulerHandlers msg =
    { pressedRuler : { offsetX : Float, clientX : Float, shift : Bool, isTouch : Bool } -> msg
    , pressedLoopHandle : Bool -> Float -> msg
    , wheelZoomedRuler : { deltaY : Float, offsetX : Float } -> msg
    }


type alias RulerOpts =
    { pxPerSixteenth : Int
    , totalBars : Int
    , sections : List SectionSpan
    , loop : Maybe { startTicks : Int, endTicks : Int }
    , loopEditable : Bool
    , playheadTicks : Int
    }


{-| ルーラー（小節番号・セクション帯・ループ帯・プレイヘッド）を描画する。PianoRoll の Config 全体ではなく、
ルーラーが実際に使うハンドラだけに narrow 化してあるので、DrumEditor 等他モジュールからも直接呼べる。
-}
rulerViewWith : Bool -> RulerHandlers msg -> RulerOpts -> Html msg
rulerViewWith isNarrow handlers opts =
    Svg.svg
        [ SA.width (String.fromInt (gridWidth opts.pxPerSixteenth opts.totalBars))
        , SA.height (String.fromInt rulerHeight)
        , SA.viewBox ("0 0 " ++ String.fromInt (gridWidth opts.pxPerSixteenth opts.totalBars) ++ " " ++ String.fromInt rulerHeight)
        , HA.style "display" "block"
        , HA.style "cursor" "pointer"
        , HA.style "touch-action" "none"
        , HA.title "クリックで再生位置を移動。shift + ドラッグでループ区間を作成。マウスホイールでズーム"
        -- タッチはpointerdownした要素に暗黙キャプチャされ、ドラッグ中のpointermove/pointerupが
        -- 全画面オーバーレイ（Main.elm の viewDragOverlay）に届かなくなる。ここを解除して、
        -- shift+ドラッグのループ作成やタップシークがタッチでも ReleasedDrag まで届くようにする。
        , HA.attribute "data-pointer-release-capture" ""
        , Html.Events.on "pointerdown" (Decode.map handlers.pressedRuler rulerPressDecoder)
        , Html.Events.preventDefaultOn "wheel" (Decode.map (\w -> ( handlers.wheelZoomedRuler w, True )) wheelDecoder)
        ]
        (Svg.rect
            [ SA.x "0"
            , SA.y "0"
            , SA.width (String.fromInt (gridWidth opts.pxPerSixteenth opts.totalBars))
            , SA.height (String.fromInt rulerHeight)
            , SA.fill Theme.surfaceContainer
            ]
            []
            :: List.concat (List.indexedMap (sectionBand opts.pxPerSixteenth) opts.sections)
            ++ barNumbers opts.pxPerSixteenth opts.totalBars
            ++ loopBandView isNarrow handlers.pressedLoopHandle opts.pxPerSixteenth opts.loopEditable opts.loop
            ++ [ playheadLine opts.pxPerSixteenth rulerHeight opts.playheadTicks ]
        )


rulerView : Config msg -> ViewOpts -> Html msg
rulerView config opts =
    rulerViewWith
        opts.isNarrow
        { pressedRuler = config.pressedRuler
        , pressedLoopHandle = config.pressedLoopHandle
        , wheelZoomedRuler = config.wheelZoomedRuler
        }
        { pxPerSixteenth = opts.pxPerSixteenth
        , totalBars = opts.totalBars
        , sections = opts.sections
        , loop = opts.loop
        , loopEditable = opts.loopEditable
        , playheadTicks = opts.playheadTicks
        }


{-| button フィルタを入れ、右クリックでは発火させない。入れないと右クリックでも pressedRuler が呼ばれ seek/loop ドラッグが始まり、
contextmenu 発火後にボタンを離しても状態がホールドされる。
-}
rulerPressDecoder : Decode.Decoder { offsetX : Float, clientX : Float, shift : Bool, isTouch : Bool }
rulerPressDecoder =
    Decode.field "button" Decode.int
        |> Decode.andThen
            (\button ->
                if button == 0 then
                    Decode.map4 (\ox cx sh touch -> { offsetX = ox, clientX = cx, shift = sh, isTouch = touch })
                        (Decode.field "offsetX" Decode.float)
                        (Decode.field "clientX" Decode.float)
                        (Decode.field "shiftKey" Decode.bool)
                        (Decode.field "pointerType" Decode.string |> Decode.map ((==) "touch"))

                else
                    Decode.fail "not left button"
            )


wheelDecoder : Decode.Decoder { deltaY : Float, offsetX : Float }
wheelDecoder =
    Decode.map2 (\dy ox -> { deltaY = dy, offsetX = ox })
        (Decode.field "deltaY" Decode.float)
        (Decode.field "offsetX" Decode.float)


{-| ルーラー上にループ区間を琥珀色の帯で示す。editable が真なら左右端につまむハンドルを追加で描く（「範囲」モードのループのみ）。
-}
loopBandView : Bool -> (Bool -> Float -> msg) -> Int -> Bool -> Maybe { startTicks : Int, endTicks : Int } -> List (Svg.Svg msg)
loopBandView isNarrow pressedLoopHandle pxPerSixteenth editable loop =
    case loop of
        Nothing ->
            []

        Just l ->
            let
                x0 =
                    ticksToPixels pxPerSixteenth l.startTicks

                x1 =
                    ticksToPixels pxPerSixteenth l.endTicks

                band =
                    Svg.rect
                        [ SA.x (String.fromFloat x0)
                        , SA.y "16"
                        , SA.width (String.fromFloat (Basics.max 0 (x1 - x0)))
                        , SA.height "6"
                        , SA.fill Style.colorLoop
                        , SA.pointerEvents "none"
                        ]
                        []
            in
            band
                :: (if editable then
                        [ loopHandle isNarrow pressedLoopHandle False x0
                        , loopHandle isNarrow pressedLoopHandle True x1
                        ]

                    else
                        []
                   )


{-| ループ帯の端をつまんで伸縮するためのハンドル。バンド本体（6pxx6px）より少し大きめにしてつかみやすくする。
-}
loopHandle : Bool -> (Bool -> Float -> msg) -> Bool -> Float -> Svg.Svg msg
loopHandle isNarrow pressedLoopHandle isEnd x =
    let
        handleWidth =
            if isNarrow then
                18.0

            else
                12.0

        handleHeight =
            if isNarrow then
                24.0

            else
                18.0
    in
    Svg.rect
        [ SA.x (String.fromFloat (x - handleWidth / 2))
        , SA.y "10"
        , SA.width (String.fromFloat handleWidth)
        , SA.height (String.fromFloat handleHeight)
        , SA.fill Style.colorLoopHandle
        , SA.cursor "ew-resize"
        , HA.style "touch-action" "none"
        , HA.title "ドラッグでループ範囲を伸縮（ループ:範囲 のときだけ）"
        , Html.Events.stopPropagationOn "pointerdown"
            (Decode.field "button" Decode.int
                |> Decode.andThen
                    (\button ->
                        if button == 0 then
                            Decode.map (\cx -> ( pressedLoopHandle isEnd cx, True )) (Decode.field "clientX" Decode.float)

                        else
                            Decode.fail "not left button"
                    )
            )
        ]
        []


sectionBand : Int -> Int -> SectionSpan -> List (Svg.Svg msg)
sectionBand pxPerSixteenth idx span =
    [ Svg.rect
        [ SA.x (String.fromInt (span.startBar * barWidth pxPerSixteenth))
        , SA.y "0"
        , SA.width (String.fromInt (span.lengthBars * barWidth pxPerSixteenth))
        , SA.height "16"
        , SA.fill (Palette.sectionColor idx)
        , SA.fillOpacity "0.85"
        ]
        []
    , Svg.text_
        [ SA.x (String.fromInt (span.startBar * barWidth pxPerSixteenth + 4))
        , SA.y "12"
        , SA.fontSize "10"
        , SA.fill Theme.surfaceContainerLowest
        , SA.pointerEvents "none"
        ]
        [ Svg.text span.name ]
    ]


barNumbers : Int -> Int -> List (Svg.Svg msg)
barNumbers pxPerSixteenth totalBars =
    List.range 0 (totalBars - 1)
        |> List.concatMap
            (\i ->
                [ Svg.line
                    [ SA.x1 (String.fromInt (i * barWidth pxPerSixteenth))
                    , SA.y1 "16"
                    , SA.x2 (String.fromInt (i * barWidth pxPerSixteenth))
                    , SA.y2 (String.fromInt rulerHeight)
                    , SA.stroke Theme.outlineVariant
                    ]
                    []
                , Svg.text_
                    [ SA.x (String.fromInt (i * barWidth pxPerSixteenth + 4))
                    , SA.y "30"
                    , SA.fontSize "10"
                    , SA.fill Theme.onSurfaceVariant
                    , SA.pointerEvents "none"
                    ]
                    [ Svg.text (String.fromInt (i + 1)) ]
                ]
            )


gridView : Config msg -> ViewOpts -> Html msg
gridView config opts =
    Svg.svg
        ([ SA.width (String.fromInt (gridWidth opts.pxPerSixteenth opts.totalBars))
         , SA.height (String.fromInt (gridHeight opts.isNarrow))
         , SA.viewBox ("0 0 " ++ String.fromInt (gridWidth opts.pxPerSixteenth opts.totalBars) ++ " " ++ String.fromInt (gridHeight opts.isNarrow))
         , HA.style "display" "block"
         , HA.style "cursor"
            (if opts.tool == CutTool then
                "col-resize"

             else if opts.tool == LockTool then
                "default"

             else
                "crosshair"
            )
         , HA.style "touch-action" opts.gridTouchAction
         , Html.Events.on "pointerdown" (Decode.map config.pressedEmpty emptyPressDecoder)
         , HA.attribute "data-pointer-capture" ""
         , Html.Events.on "pointermove"
            (Decode.map config.draggedWhilePressingNote noteMoveDecoder)
         , Html.Events.on "pointerup" (Decode.succeed config.releasedNotePress)
         , Html.Events.on "pointercancel" (Decode.succeed config.canceledNotePress)
         ]
            ++ (if opts.tool == CutTool then
                    [ Html.Events.on "mousemove" (Decode.map config.movedCutGuide cutGuideMoveDecoder)
                    , Html.Events.on "mouseleave" (Decode.succeed config.clearedCutGuide)
                    ]

                else
                    []
               )
            ++ (if opts.scrollLock then
                    -- 長押しで矩形選択・ループドラッグに入った後の、ネイティブスクロール抑止フラグ。
                    -- touch-action は pointerdown 時点で確定してしまうので途中変更では効かない。
                    -- js/main.js の非パッシブ touchmove リスナーがこの属性を closest() で拾って
                    -- preventDefault する（ノート rect 上のタッチも含めて拾えるよう、グリッド側に1箇所だけ付与する）。
                    [ HA.attribute "data-suppress-touch-scroll" "" ]

                else
                    []
               )
        )
        (rowBackgrounds opts.isNarrow opts.pxPerSixteenth opts.totalBars
            ++ List.concat (List.indexedMap (sectionTint opts.isNarrow opts.pxPerSixteenth) opts.sections)
            ++ verticalLines opts.isNarrow opts.gridUnit opts.pxPerSixteenth opts.totalBars
            ++ List.concat (List.indexedMap (\idx notes -> List.map (ghostNoteView opts.isNarrow opts.pxPerSixteenth idx) notes) opts.ghostNoteGroups)
            ++ List.concatMap (noteView opts.isNarrow config opts.pxPerSixteenth opts.selectedIds opts.tool) opts.notes
            ++ rubberBandView opts.rubberBand
            ++ loopLinesView opts.pxPerSixteenth (gridHeight opts.isNarrow) opts.loop
            ++ cutGuideView opts.cutGuideTicks (gridHeight opts.isNarrow) opts.pxPerSixteenth
            ++ [ playheadLine opts.pxPerSixteenth (gridHeight opts.isNarrow) opts.playheadTicks ]
        )


cutGuideMoveDecoder : Decode.Decoder { offsetX : Float }
cutGuideMoveDecoder =
    Decode.map (\ox -> { offsetX = ox }) (Decode.field "offsetX" Decode.float)


{-| カットツール選択中、マウス位置（スナップ済み tick）に追従する薄い縦ガイド線。rubberBandView と同じく
pointerEvents "none" の破線で、クリック判定には一切関与しない（あくまで視覚フィードバック）。
-}
cutGuideView : Maybe Int -> Int -> Int -> List (Svg.Svg msg)
cutGuideView cutGuideTicks height pxPerSixteenth =
    case cutGuideTicks of
        Nothing ->
            []

        Just ticks ->
            [ Svg.line
                [ SA.x1 (String.fromFloat (ticksToPixels pxPerSixteenth ticks))
                , SA.y1 "0"
                , SA.x2 (String.fromFloat (ticksToPixels pxPerSixteenth ticks))
                , SA.y2 (String.fromInt height)
                , SA.stroke (Theme.withAlpha 0.5 Theme.primary)
                , SA.strokeWidth "2"
                , SA.strokeDasharray "4 2"
                , SA.pointerEvents "none"
                ]
                []
            ]


{-| 各ノートの velocity を縦バーで表示・編集するレーン。gridView と同じ横座標系
(ticksToPixels / gridWidth)を共有するのでスクロール同期は自動。Config 全体ではなく
pressedVelocityBar ハンドラだけに narrow 化してあるので、DrumEditor 等他モジュールからも直接呼べる。
-}
velocityLaneViewWith :
    { pressedVelocityBar : Int -> { clientX : Float, clientY : Float } -> msg }
    ->
        { pxPerSixteenth : Int
        , totalBars : Int
        , notes : List Note
        , selectedIds : Set Int
        , playheadTicks : Int
        }
    -> Html msg
velocityLaneViewWith handlers opts =
    let
        width =
            gridWidth opts.pxPerSixteenth opts.totalBars

        -- 選択中バーを上のレイヤーに描く(重なった時につかめるように)
        ( selectedNotes, unselectedNotes ) =
            List.partition (\n -> Set.member n.id opts.selectedIds) opts.notes
    in
    Svg.svg
        [ SA.width (String.fromInt width)
        , SA.height (String.fromInt velocityLaneHeight)
        , SA.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt velocityLaneHeight)
        , HA.style "display" "block"
        , HA.style "background" Theme.surfaceContainerLow
        , HA.style "border-top" ("1px solid " ++ Theme.outlineVariant)
        ]
        (List.map (velocityBarView handlers opts.pxPerSixteenth opts.selectedIds) (unselectedNotes ++ selectedNotes)
            ++ [ playheadLine opts.pxPerSixteenth velocityLaneHeight opts.playheadTicks ]
        )


velocityLaneView : Config msg -> ViewOpts -> Html msg
velocityLaneView config opts =
    if isEditable opts.tool then
        velocityLaneViewWith
            { pressedVelocityBar = config.pressedVelocityBar }
            { pxPerSixteenth = opts.pxPerSixteenth
            , totalBars = opts.totalBars
            , notes = opts.notes
            , selectedIds = opts.selectedIds
            , playheadTicks = opts.playheadTicks
            }

    else
        -- LockTool（や CutTool）では触れないので読み取り専用に落とす。選択強調だけは残す。
        velocityLaneViewReadOnly
            { pxPerSixteenth = opts.pxPerSixteenth
            , totalBars = opts.totalBars
            , notes = opts.notes
            , selectedIds = opts.selectedIds
            , playheadTicks = opts.playheadTicks
            }


velocityBarView : { pressedVelocityBar : Int -> { clientX : Float, clientY : Float } -> msg } -> Int -> Set Int -> Note -> Svg.Svg msg
velocityBarView handlers pxPerSixteenth selectedIds note =
    let
        x =
            ticksToPixels pxPerSixteenth note.start

        barW =
            toFloat (clamp 3 9 (pxPerSixteenth // 2))

        h =
            Basics.max 2 (toFloat note.velocity / 127 * toFloat (velocityLaneHeight - 2))

        selected =
            Set.member note.id selectedIds
    in
    Svg.rect
        [ SA.x (String.fromFloat x)
        , SA.y (String.fromFloat (toFloat velocityLaneHeight - h))
        , SA.width (String.fromFloat barW)
        , SA.height (String.fromFloat h)
        , SA.fill
            (if selected then
                Style.colorSelection

             else
                Theme.primary
            )
        , SA.stroke
            (if selected then
                Theme.selectionDeep

             else
                "none"
            )
        , HA.style "cursor" "ns-resize"
        , HA.style "touch-action" "none"
        , HA.title "上下ドラッグで強さ（velocity）を変える"
        , Html.Events.stopPropagationOn "pointerdown"
            (Decode.map (\pos -> ( handlers.pressedVelocityBar note.id pos, True )) velocityPressDecoder)
        ]
        []


{-| コードトラックやロック中のピアノロール用の読み取り専用ベロシティバー。velocityBarView からイベントハンドラと ns-resize カーソルを除いた版。
selectedIds は選択強調の色付けのためだけに使い、pointerEvents "none" でピアノロール側のマウス操作を妨げない。
-}
velocityBarViewReadOnly : Int -> Set Int -> Note -> Svg.Svg msg
velocityBarViewReadOnly pxPerSixteenth selectedIds note =
    let
        x =
            ticksToPixels pxPerSixteenth note.start

        barW =
            toFloat (clamp 3 9 (pxPerSixteenth // 2))

        h =
            Basics.max 2 (toFloat note.velocity / 127 * toFloat (velocityLaneHeight - 2))

        selected =
            Set.member note.id selectedIds
    in
    Svg.rect
        [ SA.x (String.fromFloat x)
        , SA.y (String.fromFloat (toFloat velocityLaneHeight - h))
        , SA.width (String.fromFloat barW)
        , SA.height (String.fromFloat h)
        , SA.fill
            (if selected then
                Style.colorSelection

             else
                Theme.primary
            )
        , SA.stroke
            (if selected then
                Theme.selectionDeep

             else
                "none"
            )
        , SA.pointerEvents "none"
        ]
        []


{-| velocityLaneViewWith の読み取り専用版。コードトラックのプレビューノート（previewNotes）や、LockTool 中のピアノロールのノートを
並べるだけでハンドラは持たないが、selectedIds の選択強調は保つ。
-}
velocityLaneViewReadOnly :
    { pxPerSixteenth : Int
    , totalBars : Int
    , notes : List Note
    , selectedIds : Set Int
    , playheadTicks : Int
    }
    -> Html msg
velocityLaneViewReadOnly opts =
    let
        width =
            gridWidth opts.pxPerSixteenth opts.totalBars
    in
    Svg.svg
        [ SA.width (String.fromInt width)
        , SA.height (String.fromInt velocityLaneHeight)
        , SA.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt velocityLaneHeight)
        , HA.style "display" "block"
        , HA.style "background" Theme.surfaceContainerLow
        , HA.style "border-top" ("1px solid " ++ Theme.outlineVariant)
        ]
        (List.map (velocityBarViewReadOnly opts.pxPerSixteenth opts.selectedIds) opts.notes
            ++ [ playheadLine opts.pxPerSixteenth velocityLaneHeight opts.playheadTicks ]
        )


{-| button フィルタを入れ、右クリックでは発火させない。入れないと右クリックでも pressedVelocityBar が呼ばれ velocity ドラッグが始まり、
contextmenu 発火後にボタンを離しても状態がホールドされる。
-}
velocityPressDecoder : Decode.Decoder { clientX : Float, clientY : Float }
velocityPressDecoder =
    Decode.field "button" Decode.int
        |> Decode.andThen
            (\button ->
                if button == 0 then
                    Decode.map2 (\cx cy -> { clientX = cx, clientY = cy })
                        (Decode.field "clientX" Decode.float)
                        (Decode.field "clientY" Decode.float)

                else
                    Decode.fail "not left button"
            )


{-| グリッド上にループ区間の開始・終了を縦の破線で示す。面を塗るとスケールガイドや sectionTint と層が重なって見づらくなるので線のみにする。
-}
loopLinesView : Int -> Int -> Maybe { startTicks : Int, endTicks : Int } -> List (Svg.Svg msg)
loopLinesView pxPerSixteenth height loop =
    case loop of
        Nothing ->
            []

        Just l ->
            [ loopLine height (ticksToPixels pxPerSixteenth l.startTicks)
            , loopLine height (ticksToPixels pxPerSixteenth l.endTicks)
            ]


loopLine : Int -> Float -> Svg.Svg msg
loopLine height x =
    Svg.line
        [ SA.x1 (String.fromFloat x)
        , SA.y1 "0"
        , SA.x2 (String.fromFloat x)
        , SA.y2 (String.fromInt height)
        , SA.stroke Theme.highlight
        , SA.strokeWidth "2"
        , SA.strokeDasharray "4 2"
        , SA.pointerEvents "none"
        ]
        []


{-| セクションごとの淡い背景色。height を引数化してあるので、ピアノロール以外（ドラムグリッド等）の高さにも合わせられる。
-}
sectionTintWithHeight : Int -> Int -> Int -> SectionSpan -> List (Svg.Svg msg)
sectionTintWithHeight height pxPerSixteenth idx span =
    [ Svg.rect
        [ SA.x (String.fromInt (span.startBar * barWidth pxPerSixteenth))
        , SA.y "0"
        , SA.width (String.fromInt (span.lengthBars * barWidth pxPerSixteenth))
        , SA.height (String.fromInt height)
        , SA.fill (Palette.sectionColor idx)
        , SA.fillOpacity "0.05"
        , SA.pointerEvents "none"
        ]
        []
    ]


sectionTint : Bool -> Int -> Int -> SectionSpan -> List (Svg.Svg msg)
sectionTint isNarrow pxPerSixteenth idx span =
    sectionTintWithHeight (gridHeight isNarrow) pxPerSixteenth idx span


{-| pointerdown 用デコーダ。button フィルタを必ず入れる: これがないと空セルの右クリック（コンテキストメニューを
出す想定）でも PressedEmptyCell が発火し、「押したまま伸ばして配置する」ためのリサイズ状態が始まってしまう。
右ボタンを離しても contextmenu 発火後はイベント配送が止まるため状態がホールドされ続ける。
-}
emptyPressDecoder : Decode.Decoder { offsetX : Float, offsetY : Float, clientX : Float, clientY : Float, shift : Bool, seekMod : Bool, isTouch : Bool }
emptyPressDecoder =
    Decode.field "button" Decode.int
        |> Decode.andThen
            (\button ->
                if button == 0 then
                    Decode.map2
                        (\r touch ->
                            { offsetX = r.offsetX
                            , offsetY = r.offsetY
                            , clientX = r.clientX
                            , clientY = r.clientY
                            , shift = r.shift
                            , seekMod = r.seekMod
                            , isTouch = touch
                            }
                        )
                        (Decode.map8
                            (\ox oy cx cy sh ctrl meta alt ->
                                { offsetX = ox
                                , offsetY = oy
                                , clientX = cx
                                , clientY = cy
                                , shift = sh
                                , seekMod = ctrl || meta || alt
                                }
                            )
                            (Decode.field "offsetX" Decode.float)
                            (Decode.field "offsetY" Decode.float)
                            (Decode.field "clientX" Decode.float)
                            (Decode.field "clientY" Decode.float)
                            (Decode.field "shiftKey" Decode.bool)
                            (Decode.field "ctrlKey" Decode.bool)
                            (Decode.field "metaKey" Decode.bool)
                            (Decode.field "altKey" Decode.bool)
                        )
                        (Decode.field "pointerType" Decode.string |> Decode.map ((==) "touch"))

                else
                    Decode.fail "not left button"
            )


{-| ノートを押している間のpointermove用。buttons ガードを入れ、ボタンを押していない（ホバーだけの）
pointermoveでは発火しないようにする。これにより、残留したpendingNoteDragが別ノート上のホバーで誤って
Draggingに昇格することが構造的に不可能になる。altKey も同時に読み、Alt押しながらドラッグしてスナップを
無効化する機能に渡す。
-}
noteMoveDecoder : Decode.Decoder { clientX : Float, clientY : Float, alt : Bool }
noteMoveDecoder =
    Decode.field "buttons" Decode.int
        |> Decode.andThen
            (\buttons ->
                if buttons > 0 then
                    Decode.map3 (\cx cy alt -> { clientX = cx, clientY = cy, alt = alt })
                        (Decode.field "clientX" Decode.float)
                        (Decode.field "clientY" Decode.float)
                        (Decode.field "altKey" Decode.bool)

                else
                    Decode.fail "no button pressed"
            )


{-| pointerdown 用デコーダ。button フィルタを必ず入れる: これがないと右クリック（contextmenu で削除する
想定）でも pointerdown が先に発火し、dragState がJustになってviewDragOverlayが画面を覆うため、
後続のcontextmenuイベントがノート要素まで届かず右クリック削除が動かなくなる。
-}
notePressDecoder : Decode.Decoder { clientX : Float, clientY : Float, shift : Bool, isTouch : Bool, timeStamp : Float }
notePressDecoder =
    Decode.field "button" Decode.int
        |> Decode.andThen
            (\button ->
                if button == 0 then
                    Decode.map5 (\cx cy sh touch ts -> { clientX = cx, clientY = cy, shift = sh, isTouch = touch, timeStamp = ts })
                        (Decode.field "clientX" Decode.float)
                        (Decode.field "clientY" Decode.float)
                        (Decode.field "shiftKey" Decode.bool)
                        (Decode.field "pointerType" Decode.string |> Decode.map ((==) "touch"))
                        (Decode.field "timeStamp" Decode.float)

                else
                    Decode.fail "not left button"
            )


{-| ノートホバー時のマウス座標取得用デコーダ。ツールチップの位置決めに使う。
-}
noteHoverDecoder : Decode.Decoder { clientX : Float, clientY : Float }
noteHoverDecoder =
    Decode.map2 (\cx cy -> { clientX = cx, clientY = cy })
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


rowBackgrounds : Bool -> Int -> Int -> List (Svg.Svg msg)
rowBackgrounds isNarrow pxPerSixteenth totalBars =
    List.range minPitch maxPitch
        |> List.map
            (\pitch ->
                let
                    y =
                        (maxPitch - pitch) * rowHeight isNarrow

                    isBlack =
                        List.member (modBy 12 pitch) [ 1, 3, 6, 8, 10 ]
                in
                Svg.rect
                    [ SA.x "0"
                    , SA.y (String.fromInt y)
                    , SA.width (String.fromInt (gridWidth pxPerSixteenth totalBars))
                    , SA.height (String.fromInt (rowHeight isNarrow))
                    , SA.fill
                        (if isBlack then
                            Theme.gridRowBlackKey

                         else
                            Theme.gridRowWhiteKey
                        )
                    , SA.stroke Theme.surfaceContainerHighest
                    , SA.strokeWidth "0.5"
                    ]
                    []
            )


verticalLines : Bool -> Data.Time.GridUnit -> Int -> Int -> List (Svg.Svg msg)
verticalLines isNarrow gridUnit pxPerSixteenth totalBars =
    let
        grid =
            Data.Time.gridTicks gridUnit

        linesPerBar =
            Data.Time.ticksPerBar // grid
    in
    List.range 0 (totalBars * linesPerBar)
        |> List.map
            (\i ->
                let
                    t =
                        i * grid

                    x =
                        ticksToPixels pxPerSixteenth t

                    isBar =
                        modBy Data.Time.ticksPerBar t == 0

                    isBeat =
                        modBy Data.Time.ppq t == 0
                in
                Svg.line
                    [ SA.x1 (String.fromFloat x)
                    , SA.y1 "0"
                    , SA.x2 (String.fromFloat x)
                    , SA.y2 (String.fromInt (gridHeight isNarrow))
                    , SA.stroke
                        (if isBar then
                            Theme.outline

                         else if isBeat then
                            Theme.outlineVariant

                         else
                            Theme.surfaceContainerHighest
                        )
                    , SA.strokeWidth
                        (if isBar then
                            "1.5"

                         else
                            "1"
                        )
                    ]
                    []
            )


{-| 他トラック・コードトラックを透けて重ね表示する用のノート。クリック・ドラッグ不可で、
自分のノート（`noteView`）より下のレイヤーに描画される。idx はグループ（トラック）ごとの色分けに使う。
-}
ghostNoteView : Bool -> Int -> Int -> Note -> Svg.Svg msg
ghostNoteView isNarrow pxPerSixteenth idx note =
    let
        x =
            ticksToPixels pxPerSixteenth note.start

        y =
            (maxPitch - note.pitch) * rowHeight isNarrow

        w =
            ticksToPixels pxPerSixteenth note.duration
    in
    Svg.rect
        [ SA.x (String.fromFloat x)
        , SA.y (String.fromInt (y + 1))
        , SA.width (String.fromFloat (Basics.max 2 (w - 1)))
        , SA.height (String.fromInt (rowHeight isNarrow - 2))
        , SA.rx "2"
        , SA.fill (Palette.sectionColor idx)
        , SA.fillOpacity "0.35"
        , SA.pointerEvents "none"
        ]
        []


{-| ghostNoteView にホバーのみを追加した読み取り専用版。コード進行トラックのMIDIプレビュー（chordTrackNoteGrid）で使う。
ghostNoteView と違い pointerEvents を有効にしてmouseover/mouseoutだけを受け取るが、mousedown等の編集系ハンドラーは一切付けない（編集不可）。
-}
hoverableGhostNoteView : Config msg -> Bool -> Int -> Note -> Svg.Svg msg
hoverableGhostNoteView config isNarrow pxPerSixteenth note =
    let
        x =
            ticksToPixels pxPerSixteenth note.start

        y =
            (maxPitch - note.pitch) * rowHeight isNarrow

        w =
            ticksToPixels pxPerSixteenth note.duration
    in
    Svg.rect
        [ SA.x (String.fromFloat x)
        , SA.y (String.fromInt (y + 1))
        , SA.width (String.fromFloat (Basics.max 2 (w - 1)))
        , SA.height (String.fromInt (rowHeight isNarrow - 2))
        , SA.rx "2"
        , SA.fill Theme.primary
        , SA.fillOpacity "0.55"
        , SA.pointerEvents "visiblePainted"
        , HA.style "cursor" "default"
        , Html.Events.on "mouseover"
            (Decode.map (\pos -> config.hoveredNote note pos.clientX pos.clientY) noteHoverDecoder)
        , Html.Events.on "mouseout" (Decode.succeed config.unhoveredNote)
        ]
        []


{-| ノート本体・左右リサイズハンドルの3要素に共通で付与するpointer系リスナーと属性。

pointerdown後の強制ポインターキャプチャ（js/main.js の data-pointer-capture リスナー）と対になっている:
マウスには Pointer Events の暗黙キャプチャがないため、setPointerCapture を明示呼びしない限り、
ポインターが要素外に出た瞬間 pointermove/pointerup が届かなくなる。data-pointer-capture 属性で
限定することで、他のオーバーレイ方式のドラッグと干渉せずに共存できる。
pointercancelも pointerup と同じ扱いにする（ドラッグ中のスクロールジェスチャ等で中断された場合に状態を残さないため）。

-}
notePointerListeners : Config msg -> List (Html.Attribute msg)
notePointerListeners config =
    [ HA.attribute "data-pointer-capture" ""
    , Html.Events.stopPropagationOn "pointermove"
        (Decode.map (\pos -> ( config.draggedWhilePressingNote pos, True )) noteMoveDecoder)
    , Html.Events.stopPropagationOn "pointerup"
        (Decode.succeed ( config.releasedNotePress, True ))
    , Html.Events.stopPropagationOn "pointercancel"
        (Decode.succeed ( config.releasedNotePress, True ))
    ]


{-| ノート矩形の左上のグリッド座標。noteView の x/y 計算と同じ式だが、Main 側が長押しでの
矩形選択の原点として外から使えるよう公開関数として切り出してある。
-}
noteTopLeft : Bool -> Int -> { r | pitch : Int, start : Int } -> { x : Float, y : Float }
noteTopLeft isNarrow pxPerSixteenth note =
    { x = ticksToPixels pxPerSixteenth note.start
    , y = toFloat ((maxPitch - note.pitch) * rowHeight isNarrow)
    }


noteView : Bool -> Config msg -> Int -> Set Int -> Tool -> Note -> List (Svg.Svg msg)
noteView isNarrow config pxPerSixteenth selectedIds tool note =
    let
        x =
            ticksToPixels pxPerSixteenth note.start

        y =
            (maxPitch - note.pitch) * rowHeight isNarrow

        w =
            ticksToPixels pxPerSixteenth note.duration

        handleWidth =
            if isNarrow then
                12.0

            else
                6.0

        minFullHandleWidth =
            18.0

        selected =
            Set.member note.id selectedIds

        editable =
            isEditable tool

        selectable =
            isSelectable tool

        {- ノート本体の pointer-events。選択できない（CutTool）ときだけ透過させる。 -}
        bodyPointerAttrs =
            if selectable then
                []

            else
                [ SA.pointerEvents "none" ]

        {- リサイズハンドルの pointer-events。編集不可（LockTool/CutTool）なら透過させ、
           下のノート本体やグリッドへタップ・スワイプが抜けるようにする。
        -}
        handlePointerAttrs =
            if editable then
                []

            else
                [ SA.pointerEvents "none" ]
    in
    [ Svg.rect
        ([ SA.x (String.fromFloat x)
         , SA.y (String.fromInt (y + 1))
         , SA.width (String.fromFloat (Basics.max 2 (w - 1)))
         , SA.height (String.fromInt (rowHeight isNarrow - 2))
         , SA.rx "2"
         , SA.fill
            (if selected then
                Style.colorSelection

             else
                Theme.noteFill
            )
         , SA.stroke
            (if selected then
                Theme.selectionDeep

             else
                Theme.primary
            )
         , SA.strokeWidth "1"
         , SA.shapeRendering "crispEdges"
         , HA.style "cursor"
            (if editable then
                "move"

             else if selectable then
                "pointer"

             else
                "inherit"
            )
         , HA.style "touch-action"
            (if editable then
                "none"

             else if tool == LockTool then
                -- ロック中はノート上のスワイプもスクロールになるべきなので pan-x pan-y のままにする。
                "pan-x pan-y"

             else
                "auto"
            )
         ]
            ++ bodyPointerAttrs
            ++ (if selectable then
                    [ Html.Events.stopPropagationOn "pointerdown"
                        (Decode.map (\pos -> ( config.pressedNote note.id NoResize pos, True )) notePressDecoder)
                    ]
                        ++ (if editable then
                                notePointerListeners config
                                    ++ [ Html.Events.stopPropagationOn "dblclick"
                                            (Decode.succeed ( config.doubleClickedNote note.id, True ))
                                       , Html.Events.preventDefaultOn "contextmenu"
                                            (Decode.succeed ( config.rightClickedNote note.id, True ))
                                       , Html.Events.on "mouseover"
                                            (Decode.map (\pos -> config.hoveredNote note pos.clientX pos.clientY) noteHoverDecoder)
                                       , Html.Events.on "mouseout" (Decode.succeed config.unhoveredNote)
                                       ]

                            else
                                []
                           )

                else
                    []
               )
        )
        []
    , Svg.rect
        ([ SA.x
            (String.fromFloat
                (if w >= 3 * minFullHandleWidth then
                    x + w - 1 - 1.5 - handleWidth

                 else
                    x + w - handleWidth
                )
            )
         , SA.y
            (String.fromFloat
                (if w >= 3 * minFullHandleWidth then
                    toFloat y + 2.5

                 else
                    toFloat y + 1
                )
            )
         , SA.width (String.fromFloat handleWidth)
         , SA.height
            (String.fromFloat
                (if w >= 3 * minFullHandleWidth then
                    toFloat (rowHeight isNarrow) - 5

                 else
                    toFloat (rowHeight isNarrow) - 2
                )
            )
         , SA.rx "1"
         , SA.fill
            (if w >= 3 * minFullHandleWidth then
                if selected then
                    Theme.selectionDeep

                else
                    Theme.primary

             else
                "transparent"
            )
         , HA.style "cursor"
            (if editable then
                "ew-resize"

             else
                "inherit"
            )
         , HA.style "touch-action"
            (if editable then
                "none"

             else
                "auto"
            )
         , HA.title
            (if editable then
                "ドラッグで長さを変える"

             else
                ""
            )
         ]
            ++ handlePointerAttrs
            ++ (if editable then
                    [ Html.Events.stopPropagationOn "pointerdown"
                        (Decode.map (\pos -> ( config.pressedNote note.id ResizeRight pos, True )) notePressDecoder)
                    ]
                        ++ notePointerListeners config
                        ++ [ Html.Events.preventDefaultOn "contextmenu"
                                (Decode.succeed ( config.rightClickedNote note.id, True ))
                           ]

                else
                    []
               )
        )
        []
    ]
        ++ (if w >= 3 * minFullHandleWidth then
                [ Svg.rect
                    ([ SA.x (String.fromFloat x)
                     , SA.y (String.fromInt (y + 1))
                     , SA.width (String.fromFloat handleWidth)
                     , SA.height (String.fromInt (rowHeight isNarrow - 2))
                     , SA.fill "transparent"
                     , HA.style "cursor"
                        (if editable then
                            "ew-resize"

                         else
                            "inherit"
                        )
                     , HA.style "touch-action"
                        (if editable then
                            "none"

                         else
                            "auto"
                        )
                     , HA.title
                        (if editable then
                            "ドラッグで長さを変える"

                         else
                            ""
                        )
                     ]
                        ++ handlePointerAttrs
                        ++ (if editable then
                                [ Html.Events.stopPropagationOn "pointerdown"
                                    (Decode.map (\pos -> ( config.pressedNote note.id ResizeLeft pos, True )) notePressDecoder)
                                ]
                                    ++ notePointerListeners config
                                    ++ [ Html.Events.preventDefaultOn "contextmenu"
                                            (Decode.succeed ( config.rightClickedNote note.id, True ))
                                       ]

                            else
                                []
                           )
                    )
                    []
                ]

            else
                []
           )


rubberBandView : Maybe { x : Float, y : Float, w : Float, h : Float } -> List (Svg.Svg msg)
rubberBandView band =
    case band of
        Nothing ->
            []

        Just r ->
            [ Svg.rect
                [ SA.x (String.fromFloat r.x)
                , SA.y (String.fromFloat r.y)
                , SA.width (String.fromFloat r.w)
                , SA.height (String.fromFloat r.h)
                , SA.fill (Theme.withAlpha 0.15 Theme.primary)
                , SA.stroke Theme.primary
                , SA.strokeDasharray "4 2"
                , SA.pointerEvents "none"
                ]
                []
            ]


playheadLine : Int -> Int -> Int -> Svg.Svg msg
playheadLine pxPerSixteenth height ticks =
    Svg.line
        [ SA.x1 (String.fromFloat (ticksToPixels pxPerSixteenth ticks))
        , SA.y1 "0"
        , SA.x2 (String.fromFloat (ticksToPixels pxPerSixteenth ticks))
        , SA.y2 (String.fromInt height)
        , SA.stroke Theme.playhead
        , SA.strokeWidth "2"
        , SA.pointerEvents "none"
        ]
        []
