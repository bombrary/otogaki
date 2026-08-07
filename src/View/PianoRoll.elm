module View.PianoRoll exposing
    ( Config
    , SectionSpan
    , maxPitch
    , minPitch
    , pianoRollScrollId
    , pixelsToTicks
    , rowHeight
    , ticksToPixels
    , view
    , yToPitch
    )

import Array exposing (Array)
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
import View.Palette as Palette
import View.Style as Style


type alias Config msg =
    { pressedEmpty : { offsetX : Float, offsetY : Float, clientX : Float, clientY : Float, shift : Bool, seekMod : Bool } -> msg
    , pressedNote : Int -> Bool -> { clientX : Float, clientY : Float, shift : Bool } -> msg
    , doubleClickedNote : Int -> msg
    , rightClickedNote : Int -> msg
    , pressedRuler : { offsetX : Float, clientX : Float, shift : Bool } -> msg
    , pressedLoopHandle : Bool -> Float -> msg
    , pressedKey : Int -> msg
    }


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


rowHeight : Int
rowHeight =
    14


pxPerSixteenth : Int
pxPerSixteenth =
    20


minPitch : Int
minPitch =
    36


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


barWidth : Int
barWidth =
    16 * pxPerSixteenth


pixelsToTicks : Float -> Int
pixelsToTicks px =
    round (px * toFloat Data.Time.ticksPerSixteenth / toFloat pxPerSixteenth)


ticksToPixels : Int -> Float
ticksToPixels ticks =
    toFloat ticks * toFloat pxPerSixteenth / toFloat Data.Time.ticksPerSixteenth


yToPitch : Float -> Int
yToPitch y =
    maxPitch - floor (y / toFloat rowHeight)


gridWidth : Int -> Int
gridWidth totalBars =
    totalBars * barWidth


gridHeight : Int
gridHeight =
    (maxPitch - minPitch + 1) * rowHeight


view : Config msg -> ViewOpts -> Html msg
view config opts =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "margin-top" "1rem"
        , HA.style "border" "1px solid #ccc"
        ]
        [ keyColumn config (opts.waveform /= Nothing) opts.highlightedPitch opts.scalePitchClasses
        , Html.div
            [ HA.id pianoRollScrollId
            , HA.style "overflow-x" "auto"
            , HA.style "flex" "1"
            , HA.tabindex 0
            , HA.attribute "aria-label" "ピアノロール（矢印キーでノートを選択・移動）"
            ]
            [ rulerView config opts
            , waveformView opts
            , gridView config opts
            ]
        ]


waveformView : ViewOpts -> Html msg
waveformView opts =
    case opts.waveform of
        Nothing ->
            Html.text ""

        Just w ->
            Svg.Lazy.lazy5 waveStrip w.peaks w.peakDt w.secsPerTick w.offsetMs opts.totalBars


{-| 参考オーディオの波形帯。BPM とオフセットに合わせてタイムライン上に描く。
-}
waveStrip : Array Float -> Float -> Float -> Int -> Int -> Svg.Svg msg
waveStrip peaks peakDt secsPerTick offsetMs totalBars =
    let
        width =
            gridWidth totalBars

        colStep =
            3

        mid =
            toFloat waveHeight / 2

        ampAt x =
            let
                sec =
                    toFloat (pixelsToTicks (toFloat x)) * secsPerTick + toFloat offsetMs / 1000
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
                            [ SA.x1 (String.fromInt (i * barWidth))
                            , SA.y1 "0"
                            , SA.x2 (String.fromInt (i * barWidth))
                            , SA.y2 (String.fromInt waveHeight)
                            , SA.stroke "#ccc"
                            ]
                            []
                    )
    in
    Svg.svg
        [ SA.width (String.fromInt width)
        , SA.height (String.fromInt waveHeight)
        , SA.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt waveHeight)
        , HA.style "display" "block"
        , HA.style "background" "#fbfcff"
        , HA.style "border-bottom" "1px solid #ddd"
        ]
        (barLines
            ++ [ Svg.path [ SA.d d, SA.stroke "#8fb8dd", SA.strokeWidth "2" ] [] ]
        )


keyColumn : Config msg -> Bool -> Set Int -> Set Int -> Html msg
keyColumn config hasWave highlightedPitch scalePitchClasses =
    let
        spacerHeight =
            rulerHeight
                + (if hasWave then
                    waveHeight

                   else
                    0
                  )
    in
    Html.div
        [ HA.style "flex" "0 0 44px"
        , HA.style "border-right" "1px solid #bbb"
        ]
        (Html.div [ HA.style "height" (String.fromInt spacerHeight ++ "px"), HA.style "box-sizing" "border-box", HA.style "border-bottom" "1px solid #ddd" ] []
            :: (List.range minPitch maxPitch
                    |> List.reverse
                    |> List.map (keyRow config highlightedPitch scalePitchClasses)
               )
        )


keyRow : Config msg -> Set Int -> Set Int -> Int -> Html msg
keyRow config highlightedPitch scalePitchClasses pitch =
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
        [ HA.style "height" (String.fromInt rowHeight ++ "px")
        , HA.style "box-sizing" "border-box"
        , HA.style "position" "relative"
        , HA.style "background"
            (if isHighlighted then
                Style.colorHighlight

             else if isBlack then
                "#444"

             else
                "#fff"
            )
        , HA.style "color"
            (if isHighlighted then
                "#333"

             else if isBlack then
                "#eee"

             else
                "#555"
            )
        , HA.style "border-bottom" "1px solid #ddd"
        , HA.style "font-size" "9px"
        , HA.style "line-height" (String.fromInt (rowHeight - 1) ++ "px")
        , HA.style "text-align" "right"
        , HA.style "padding-right" "3px"
        , HA.style "cursor" "pointer"
        , HA.style "user-select" "none"
        , Html.Events.onMouseDown (config.pressedKey pitch)
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
                        , HA.style "background" "#4a90d9"
                        , HA.style "pointer-events" "none"
                        ]
                        []
                    ]

                else
                    []
               )
        )


rulerView : Config msg -> ViewOpts -> Html msg
rulerView config opts =
    Svg.svg
        [ SA.width (String.fromInt (gridWidth opts.totalBars))
        , SA.height (String.fromInt rulerHeight)
        , SA.viewBox ("0 0 " ++ String.fromInt (gridWidth opts.totalBars) ++ " " ++ String.fromInt rulerHeight)
        , HA.style "display" "block"
        , HA.style "cursor" "pointer"
        , HA.title "クリックで再生位置を移動。shift + ドラッグでループ区間を作成"
        , Html.Events.on "mousedown" (Decode.map config.pressedRuler rulerPressDecoder)
        ]
        (Svg.rect
            [ SA.x "0"
            , SA.y "0"
            , SA.width (String.fromInt (gridWidth opts.totalBars))
            , SA.height (String.fromInt rulerHeight)
            , SA.fill "#f8f8f8"
            ]
            []
            :: List.concat (List.indexedMap sectionBand opts.sections)
            ++ barNumbers opts.totalBars
            ++ loopBandView config opts.loopEditable opts.loop
            ++ [ playheadLine rulerHeight opts.playheadTicks ]
        )


rulerPressDecoder : Decode.Decoder { offsetX : Float, clientX : Float, shift : Bool }
rulerPressDecoder =
    Decode.map3 (\ox cx sh -> { offsetX = ox, clientX = cx, shift = sh })
        (Decode.field "offsetX" Decode.float)
        (Decode.field "clientX" Decode.float)
        (Decode.field "shiftKey" Decode.bool)


{-| ルーラー上にループ区間を琥珀色の帯で示す。editable が真なら左右端につまむハンドルを追加で描く（「範囲」モードのループのみ）。
-}
loopBandView : Config msg -> Bool -> Maybe { startTicks : Int, endTicks : Int } -> List (Svg.Svg msg)
loopBandView config editable loop =
    case loop of
        Nothing ->
            []

        Just l ->
            let
                x0 =
                    ticksToPixels l.startTicks

                x1 =
                    ticksToPixels l.endTicks

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
                        [ loopHandle config False x0
                        , loopHandle config True x1
                        ]

                    else
                        []
                   )


{-| ループ帯の端をつまんで伸縮するためのハンドル。バンド本体（6pxx6px）より少し大きめにしてつかみやすくする。
-}
loopHandle : Config msg -> Bool -> Float -> Svg.Svg msg
loopHandle config isEnd x =
    Svg.rect
        [ SA.x (String.fromFloat (x - 6))
        , SA.y "10"
        , SA.width "12"
        , SA.height "18"
        , SA.fill Style.colorLoopHandle
        , SA.cursor "ew-resize"
        , Html.Events.stopPropagationOn "mousedown"
            (Decode.map (\cx -> ( config.pressedLoopHandle isEnd cx, True )) (Decode.field "clientX" Decode.float))
        ]
        []


sectionBand : Int -> SectionSpan -> List (Svg.Svg msg)
sectionBand idx span =
    [ Svg.rect
        [ SA.x (String.fromInt (span.startBar * barWidth))
        , SA.y "0"
        , SA.width (String.fromInt (span.lengthBars * barWidth))
        , SA.height "16"
        , SA.fill (Palette.sectionColor idx)
        , SA.fillOpacity "0.85"
        ]
        []
    , Svg.text_
        [ SA.x (String.fromInt (span.startBar * barWidth + 4))
        , SA.y "12"
        , SA.fontSize "10"
        , SA.fill "#fff"
        , SA.pointerEvents "none"
        ]
        [ Svg.text span.name ]
    ]


barNumbers : Int -> List (Svg.Svg msg)
barNumbers totalBars =
    List.range 0 (totalBars - 1)
        |> List.concatMap
            (\i ->
                [ Svg.line
                    [ SA.x1 (String.fromInt (i * barWidth))
                    , SA.y1 "16"
                    , SA.x2 (String.fromInt (i * barWidth))
                    , SA.y2 (String.fromInt rulerHeight)
                    , SA.stroke "#ccc"
                    ]
                    []
                , Svg.text_
                    [ SA.x (String.fromInt (i * barWidth + 4))
                    , SA.y "30"
                    , SA.fontSize "10"
                    , SA.fill "#888"
                    , SA.pointerEvents "none"
                    ]
                    [ Svg.text (String.fromInt (i + 1)) ]
                ]
            )


gridView : Config msg -> ViewOpts -> Html msg
gridView config opts =
    Svg.svg
        [ SA.width (String.fromInt (gridWidth opts.totalBars))
        , SA.height (String.fromInt gridHeight)
        , SA.viewBox ("0 0 " ++ String.fromInt (gridWidth opts.totalBars) ++ " " ++ String.fromInt gridHeight)
        , HA.style "display" "block"
        , HA.style "cursor" "crosshair"
        , Html.Events.on "mousedown" (Decode.map config.pressedEmpty emptyPressDecoder)
        ]
        (rowBackgrounds opts.totalBars
            ++ List.concat (List.indexedMap sectionTint opts.sections)
            ++ verticalLines opts.totalBars
            ++ List.concat (List.indexedMap (\idx notes -> List.map (ghostNoteView idx) notes) opts.ghostNoteGroups)
            ++ List.concatMap (noteView config opts.selectedIds) opts.notes
            ++ rubberBandView opts.rubberBand
            ++ loopLinesView gridHeight opts.loop
            ++ [ playheadLine gridHeight opts.playheadTicks ]
        )


{-| グリッド上にループ区間の開始・終了を縦の破線で示す。面を塗るとスケールガイドや sectionTint と層が重なって見づらくなるので線のみにする。
-}
loopLinesView : Int -> Maybe { startTicks : Int, endTicks : Int } -> List (Svg.Svg msg)
loopLinesView height loop =
    case loop of
        Nothing ->
            []

        Just l ->
            [ loopLine height (ticksToPixels l.startTicks)
            , loopLine height (ticksToPixels l.endTicks)
            ]


loopLine : Int -> Float -> Svg.Svg msg
loopLine height x =
    Svg.line
        [ SA.x1 (String.fromFloat x)
        , SA.y1 "0"
        , SA.x2 (String.fromFloat x)
        , SA.y2 (String.fromInt height)
        , SA.stroke "#f1c40f"
        , SA.strokeWidth "2"
        , SA.strokeDasharray "4 2"
        , SA.pointerEvents "none"
        ]
        []


sectionTint : Int -> SectionSpan -> List (Svg.Svg msg)
sectionTint idx span =
    [ Svg.rect
        [ SA.x (String.fromInt (span.startBar * barWidth))
        , SA.y "0"
        , SA.width (String.fromInt (span.lengthBars * barWidth))
        , SA.height (String.fromInt gridHeight)
        , SA.fill (Palette.sectionColor idx)
        , SA.fillOpacity "0.05"
        , SA.pointerEvents "none"
        ]
        []
    ]


emptyPressDecoder : Decode.Decoder { offsetX : Float, offsetY : Float, clientX : Float, clientY : Float, shift : Bool, seekMod : Bool }
emptyPressDecoder =
    Decode.map8
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


notePressDecoder : Decode.Decoder { clientX : Float, clientY : Float, shift : Bool }
notePressDecoder =
    Decode.map3 (\cx cy sh -> { clientX = cx, clientY = cy, shift = sh })
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
        (Decode.field "shiftKey" Decode.bool)


rowBackgrounds : Int -> List (Svg.Svg msg)
rowBackgrounds totalBars =
    List.range minPitch maxPitch
        |> List.map
            (\pitch ->
                let
                    y =
                        (maxPitch - pitch) * rowHeight

                    isBlack =
                        List.member (modBy 12 pitch) [ 1, 3, 6, 8, 10 ]
                in
                Svg.rect
                    [ SA.x "0"
                    , SA.y (String.fromInt y)
                    , SA.width (String.fromInt (gridWidth totalBars))
                    , SA.height (String.fromInt rowHeight)
                    , SA.fill
                        (if isBlack then
                            "#f0f0f0"

                         else
                            "#ffffff"
                        )
                    , SA.stroke "#e8e8e8"
                    , SA.strokeWidth "0.5"
                    ]
                    []
            )


verticalLines : Int -> List (Svg.Svg msg)
verticalLines totalBars =
    List.range 0 (totalBars * 16)
        |> List.map
            (\i ->
                let
                    x =
                        i * pxPerSixteenth

                    isBar =
                        modBy 16 i == 0

                    isBeat =
                        modBy 4 i == 0
                in
                Svg.line
                    [ SA.x1 (String.fromInt x)
                    , SA.y1 "0"
                    , SA.x2 (String.fromInt x)
                    , SA.y2 (String.fromInt gridHeight)
                    , SA.stroke
                        (if isBar then
                            "#999"

                         else if isBeat then
                            "#ccc"

                         else
                            "#eee"
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
ghostNoteView : Int -> Note -> Svg.Svg msg
ghostNoteView idx note =
    let
        x =
            ticksToPixels note.start

        y =
            (maxPitch - note.pitch) * rowHeight

        w =
            ticksToPixels note.duration
    in
    Svg.rect
        [ SA.x (String.fromFloat x)
        , SA.y (String.fromInt (y + 1))
        , SA.width (String.fromFloat (Basics.max 2 (w - 1)))
        , SA.height (String.fromInt (rowHeight - 2))
        , SA.rx "2"
        , SA.fill (Palette.sectionColor idx)
        , SA.fillOpacity "0.35"
        , SA.pointerEvents "none"
        ]
        []


noteView : Config msg -> Set Int -> Note -> List (Svg.Svg msg)
noteView config selectedIds note =
    let
        x =
            ticksToPixels note.start

        y =
            (maxPitch - note.pitch) * rowHeight

        w =
            ticksToPixels note.duration

        handleWidth =
            6.0

        selected =
            Set.member note.id selectedIds
    in
    [ Svg.rect
        [ SA.x (String.fromFloat x)
        , SA.y (String.fromInt (y + 1))
        , SA.width (String.fromFloat (Basics.max 2 (w - 1)))
        , SA.height (String.fromInt (rowHeight - 2))
        , SA.rx "2"
        , SA.fill
            (if selected then
                Style.colorSelection

             else
                "#4a90d9"
            )
        , SA.stroke
            (if selected then
                "#9c1f52"

             else
                "none"
            )
        , HA.style "cursor" "move"
        , Html.Events.stopPropagationOn "mousedown"
            (Decode.map (\pos -> ( config.pressedNote note.id False pos, True )) notePressDecoder)
        , Html.Events.stopPropagationOn "dblclick"
            (Decode.succeed ( config.doubleClickedNote note.id, True ))
        , Html.Events.preventDefaultOn "contextmenu"
            (Decode.succeed ( config.rightClickedNote note.id, True ))
        ]
        []
    , Svg.rect
        [ SA.x (String.fromFloat (x + w - handleWidth))
        , SA.y (String.fromInt (y + 1))
        , SA.width (String.fromFloat handleWidth)
        , SA.height (String.fromInt (rowHeight - 2))
        , SA.fill
            (if selected then
                "#9c1f52"

             else
                "#2a70b9"
            )
        , HA.style "cursor" "ew-resize"
        , Html.Events.stopPropagationOn "mousedown"
            (Decode.map (\pos -> ( config.pressedNote note.id True pos, True )) notePressDecoder)
        , Html.Events.preventDefaultOn "contextmenu"
            (Decode.succeed ( config.rightClickedNote note.id, True ))
        ]
        []
    ]


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
                , SA.fill "rgba(74, 144, 217, 0.15)"
                , SA.stroke "#4a90d9"
                , SA.strokeDasharray "4 2"
                , SA.pointerEvents "none"
                ]
                []
            ]


playheadLine : Int -> Int -> Svg.Svg msg
playheadLine height ticks =
    Svg.line
        [ SA.x1 (String.fromFloat (ticksToPixels ticks))
        , SA.y1 "0"
        , SA.x2 (String.fromFloat (ticksToPixels ticks))
        , SA.y2 (String.fromInt height)
        , SA.stroke "#e74c3c"
        , SA.strokeWidth "2"
        ]
        []
