module View.DrumEditor exposing (Config, ViewOpts, cellAt, pitchesInYRange, rowPitches, rowsFor, view)

import Data.DrumPattern
import Data.Note exposing (Note)
import Data.Time
import Html exposing (Html, button, div, option, select, span, text)
import Html.Attributes as HA
import Html.Events
import Json.Decode as Decode
import Set exposing (Set)
import Svg
import Svg.Attributes as SA
import View.PianoRoll as PianoRoll
import View.Style as Style
import View.Theme as Theme


type alias Config msg =
    { pressedCell : { pitch : Int, tick : Int, offsetX : Float, offsetY : Float, clientX : Float, clientY : Float, shift : Bool, isTouch : Bool, timeStamp : Float } -> msg
    , draggedWhilePressingCell : { clientX : Float, clientY : Float, alt : Bool } -> msg
    , releasedCellPress : msg
    , rightClickedCell : { pitch : Int, tick : Int } -> msg
    , doubleClickedCell : { pitch : Int, tick : Int } -> msg
    , pressedVelocityBar : Int -> { clientX : Float, clientY : Float } -> msg
    , appliedPreset : String -> msg
    , changedFillBars : String -> msg
    , changedApplyTarget : String -> msg
    , changedApplyMode : String -> msg
    , toggledLane : Int -> msg
    , pressedRuler : { offsetX : Float, clientX : Float, shift : Bool } -> msg
    , pressedLoopHandle : Bool -> Float -> msg
    , wheelZoomedRuler : { deltaY : Float, offsetX : Float } -> msg
    , scrolled : { scrollLeft : Float, scrollTop : Float, clientWidth : Float } -> msg
    }


type alias ViewOpts =
    { sections : List PianoRoll.SectionSpan
    , totalBars : Int
    , fillBars : Int
    , notes : List Note
    , selectedIds : Set Int
    , playheadTicks : Int
    , pxPerSixteenth : Int
    , gridUnit : Data.Time.GridUnit
    , loop : Maybe { startTicks : Int, endTicks : Int }
    , loopEditable : Bool
    , rubberBand : Maybe { x : Float, y : Float, w : Float, h : Float }
    , applyTargetValue : String
    , applyModeValue : String
    , rangeLabel : String
    , excludedLanes : Set Int
    }


{-| 常に表示するコア行。js/audio.js の DRUM_ROLES のうち、ドラフトで実際に使う中心的な音。
-}
coreRows : List ( Int, String )
coreRows =
    [ ( 49, "クラッシュ" )
    , ( 51, "ライド" )
    , ( 46, "オープンHH" )
    , ( 42, "ハイハット" )
    , ( 45, "タム" )
    , ( 38, "スネア" )
    , ( 36, "キック" )
    ]


rowHeight : Int
rowHeight =
    22


{-| DRUM_ROLES に載っている全 pitch のラベル。コア行に無い pitch でも、ここにあればライド・クラップなど
意味のある名前がつく。
-}
labelFor : Int -> String
labelFor pitch =
    case pitch of
        35 ->
            "キック(低)"

        37 ->
            "リムショット"

        39 ->
            "クラップ"

        41 ->
            "ロータム"

        48 ->
            "ハイタム"

        54 ->
            "シェイカー"

        56 ->
            "カウベル"

        _ ->
            "pitch " ++ String.fromInt pitch


{-| 表示する行。コア行は常に出し、それ以外はノートが実在する pitch だけを高い順に上に足す。
固定行だけだと「置いたのに見えない」（例：バラードのライド 51）が起きうるのを構造的になくす。
extra 行はコア行より上にまとめ、コア行の相対位置が動かないようにする。
-}
rowsFor : List Note -> List ( Int, String )
rowsFor notes =
    let
        core =
            Set.fromList (List.map Tuple.first coreRows)

        extra =
            notes
                |> List.map .pitch
                |> Set.fromList
                |> (\s -> Set.diff s core)
                |> Set.toList
                |> List.sortBy negate
                |> List.map (\p -> ( p, labelFor p ))
    in
    extra ++ coreRows


{-| 現在の行の pitch 一覧。レーン絞り込み UI の既定集合（全行含む）を求めるのに Main から使う。
-}
rowPitches : List Note -> List Int
rowPitches notes =
    List.map Tuple.first (rowsFor notes)


totalSteps : Int -> Int
totalSteps totalBars =
    totalBars * 16


gridWidth : Int -> Int -> Int
gridWidth pxPerSixteenth totalBars =
    totalSteps totalBars * pxPerSixteenth


gridHeight : List ( Int, String ) -> Int
gridHeight rows =
    List.length rows * rowHeight


rowIndexOf : List ( Int, String ) -> Int -> Maybe Int
rowIndexOf rows pitch =
    rows
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, ( p, _ ) ) -> p == pitch)
        |> List.head
        |> Maybe.map Tuple.first


{-| ラバーバンド選択のY範囲（ピクセル座標）に交差する行のpitch集合を返す。
`rows` は非連続・非単調のpitchリストなので、行indexとの交差判定を経由する。
-}
pitchesInYRange : List ( Int, String ) -> Float -> Float -> Set Int
pitchesInYRange rows y0 y1 =
    rows
        |> List.indexedMap (\i ( pitch, _ ) -> ( i, pitch ))
        |> List.filter (\( i, _ ) -> toFloat (i * rowHeight) < y1 && toFloat ((i + 1) * rowHeight) > y0)
        |> List.map Tuple.second
        |> Set.fromList


{-| ピクセル座標(offsetX, offsetY)からグリッドセルのpitch/tickを求める共通ヘルパー。
-}
cellAt : List ( Int, String ) -> Data.Time.GridUnit -> Int -> Float -> Float -> { pitch : Int, tick : Int }
cellAt rows gridUnit pxPerSixteenth ox oy =
    let
        tick =
            Data.Time.snapFloor (Data.Time.gridTicks gridUnit) (PianoRoll.pixelsToTicks pxPerSixteenth ox)

        rowIdx =
            floor (oy / toFloat rowHeight)

        pitch =
            rows
                |> List.drop rowIdx
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 36
    in
    { pitch = pitch, tick = tick }


{-| pointerdown用。pitch/tickに加えてshift判定とドラッグ開始点（rubber band用）を持つ。
buttonフィルタを必ず入れる: これがないと右クリック（contextmenuで削除する想定）でもpointerdownが先に
発火し、rubberBandがJustになってviewDragOverlayが画面を覆うため、後続のcontextmenuイベントが
セル要素まで届かず右クリック削除が動かなくなる。
-}
cellPressDecoder :
    List ( Int, String )
    -> Data.Time.GridUnit
    -> Int
    -> Decode.Decoder { pitch : Int, tick : Int, offsetX : Float, offsetY : Float, clientX : Float, clientY : Float, shift : Bool, isTouch : Bool, timeStamp : Float }
cellPressDecoder rows gridUnit pxPerSixteenth =
    Decode.field "button" Decode.int
        |> Decode.andThen
            (\button ->
                if button == 0 then
                    Decode.map7
                        (\ox oy cx cy sh touch ts ->
                            let
                                cell =
                                    cellAt rows gridUnit pxPerSixteenth ox oy
                            in
                            { pitch = cell.pitch, tick = cell.tick, offsetX = ox, offsetY = oy, clientX = cx, clientY = cy, shift = sh, isTouch = touch, timeStamp = ts }
                        )
                        (Decode.field "offsetX" Decode.float)
                        (Decode.field "offsetY" Decode.float)
                        (Decode.field "clientX" Decode.float)
                        (Decode.field "clientY" Decode.float)
                        (Decode.field "shiftKey" Decode.bool)
                        (Decode.field "pointerType" Decode.string |> Decode.map ((==) "touch"))
                        (Decode.field "timeStamp" Decode.float)

                else
                    Decode.fail "not left button"
            )


{-| dblclick / contextmenu 用。pitch/tickだけあればよい。
-}
cellClickDecoder : List ( Int, String ) -> Data.Time.GridUnit -> Int -> Decode.Decoder { pitch : Int, tick : Int }
cellClickDecoder rows gridUnit pxPerSixteenth =
    Decode.map2 (cellAt rows gridUnit pxPerSixteenth)
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)


targetOptions : List ( String, String )
targetOptions =
    [ ( "section", "セクション" ), ( "loop", "ループ範囲" ), ( "playhead", "プレイヘッドから" ) ]


modeOptions : List ( String, String )
modeOptions =
    [ ( "replaceLanes", "差し替え" ), ( "merge", "重ねる" ), ( "replace", "全消去して差し替え" ) ]


labeledSelect : (String -> msg) -> String -> List ( String, String ) -> Html msg
labeledSelect toMsg currentValue options =
    select [ Html.Events.onInput toMsg ]
        (List.map
            (\( value, label ) ->
                option [ HA.value value, HA.selected (value == currentValue) ] [ text label ]
            )
            options
        )


view : Config msg -> ViewOpts -> Html msg
view config opts =
    let
        rows =
            rowsFor opts.notes
    in
    div [ HA.style "margin-top" "1rem" ]
        [ div [ HA.style "display" "flex", HA.style "gap" "0.4rem", HA.style "align-items" "center", HA.style "flex-wrap" "wrap" ]
            [ span [ HA.style "font-size" "0.85rem" ] [ text "適用先: " ]
            , labeledSelect config.changedApplyTarget opts.applyTargetValue targetOptions
            , span [ HA.style "font-size" "0.85rem", HA.style "color" Theme.onSurfaceVariant ] [ text opts.rangeLabel ]
            , span [ HA.style "font-size" "0.85rem" ] [ text " 適用方法: " ]
            , labeledSelect config.changedApplyMode opts.applyModeValue modeOptions
            ]
        , div [ HA.style "display" "flex", HA.style "gap" "0.4rem", HA.style "align-items" "center", HA.style "flex-wrap" "wrap", HA.style "margin-top" "0.3rem" ]
            (List.map
                (\pattern ->
                    button (Style.baseButton ++ [ Html.Events.onClick (config.appliedPreset pattern.name) ]) [ text pattern.name ]
                )
                Data.DrumPattern.patterns
                ++ [ span [ HA.style "font-size" "0.85rem" ] [ text " 長さ:" ]
                   , select
                        [ Html.Events.onInput config.changedFillBars
                        , HA.disabled (opts.applyTargetValue /= "playhead")
                        ]
                        (List.map
                            (\n ->
                                option
                                    [ HA.value (String.fromInt n)
                                    , HA.selected (n == opts.fillBars)
                                    ]
                                    [ text (String.fromInt n ++ "小節") ]
                            )
                            [ 1, 2, 4, 8, 16, 32, 64 ]
                        )
                   ]
            )
        , div
            [ HA.style "display" "flex"
            , HA.style "margin-top" "0.4rem"
            , HA.style "border" ("1px solid " ++ Theme.outlineVariant)
            ]
            [ labelColumn rows config opts
            , div
                [ HA.id PianoRoll.pianoRollScrollId
                , HA.style "overflow-x" "auto"
                , HA.style "flex" "1"
                , HA.tabindex 0
                , HA.attribute "aria-label" "ドラムステップグリッド"
                , Html.Events.on "scroll" (PianoRoll.scrollDecoder config.scrolled)
                ]
                [ PianoRoll.rulerViewWith
                    False
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
                , gridView rows config opts
                , PianoRoll.velocityLaneViewWith
                    { pressedVelocityBar = config.pressedVelocityBar }
                    { pxPerSixteenth = opts.pxPerSixteenth
                    , totalBars = opts.totalBars
                    , notes = opts.notes
                    , selectedIds = opts.selectedIds
                    , playheadTicks = opts.playheadTicks
                    }
                ]
            ]
        ]


labelColumn : List ( Int, String ) -> Config msg -> ViewOpts -> Html msg
labelColumn rows config opts =
    div [ HA.style "flex" "0 0 90px", HA.style "border-right" ("1px solid " ++ Theme.outline) ]
        (div
            [ HA.style "height" (String.fromInt PianoRoll.rulerHeight ++ "px")
            , HA.style "box-sizing" "border-box"
            , HA.style "border-bottom" ("1px solid " ++ Theme.outlineVariant)
            , HA.style "font-size" "9px"
            , HA.style "color" Theme.onSurfaceVariant
            , HA.style "text-align" "right"
            , HA.style "padding-right" "0.4rem"
            , HA.style "display" "flex"
            , HA.style "align-items" "flex-end"
            , HA.style "justify-content" "flex-end"
            , HA.style "padding-bottom" "2px"
            ]
            [ text "適用" ]
            :: List.map
                (\( pitch, label ) ->
                    let
                        excluded =
                            Set.member pitch opts.excludedLanes
                    in
                    div
                        [ HA.style "height" (String.fromInt rowHeight ++ "px")
                        , HA.style "line-height" (String.fromInt rowHeight ++ "px")
                        , HA.style "font-size" "0.8rem"
                        , HA.style "text-align" "right"
                        , HA.style "padding-right" "0.4rem"
                        , HA.style "cursor" "pointer"
                        , HA.style "user-select" "none"
                        , HA.style "opacity"
                            (if excluded then
                                "0.4"

                             else
                                "1"
                            )
                        , HA.style "text-decoration"
                            (if excluded then
                                "line-through"

                             else
                                "none"
                            )
                        , HA.title "プリセット適用の対象（クリックで除外）"
                        , Html.Events.onClick (config.toggledLane pitch)
                        ]
                        [ text label ]
                )
                rows
            ++ [ div
                    [ HA.style "height" (String.fromInt PianoRoll.velocityLaneHeight ++ "px")
                    , HA.style "box-sizing" "border-box"
                    , HA.style "border-top" ("1px solid " ++ Theme.outlineVariant)
                    , HA.style "font-size" "9px"
                    , HA.style "color" Theme.onSurfaceVariant
                    , HA.style "padding" "2px 3px"
                    , HA.style "text-align" "right"
                    ]
                    [ text "Vel" ]
               ]
        )


gridView : List ( Int, String ) -> Config msg -> ViewOpts -> Html msg
gridView rows config opts =
    let
        h =
            gridHeight rows
    in
    Svg.svg
        [ SA.width (String.fromInt (gridWidth opts.pxPerSixteenth opts.totalBars))
        , SA.height (String.fromInt h)
        , SA.viewBox ("0 0 " ++ String.fromInt (gridWidth opts.pxPerSixteenth opts.totalBars) ++ " " ++ String.fromInt h)
        , HA.style "display" "block"
        , HA.style "cursor" "pointer"
        , HA.style "touch-action" "none"
        , Html.Events.on "pointerdown"
            (Decode.map config.pressedCell (cellPressDecoder rows opts.gridUnit opts.pxPerSixteenth))
        , HA.attribute "data-pointer-capture" ""
        , Html.Events.on "pointermove"
            (Decode.map config.draggedWhilePressingCell PianoRoll.noteMoveDecoder)
        , Html.Events.on "pointerup" (Decode.succeed config.releasedCellPress)
        , Html.Events.on "pointercancel" (Decode.succeed config.releasedCellPress)
        , Html.Events.on "dblclick"
            (Decode.map config.doubleClickedCell (cellClickDecoder rows opts.gridUnit opts.pxPerSixteenth))
        , Html.Events.preventDefaultOn "contextmenu"
            (Decode.map (\cell -> ( config.rightClickedCell cell, True )) (cellClickDecoder rows opts.gridUnit opts.pxPerSixteenth))
        ]
        (backgroundRows rows opts.pxPerSixteenth opts.totalBars
            ++ List.concat (List.indexedMap (PianoRoll.sectionTintWithHeight h opts.pxPerSixteenth) opts.sections)
            ++ verticalLines opts.gridUnit opts.pxPerSixteenth opts.totalBars h
            ++ List.filterMap (activeCell rows opts.gridUnit opts.pxPerSixteenth opts.selectedIds) opts.notes
            ++ [ PianoRoll.playheadLine opts.pxPerSixteenth h opts.playheadTicks ]
            ++ PianoRoll.rubberBandView opts.rubberBand
        )


backgroundRows : List ( Int, String ) -> Int -> Int -> List (Svg.Svg msg)
backgroundRows rows pxPerSixteenth totalBars =
    List.indexedMap
        (\i _ ->
            Svg.rect
                [ SA.x "0"
                , SA.y (String.fromInt (i * rowHeight))
                , SA.width (String.fromInt (gridWidth pxPerSixteenth totalBars))
                , SA.height (String.fromInt rowHeight)
                , SA.fill
                    (if modBy 2 i == 0 then
                        Theme.surfaceContainerLowest

                     else
                        Theme.surfaceContainerLow
                    )
                , SA.stroke Theme.surfaceContainerHighest
                , SA.strokeWidth "0.5"
                ]
                []
        )
        rows


verticalLines : Data.Time.GridUnit -> Int -> Int -> Int -> List (Svg.Svg msg)
verticalLines gridUnit pxPerSixteenth totalBars h =
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
                        PianoRoll.ticksToPixels pxPerSixteenth t
                in
                Svg.line
                    [ SA.x1 (String.fromFloat x)
                    , SA.y1 "0"
                    , SA.x2 (String.fromFloat x)
                    , SA.y2 (String.fromInt h)
                    , SA.stroke
                        (if modBy Data.Time.ticksPerBar t == 0 then
                            Theme.outline

                         else if modBy Data.Time.ppq t == 0 then
                            Theme.outlineVariant

                         else
                            Theme.surfaceContainerHighest
                        )
                    ]
                    []
            )


activeCell : List ( Int, String ) -> Data.Time.GridUnit -> Int -> Set Int -> Note -> Maybe (Svg.Svg msg)
activeCell rows gridUnit pxPerSixteenth selectedIds note =
    rowIndexOf rows note.pitch
        |> Maybe.map
            (\rowIdx ->
                let
                    grid =
                        Data.Time.gridTicks gridUnit

                    cellPx =
                        PianoRoll.ticksToPixels pxPerSixteenth grid

                    x =
                        PianoRoll.ticksToPixels pxPerSixteenth (note.start // grid * grid)

                    pad =
                        Basics.min 2.0 (cellPx / 6)

                    selected =
                        Set.member note.id selectedIds
                in
                Svg.rect
                    [ SA.x (String.fromFloat (x + pad))
                    , SA.y (String.fromInt (rowIdx * rowHeight + 3))
                    , SA.width (String.fromFloat (Basics.max 2.0 (cellPx - pad * 2)))
                    , SA.height (String.fromInt (rowHeight - 6))
                    , SA.rx "3"
                    , SA.fill
                        (if selected then
                            Style.colorSelection

                         else
                            Theme.onPendingContainer
                        )
                    , SA.stroke
                        (if selected then
                            Theme.selectionDeep

                         else
                            "none"
                        )
                    ]
                    []
            )
