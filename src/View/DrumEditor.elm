module View.DrumEditor exposing (Config, ViewOpts, pitchesInYRange, view)

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
    , pressedRuler : { offsetX : Float, clientX : Float, shift : Bool } -> msg
    , pressedLoopHandle : Bool -> Float -> msg
    , wheelZoomedRuler : { deltaY : Float, offsetX : Float } -> msg
    , scrolled : { scrollLeft : Float, clientWidth : Float } -> msg
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
    }


rows : List ( Int, String )
rows =
    [ ( 49, "クラッシュ" )
    , ( 46, "オープンHH" )
    , ( 42, "ハイハット" )
    , ( 45, "タム" )
    , ( 38, "スネア" )
    , ( 36, "キック" )
    ]


rowHeight : Int
rowHeight =
    22


totalSteps : Int -> Int
totalSteps totalBars =
    totalBars * 16


gridWidth : Int -> Int -> Int
gridWidth pxPerSixteenth totalBars =
    totalSteps totalBars * pxPerSixteenth


gridHeight : Int
gridHeight =
    List.length rows * rowHeight


rowIndexOf : Int -> Maybe Int
rowIndexOf pitch =
    rows
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, ( p, _ ) ) -> p == pitch)
        |> List.head
        |> Maybe.map Tuple.first


{-| ラバーバンド選択のY範囲（ピクセル座標）に交差する行のpitch集合を返す。
`rows` は非連続・非単調なpitchリストなので、行indexとの交差判定を経由する。
-}
pitchesInYRange : Float -> Float -> Set Int
pitchesInYRange y0 y1 =
    rows
        |> List.indexedMap (\i ( pitch, _ ) -> ( i, pitch ))
        |> List.filter (\( i, _ ) -> toFloat (i * rowHeight) < y1 && toFloat ((i + 1) * rowHeight) > y0)
        |> List.map Tuple.second
        |> Set.fromList


{-| ピクセル座標(offsetX, offsetY)からグリッドセルのpitch/tickを求める共通ヘルパー。
-}
cellAt : Data.Time.GridUnit -> Int -> Float -> Float -> { pitch : Int, tick : Int }
cellAt gridUnit pxPerSixteenth ox oy =
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
    Data.Time.GridUnit
    -> Int
    -> Decode.Decoder { pitch : Int, tick : Int, offsetX : Float, offsetY : Float, clientX : Float, clientY : Float, shift : Bool, isTouch : Bool, timeStamp : Float }
cellPressDecoder gridUnit pxPerSixteenth =
    Decode.field "button" Decode.int
        |> Decode.andThen
            (\button ->
                if button == 0 then
                    Decode.map7
                        (\ox oy cx cy sh touch ts ->
                            let
                                cell =
                                    cellAt gridUnit pxPerSixteenth ox oy
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
cellClickDecoder : Data.Time.GridUnit -> Int -> Decode.Decoder { pitch : Int, tick : Int }
cellClickDecoder gridUnit pxPerSixteenth =
    Decode.map2 (cellAt gridUnit pxPerSixteenth)
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)


view : Config msg -> ViewOpts -> Html msg
view config opts =
    div [ HA.style "margin-top" "1rem" ]
        [ div [ HA.style "display" "flex", HA.style "gap" "0.4rem", HA.style "align-items" "center", HA.style "flex-wrap" "wrap" ]
            (span [ HA.style "font-size" "0.85rem" ] [ text "プリセット（選択セクションがあればその範囲、なければ先頭から右の長さ）: " ]
                :: List.map
                    (\pattern ->
                        button (Style.baseButton ++ [ Html.Events.onClick (config.appliedPreset pattern.name) ]) [ text pattern.name ]
                    )
                    Data.DrumPattern.patterns
                ++ [ span [ HA.style "font-size" "0.85rem" ] [ text " 長さ:" ]
                   , select [ Html.Events.onInput config.changedFillBars ]
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
            [ labelColumn
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
                , gridView config opts
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


labelColumn : Html msg
labelColumn =
    div [ HA.style "flex" "0 0 90px", HA.style "border-right" ("1px solid " ++ Theme.outline) ]
        (div
            [ HA.style "height" (String.fromInt PianoRoll.rulerHeight ++ "px")
            , HA.style "box-sizing" "border-box"
            , HA.style "border-bottom" ("1px solid " ++ Theme.outlineVariant)
            ]
            []
            :: List.map
                (\( _, label ) ->
                    div
                        [ HA.style "height" (String.fromInt rowHeight ++ "px")
                        , HA.style "line-height" (String.fromInt rowHeight ++ "px")
                        , HA.style "font-size" "0.8rem"
                        , HA.style "text-align" "right"
                        , HA.style "padding-right" "0.4rem"
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


gridView : Config msg -> ViewOpts -> Html msg
gridView config opts =
    Svg.svg
        [ SA.width (String.fromInt (gridWidth opts.pxPerSixteenth opts.totalBars))
        , SA.height (String.fromInt gridHeight)
        , SA.viewBox ("0 0 " ++ String.fromInt (gridWidth opts.pxPerSixteenth opts.totalBars) ++ " " ++ String.fromInt gridHeight)
        , HA.style "display" "block"
        , HA.style "cursor" "pointer"
        , HA.style "touch-action" "none"
        , Html.Events.on "pointerdown"
            (Decode.map config.pressedCell (cellPressDecoder opts.gridUnit opts.pxPerSixteenth))
        , HA.attribute "data-pointer-capture" ""
        , Html.Events.on "pointermove"
            (Decode.map config.draggedWhilePressingCell PianoRoll.noteMoveDecoder)
        , Html.Events.on "pointerup" (Decode.succeed config.releasedCellPress)
        , Html.Events.on "pointercancel" (Decode.succeed config.releasedCellPress)
        , Html.Events.on "dblclick"
            (Decode.map config.doubleClickedCell (cellClickDecoder opts.gridUnit opts.pxPerSixteenth))
        , Html.Events.preventDefaultOn "contextmenu"
            (Decode.map (\cell -> ( config.rightClickedCell cell, True )) (cellClickDecoder opts.gridUnit opts.pxPerSixteenth))
        ]
        (backgroundRows opts.pxPerSixteenth opts.totalBars
            ++ List.concat (List.indexedMap (PianoRoll.sectionTintWithHeight gridHeight opts.pxPerSixteenth) opts.sections)
            ++ verticalLines opts.gridUnit opts.pxPerSixteenth opts.totalBars
            ++ List.filterMap (activeCell opts.gridUnit opts.pxPerSixteenth opts.selectedIds) opts.notes
            ++ [ PianoRoll.playheadLine opts.pxPerSixteenth gridHeight opts.playheadTicks ]
            ++ PianoRoll.rubberBandView opts.rubberBand
        )


backgroundRows : Int -> Int -> List (Svg.Svg msg)
backgroundRows pxPerSixteenth totalBars =
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


verticalLines : Data.Time.GridUnit -> Int -> Int -> List (Svg.Svg msg)
verticalLines gridUnit pxPerSixteenth totalBars =
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
                    , SA.y2 (String.fromInt gridHeight)
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


activeCell : Data.Time.GridUnit -> Int -> Set Int -> Note -> Maybe (Svg.Svg msg)
activeCell gridUnit pxPerSixteenth selectedIds note =
    rowIndexOf note.pitch
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
