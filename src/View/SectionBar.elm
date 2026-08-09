module View.SectionBar exposing
    ( BlockExtras
    , Config
    , RulerData
    , defaultRegionPxPerBar
    , maxRegionPxPerBar
    , minRegionPxPerBar
    , regionZoomStep
    , sectionBarScrollId
    , sectionDragTargetIndex
    , sectionStartBars
    , view
    )

import Data.ChordTrack exposing (ChordSpan)
import Data.Key
import Data.Meter
import Data.Section exposing (Section)
import Html exposing (Html, button, div, input, span, text, textarea)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import Svg
import Svg.Attributes as SA
import View.ChordStrip as ChordStrip
import View.Palette as Palette
import View.Style as Style
import View.Zoom


type alias Config msg =
    { select : Int -> msg
    , add : msg
    , remove : Int -> msg
    , rename : Int -> String -> msg
    , changeBars : Int -> String -> msg
    , changeMemo : Int -> String -> msg
    , changeKey : Int -> String -> msg
    , changeMode : Int -> String -> msg
    , changeMeter : Int -> String -> msg
    , move : Int -> Int -> msg
    , changedInsertCount : String -> msg
    , insertBefore : Int -> msg
    , removeFromStart : Int -> msg
    , seekToStart : Int -> msg
    , transpose : Int -> Int -> msg
    , pressedBlock : Int -> Float -> msg
    , pressedResizeHandle : Int -> Float -> msg
    , wheelZoomed : { deltaY : Float, offsetX : Float } -> msg
    , pressedRuler : { offsetX : Float, clientX : Float, shift : Bool } -> msg
    , pressedLoopHandle : Bool -> Float -> msg
    , clickedChord : Int -> msg
    }


{-| セクションルーラーの描画に必要な、Msg ではないデータだけをまとめたもの。`ticksToPx` は Main 側が
`Data.Timeline.ticksToFractionalBar` と `pxPerBar` を合わせて作る変換関数（`SectionBar.elm` は `Data.Timeline` を
知らなくて済む）。
-}
type alias RulerData =
    { pxPerBar : Int
    , loopEditable : Bool
    , loop : Maybe { startTicks : Int, endTicks : Int }
    , playheadTicks : Int
    , ticksToPx : Int -> Float
    }


{-| ブロック表示に上乗せする再生情報。`playingIndex` は今再生中のセクションのリスト内インデックス
（`Main.sectionAtTicks` と同じ体系）。コード進行の表示は別途 `chordSpans`（tick位置ベース）で行うのでここには含まない。
-}
type alias BlockExtras =
    { playingIndex : Maybe Int
    }


{-| セクション行（リージョン行）のデフォルト横幅スケール。ズーム機構導入前の値。ピアノロールの
ズームとは意図的に別々（セクション行とピアノロールは現状別々のスクロール領域なので）。
-}
defaultRegionPxPerBar : Int
defaultRegionPxPerBar =
    40


minRegionPxPerBar : Int
minRegionPxPerBar =
    6


maxRegionPxPerBar : Int
maxRegionPxPerBar =
    120


{-| セクションルーラーのホイールズームの1ステップ。実装は `View.Zoom.step` に委譲（`PianoRoll.zoomStep` と同じ実装）。
-}
regionZoomStep : Float -> Int -> Int
regionZoomStep deltaY current =
    View.Zoom.step { min = minRegionPxPerBar, max = maxRegionPxPerBar } deltaY current


{-| ブロック行の水平スクロールで Browser.Dom から参照するスクロールコンテナの id。
-}
sectionBarScrollId : String
sectionBarScrollId =
    "section-bar-scroll"


regionRulerHeight : Int
regionRulerHeight =
    16


{-| 各セクションの開始小節（0-based）を、リストの先頭からの累積で求める。ルーラーの目盛り位置計算で使う。
-}
sectionStartBars : List Section -> List Int
sectionStartBars sections =
    List.foldl
        (\s ( acc, offset ) -> ( acc ++ [ offset ], offset + s.lengthBars ))
        ( [], 0 )
        sections
        |> Tuple.first


{-| ドラッグ中の累積 dx から、進行方向の隣接セクション幅の半分を超えたら入れ替え先 index と、次の判定に
持ち越す分の dx を返す。超えていなければ `Nothing`（まだ動かさない）。
古典的なソータブルリストの「隣の中間点を超えたら入れ替える」しきい値判定。
-}
sectionDragTargetIndex : Int -> List Section -> Int -> Float -> Maybe ( Int, Float )
sectionDragTargetIndex pxPerBar sections currentIndex accumDx =
    let
        dir =
            if accumDx > 0 then
                1

            else
                -1
    in
    if currentIndex + dir < 0 then
        Nothing

    else
        case sections |> List.drop (currentIndex + dir) |> List.head of
            Nothing ->
                Nothing

            Just neighbor ->
                let
                    widthPx =
                        toFloat (neighbor.lengthBars * pxPerBar)
                in
                if abs accumDx >= widthPx / 2 then
                    Just ( currentIndex + dir, accumDx - toFloat dir * widthPx )

                else
                    Nothing


view : Config msg -> RulerData -> List ChordSpan -> BlockExtras -> Maybe Int -> String -> List Section -> Maybe Int -> Maybe { sectionId : Int, lengthBars : Int } -> Html msg
view config rulerData chordSpans extras selectedId insertCountInput sections pendingDeleteId resizePreview =
    let
        totalBars =
            List.sum (List.map .lengthBars sections)
    in
    div [ HA.style "margin-top" "1rem" ]
        [ div
            [ HA.id sectionBarScrollId
            , HA.style "overflow-x" "auto"
            ]
            [ regionRulerView config rulerData.pxPerBar rulerData.loopEditable rulerData.loop rulerData.ticksToPx rulerData.playheadTicks sections
            , ChordStrip.view config.clickedChord
                { ticksToX = rulerData.ticksToPx
                , width = totalBars * rulerData.pxPerBar
                , playheadTicks = rulerData.playheadTicks
                , height = ChordStrip.height
                }
                chordSpans
            , div
                [ HA.style "display" "flex"
                , HA.style "align-items" "stretch"
                , HA.style "flex-wrap" "nowrap"
                ]
                (List.indexedMap (blockView config rulerData.pxPerBar selectedId resizePreview extras) sections
                    ++ [ button (Style.baseButton ++ [ HE.onClick config.add, HA.style "flex" "0 0 auto" ]) [ text "+ セクション" ] ]
                )
            ]
        , case selectedId |> Maybe.andThen (\sid -> sections |> List.filter (\s -> s.id == sid) |> List.head) of
            Just section ->
                editPanel config insertCountInput section pendingDeleteId

            Nothing ->
                text ""
        ]


{-| セクションの開始小節番号ラベル、マウスホイールズーム、shift+ドラッグによるループ作成・伸縮、再生位置の
表示を全て担う対話型ルーラー。`PianoRoll.elm` の `rulerView`/`loopBandView`/`loopHandle`/`playheadLine` と同じ形を
このスケール（`pxPerBar`）向けにミラーしている。
-}
regionRulerView : Config msg -> Int -> Bool -> Maybe { startTicks : Int, endTicks : Int } -> (Int -> Float) -> Int -> List Section -> Html msg
regionRulerView config pxPerBar loopEditable loop ticksToPx playheadTicks sections =
    let
        totalBars =
            List.sum (List.map .lengthBars sections)

        width =
            totalBars * pxPerBar

        boundaries =
            sectionStartBars sections

        tick bar =
            Svg.line
                [ SA.x1 (String.fromInt (bar * pxPerBar))
                , SA.y1 "0"
                , SA.x2 (String.fromInt (bar * pxPerBar))
                , SA.y2 (String.fromInt regionRulerHeight)
                , SA.stroke
                    (if List.member bar boundaries then
                        "#888"

                     else
                        "#ddd"
                    )
                , SA.strokeWidth
                    (if List.member bar boundaries then
                        "1.5"

                     else
                        "1"
                    )
                ]
                []

        label bar =
            Svg.text_
                [ SA.x (String.fromInt (bar * pxPerBar + 3))
                , SA.y "12"
                , SA.fontSize "10"
                , SA.fill "#888"
                ]
                [ Svg.text (String.fromInt (bar + 1)) ]

        loopBand =
            case loop of
                Nothing ->
                    []

                Just l ->
                    let
                        x0 =
                            ticksToPx l.startTicks

                        x1 =
                            ticksToPx l.endTicks

                        band =
                            Svg.rect
                                [ SA.x (String.fromFloat x0)
                                , SA.y "11"
                                , SA.width (String.fromFloat (Basics.max 0 (x1 - x0)))
                                , SA.height "4"
                                , SA.fill Style.colorLoop
                                , SA.pointerEvents "none"
                                ]
                                []
                    in
                    band
                        :: (if loopEditable then
                                [ regionLoopHandle config False x0
                                , regionLoopHandle config True x1
                                ]

                            else
                                []
                           )

        playhead =
            Svg.line
                [ SA.x1 (String.fromFloat (ticksToPx playheadTicks))
                , SA.y1 "0"
                , SA.x2 (String.fromFloat (ticksToPx playheadTicks))
                , SA.y2 (String.fromInt regionRulerHeight)
                , SA.stroke "#e74c3c"
                , SA.strokeWidth "2"
                ]
                []
    in
    Svg.svg
        [ SA.width (String.fromInt width)
        , SA.height (String.fromInt regionRulerHeight)
        , HA.style "display" "block"
        , HA.style "cursor" "pointer"
        , HA.title "クリックで再生位置を移動。shift + ドラッグでループ区間を作成。マウスホイールでズーム"
        , HE.on "mousedown" (Decode.map config.pressedRuler rulerPressDecoder)
        , HE.preventDefaultOn "wheel" (Decode.map (\w -> ( config.wheelZoomed w, True )) wheelDecoder)
        ]
        (List.map tick (List.range 0 totalBars)
            ++ List.map label boundaries
            ++ loopBand
            ++ [ playhead ]
        )


rulerPressDecoder : Decode.Decoder { offsetX : Float, clientX : Float, shift : Bool }
rulerPressDecoder =
    Decode.map3 (\ox cx sh -> { offsetX = ox, clientX = cx, shift = sh })
        (Decode.field "offsetX" Decode.float)
        (Decode.field "clientX" Decode.float)
        (Decode.field "shiftKey" Decode.bool)


wheelDecoder : Decode.Decoder { deltaY : Float, offsetX : Float }
wheelDecoder =
    Decode.map2 (\dy ox -> { deltaY = dy, offsetX = ox })
        (Decode.field "deltaY" Decode.float)
        (Decode.field "offsetX" Decode.float)


{-| ループ帯の端をつまんで伸縮するためのハンドル。`PianoRoll.loopHandle` と同じ形だが、このルーラーの高さ（regionRulerHeight）に
合わせて小さめにしている。
-}
regionLoopHandle : Config msg -> Bool -> Float -> Svg.Svg msg
regionLoopHandle config isEnd x =
    Svg.rect
        [ SA.x (String.fromFloat (x - 5))
        , SA.y "2"
        , SA.width "10"
        , SA.height "12"
        , SA.fill Style.colorLoopHandle
        , SA.cursor "ew-resize"
        , HE.stopPropagationOn "mousedown"
            (Decode.map (\cx -> ( config.pressedLoopHandle isEnd cx, True )) (Decode.field "clientX" Decode.float))
        ]
        []


blockView : Config msg -> Int -> Maybe Int -> Maybe { sectionId : Int, lengthBars : Int } -> BlockExtras -> Int -> Section -> Html msg
blockView config pxPerBar selectedId resizePreview extras idx section =
    let
        selected =
            selectedId == Just section.id

        displayLengthBars =
            case resizePreview of
                Just rp ->
                    if rp.sectionId == section.id then
                        rp.lengthBars

                    else
                        section.lengthBars

                Nothing ->
                    section.lengthBars

        playing =
            extras.playingIndex == Just idx

        baseTitle =
            if section.memo == "" then
                section.name

            else
                section.name ++ ": " ++ section.memo
    in
    div
        [ HA.style "width" (String.fromInt (displayLengthBars * pxPerBar) ++ "px")
        , HA.style "box-sizing" "border-box"
        , HA.style "flex-shrink" "0"
        , HA.style "padding" "0.3rem"
        , HA.style "min-height" "2.4rem"
        , HA.style "box-sizing" "border-box"
        , HA.style "text-align" "center"
        , HA.style "border"
            ((if selected then
                "2px solid "

              else
                "1px solid "
             )
                ++ Palette.sectionColor idx
            )
        , HA.style "border-radius" "4px"
        , HA.style "background"
            (if selected then
                Palette.sectionTint idx

             else
                Palette.neutral
            )
        , HA.style "box-shadow"
            (if playing then
                "0 0 8px " ++ Palette.sectionColor idx

             else
                "none"
            )
        , HA.style "position" "relative"
        , HA.style "cursor" "grab"
        , HA.style "font-size" "0.85rem"
        , HA.style "overflow" "hidden"
        , HA.style "white-space" "nowrap"
        , HE.on "mousedown" (Decode.map (config.pressedBlock section.id) (Decode.field "clientX" Decode.float))
        , HA.title baseTitle
        ]
        [ text section.name
        , if section.memo /= "" then
            span [ HA.style "margin-left" "0.2rem" ] [ text "📝" ]

          else
            text ""
        , div
            [ HA.style "position" "absolute"
            , HA.style "right" "0"
            , HA.style "top" "0"
            , HA.style "bottom" "0"
            , HA.style "width" "6px"
            , HA.style "cursor" "ew-resize"
            , HA.style "background" Style.colorPrimary
            , HA.style "border-radius" "0 3px 3px 0"
            , HE.stopPropagationOn "mousedown"
                (Decode.map (\cx -> ( config.pressedResizeHandle section.id cx, True )) (Decode.field "clientX" Decode.float))
            ]
            []
        ]


editPanel : Config msg -> String -> Section -> Maybe Int -> Html msg
editPanel config insertCountInput section pendingDeleteId =
    div
        [ HA.style "margin-top" "0.4rem"
        , HA.style "padding" "0.5rem"
        , HA.style "border" "1px solid #4a90d9"
        , HA.style "border-radius" "4px"
        , HA.style "background" "#f6faff"
        ]
        [ div [ HA.style "display" "flex", HA.style "gap" "0.5rem", HA.style "align-items" "center" ]
            [ input
                [ HA.value section.name
                , HE.onInput (config.rename section.id)
                , HA.style "width" "8rem"
                ]
                []
            , span [ HA.style "font-size" "0.85rem" ] [ text "小節数:" ]
            , input
                [ HA.type_ "number"
                , HA.value (String.fromInt section.lengthBars)
                , HE.on "change" (Decode.map (config.changeBars section.id) HE.targetValue)
                , HA.title "値を確定（フォーカスを外す）すると小節数が変わり、後ろのコード・ノートが自動で追従します"
                , HA.style "width" "3.5rem"
                ]
                []
            , button (Style.baseButton ++ [ HE.onClick (config.move section.id (negate 1)), HA.title "左へ移動", HA.attribute "aria-label" "左へ移動" ]) [ text "←" ]
            , button (Style.baseButton ++ [ HE.onClick (config.move section.id 1), HA.title "右へ移動", HA.attribute "aria-label" "右へ移動" ]) [ text "→" ]
            , button (Style.baseButton ++ [ HE.onClick (config.seekToStart section.id), HA.title "再生位置をこのセクションの先頭へ移動" ]) [ text "⏱ 先頭へ" ]
            , Style.divider
            , if pendingDeleteId == Just section.id then
                button
                    (Style.armedDangerButton
                        ++ [ HE.onClick (config.remove section.id)
                           , HA.attribute "aria-label" "セクションを本当に削除"
                           , HA.style "min-width" "6rem"
                           , HA.style "text-align" "center"
                           ]
                    )
                    [ text "本当に削除？" ]

              else
                button
                    (Style.dangerButton
                        ++ [ HE.onClick (config.remove section.id)
                           , HA.attribute "aria-label" "セクションを削除"
                           , HA.style "min-width" "6rem"
                           , HA.style "text-align" "center"
                           ]
                    )
                    [ text "✕ 削除" ]
            ]
        , div [ HA.style "display" "flex", HA.style "gap" "0.5rem", HA.style "align-items" "center", HA.style "margin-top" "0.3rem" ]
            [ span [ HA.style "font-size" "0.85rem" ] [ text "キー:" ]
            , Html.select [ HE.onInput (config.changeKey section.id) ]
                (Data.Key.tonicOptions
                    |> List.map
                        (\( tonic, label_ ) ->
                            Html.option [ HA.value (String.fromInt tonic), HA.selected (section.key.tonic == tonic) ] [ text label_ ]
                        )
                )
            , Html.select [ HE.onInput (config.changeMode section.id) ]
                [ Html.option [ HA.value "major", HA.selected (section.key.mode == Data.Key.Major) ] [ text "Major" ]
                , Html.option [ HA.value "minor", HA.selected (section.key.mode == Data.Key.Minor) ] [ text "Minor" ]
                ]
            , span [ HA.style "font-size" "0.85rem", HA.style "margin-left" "0.5rem" ] [ text "拍子:" ]
            , Html.select [ HE.onInput (config.changeMeter section.id) ]
                (Data.Meter.options
                    |> List.map
                        (\m ->
                            Html.option [ HA.value (Data.Meter.toString m), HA.selected (m == section.meter) ] [ text (Data.Meter.toString m) ]
                        )
                )
            , span [ HA.style "font-size" "0.85rem", HA.style "margin-left" "0.5rem" ] [ text "移調:" ]
            , button (Style.baseButton ++ [ HE.onClick (config.transpose section.id -12), HA.title "このセクションを1オクターブ下げる" ]) [ text "-12" ]
            , button (Style.baseButton ++ [ HE.onClick (config.transpose section.id -1), HA.title "このセクションを半音下げる" ]) [ text "-1" ]
            , button (Style.baseButton ++ [ HE.onClick (config.transpose section.id 1), HA.title "このセクションを半音上げる" ]) [ text "+1" ]
            , button (Style.baseButton ++ [ HE.onClick (config.transpose section.id 12), HA.title "このセクションを1オクターブ上げる" ]) [ text "+12" ]
            ]
        , div [ HA.style "display" "flex", HA.style "gap" "0.5rem", HA.style "align-items" "center", HA.style "margin-top" "0.3rem" ]
            [ span [ HA.style "font-size" "0.85rem" ] [ text "小節挿入/削除:" ]
            , input
                [ HA.type_ "number"
                , HA.value insertCountInput
                , HE.onInput config.changedInsertCount
                , HA.style "width" "3.5rem"
                ]
                []
            , button
                (Style.baseButton
                    ++ [ HE.onClick (config.insertBefore section.id)
                       , HA.title "このセクションの前に無音を指定小節数挿入する（コード進行の改行は崩れることがあります）"
                       ]
                )
                [ text "+ 挿入" ]
            , button
                (Style.dangerButton
                    ++ [ HE.onClick (config.removeFromStart section.id)
                       , HA.title "このセクションの先頭から指定小節数を削除する（コード進行の改行は崩れることがあります）"
                       , HA.attribute "aria-label" "先頭から小節を削除"
                       ]
                )
                [ text "✂ 小節削除" ]
            ]
        , textarea
            [ HA.value section.memo
            , HE.onInput (config.changeMemo section.id)
            , HA.placeholder "メモ：「ここは切なく」「サビは転調したい」など"
            , HA.style "width" "98%"
            , HA.style "margin-top" "0.4rem"
            , HA.style "min-height" "3rem"
            , HA.style "font-family" "inherit"
            ]
            []
        ]
