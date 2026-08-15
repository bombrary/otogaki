module View.VoicingKeyboard exposing (Config, HoverConfig, containerHeight, maxPitch, minPitch, rowHeight, scrollId, view)

import Data.Chord.Format as Format
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import Set exposing (Set)
import View.Style as Style
import View.Theme as Theme


type alias Config msg =
    { pressedOffset : Int -> { clientX : Float, clientY : Float, shift : Bool, isTouch : Bool, timeStamp : Float } -> msg
    , doubleClickedOffset : Int -> msg
    , pressedKey : Int -> msg
    , draggedWhilePressingOffset : { clientX : Float, clientY : Float, alt : Bool } -> msg
    , releasedOffsetPress : msg
    }


{-| ホバー時のコールバック。`View.Fretboard.HoverConfig` と完全に同じ型（フィールド名だけ違う）。
Main の `hoveredFretCell` / `HoveredFretCell` / `UnhoveredFretCell` をそのまま流用できるよう、
呼び出し側（ChordEditor）で `{ hoveredKey = config.hoveredFretCell, unhoveredKey = config.unhoveredFretCell }`
のように詰め替えるだけで済む。
-}
type alias HoverConfig msg =
    { hoveredKey : { pitch : Int, interval : Int, x : Float, y : Float } -> msg
    , unhoveredKey : msg
    }


{-| ピアノロール（`View.PianoRoll`）とは別の下限。Voicing.offsets の下限（-12）を含むように
`anchorPitch`（36）- 12 まで下げてある（転回形・ハイブリッドコードで root より低いベース音も鍵盤に表示できるようにするため）。
-}
minPitch : Int
minPitch =
    24


maxPitch : Int
maxPitch =
    84


rowHeight : Int
rowHeight =
    18


containerHeight : Int
containerHeight =
    336


keyColumnWidth : Int
keyColumnWidth =
    44


laneWidth : Int
laneWidth =
    120


{-| 開いた直後に root 行までスクロールさせるための DOM id。同時に開くのは 1 行だけなので固定値でよい。
-}
scrollId : String
scrollId =
    "voicing-keyboard-scroll"


isWhite : Int -> Bool
isWhite pitch =
    List.member (modBy 12 pitch) [ 0, 2, 4, 5, 7, 9, 11 ]


{-| 表示上の root（displayRootPitch）からの相対半音。実際に置かれている最低音が基準なので常に 0 以上になる。
-}
offsetLabel : Int -> Int -> String
offsetLabel displayRootPitch pitch =
    Format.degreeLabelExtended (pitch - displayRootPitch)


{-| mousedown 用デコーダ。button フィルタを必ず入れる: これがないと右クリック（contextmenu で削除する
想定）でも mousedown が先に発火し、「追加してから削除」→ドラッグ状態が残って直後のマウス移動で
削除したノートが復活する、というバグになる。
-}
pressDecoder : Decode.Decoder { clientX : Float, clientY : Float, shift : Bool, isTouch : Bool, timeStamp : Float }
pressDecoder =
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


{-| `View.Fretboard` の `fretHoverDecoder`、`View.PianoRoll` の `noteHoverDecoder` と同型。
マウスイベントから clientX/clientY だけを取り出す。
-}
keyHoverDecoder : Decode.Decoder { clientX : Float, clientY : Float }
keyHoverDecoder =
    Decode.map2 (\cx cy -> { clientX = cx, clientY = cy })
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


{-| レーン行を押している間のpointermove用。buttonsガードでホバーのみの発火を防ぎ、altKeyも同時に読む。
PianoRoll.elm の noteMoveDecoder と同型。
-}
laneMoveDecoder : Decode.Decoder { clientX : Float, clientY : Float, alt : Bool }
laneMoveDecoder =
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


view : Config msg -> HoverConfig msg -> { rootPitch : Int, displayRootPitch : Int, placed : Set Int, selectedPitches : Set Int } -> Html msg
view config hover { rootPitch, displayRootPitch, placed, selectedPitches } =
    let
        pitches =
            List.range minPitch maxPitch |> List.reverse
    in
    div
        [ HA.id scrollId
        , HA.style "height" (String.fromInt containerHeight ++ "px")
        , HA.style "overflow-y" "auto"
        , HA.style "border" ("1px solid " ++ Theme.outlineVariant)
        , HA.style "margin-top" "0.4rem"
        , HA.style "flex-shrink" "0"
        ]
        [ div [ HA.style "display" "flex" ]
            [ div [ HA.style "flex" ("0 0 " ++ String.fromInt keyColumnWidth ++ "px") ]
                (List.map (keyRow config displayRootPitch rootPitch) pitches)
            , div [ HA.style "flex" ("0 0 " ++ String.fromInt laneWidth ++ "px"), HA.style "position" "relative" ]
                (List.map (laneRow config hover displayRootPitch rootPitch placed selectedPitches) pitches)
            ]
        ]


keyRow : Config msg -> Int -> Int -> Int -> Html msg
keyRow config displayRootPitch rootPitch pitch =
    let
        isBlack =
            not (isWhite pitch)

        isRoot =
            pitch == displayRootPitch

        isBelowRoot =
            pitch < rootPitch

        label =
            if modBy 12 pitch == 0 then
                "C" ++ String.fromInt (pitch // 12 - 1)

            else
                ""
    in
    div
        [ HA.style "height" (String.fromInt rowHeight ++ "px")
        , HA.style "box-sizing" "border-box"
        , HA.style "background"
            (if isRoot then
                Theme.primaryContainer

             else if isBlack then
                Theme.onSurface

             else
                Theme.surfaceContainerLowest
            )
        , HA.style "color"
            (if isRoot then
                Theme.onPrimaryContainer

             else if isBlack then
                Theme.surfaceContainerHighest

             else
                Theme.onSurfaceVariant
            )
        , HA.style "opacity"
            (if isBelowRoot then
                "0.35"

             else
                "1"
            )
        , HA.style "border-bottom" ("1px solid " ++ Theme.outlineVariant)
        , HA.style "font-size" "9px"
        , HA.style "line-height" (String.fromInt (rowHeight - 1) ++ "px")
        , HA.style "text-align" "right"
        , HA.style "padding-right" "3px"
        , HA.style "user-select" "none"
        , HA.style "cursor" "pointer"
        , HA.style "touch-action" "none"
        , HE.on "pointerdown" (Decode.succeed (config.pressedKey pitch))
        ]
        [ text label ]


laneRow : Config msg -> HoverConfig msg -> Int -> Int -> Set Int -> Set Int -> Int -> Html msg
laneRow config hover displayRootPitch rootPitch placed selectedPitches pitch =
    let
        isPlaced =
            Set.member pitch placed

        isSelected =
            Set.member pitch selectedPitches && isPlaced

        isRoot =
            pitch == displayRootPitch

        isBelowRoot =
            pitch < rootPitch && not isPlaced
    in
    div
        [ HA.style "height" (String.fromInt rowHeight ++ "px")
        , HA.style "box-sizing" "border-box"
        , HA.style "background"
            (if isRoot then
                Theme.primaryContainer

             else if isWhite pitch then
                Theme.surfaceContainerLowest

             else
                Theme.surfaceContainerHigh
            )
        , HA.style "opacity"
            (if isBelowRoot then
                "0.35"

             else
                "1"
            )
        , HA.style "border-bottom" ("1px solid " ++ Theme.surfaceContainerHighest)
        , HA.style "font-size" "9px"
        , HA.style "line-height" (String.fromInt (rowHeight - 1) ++ "px")
        , HA.style "padding-left" "4px"
        , HA.style "color" Theme.onSurface
        , HA.style "cursor" "pointer"
        , HA.style "user-select" "none"
        , HA.style "position" "relative"
        , HA.style "touch-action" "none"
        , HA.attribute "data-pointer-capture" ""
        , HE.stopPropagationOn "pointerdown"
            (Decode.map (\pos -> ( config.pressedOffset pitch pos, True )) pressDecoder)
        , HE.stopPropagationOn "pointermove"
            (Decode.map (\pos -> ( config.draggedWhilePressingOffset pos, True )) laneMoveDecoder)
        , HE.stopPropagationOn "pointerup"
            (Decode.succeed ( config.releasedOffsetPress, True ))
        , HE.stopPropagationOn "pointercancel"
            (Decode.succeed ( config.releasedOffsetPress, True ))
        , HE.stopPropagationOn "dblclick"
            (Decode.succeed ( config.doubleClickedOffset pitch, True ))
        , HE.preventDefaultOn "contextmenu"
            (Decode.succeed ( config.doubleClickedOffset pitch, True ))
        , HE.on "mouseover"
            (Decode.map
                (\pos -> hover.hoveredKey { pitch = pitch, interval = modBy 12 (pitch - rootPitch), x = pos.clientX, y = pos.clientY })
                keyHoverDecoder
            )
        , HE.on "mouseout" (Decode.succeed hover.unhoveredKey)
        ]
        [ if isPlaced then
            div
                [ HA.style "position" "absolute"
                , HA.style "left" "1px"
                , HA.style "right" "1px"
                , HA.style "top" "1px"
                , HA.style "bottom" "1px"
                , HA.style "box-sizing" "border-box"
                , HA.style "border-radius" "2px"
                , HA.style "background"
                    (if isSelected then
                        Style.colorSelection

                     else
                        Theme.primary
                    )
                , HA.style "border"
                    (if isSelected then
                        "1px solid " ++ Theme.selectionDeep

                     else
                        "none"
                    )
                , HA.style "color" Theme.onPrimary
                , HA.style "font-size" "9px"
                , HA.style "display" "flex"
                , HA.style "align-items" "center"
                , HA.style "padding-left" "4px"
                , HA.style "overflow" "hidden"
                , HA.style "white-space" "nowrap"
                , HA.style "pointer-events" "none"
                ]
                [ text (offsetLabel displayRootPitch pitch) ]

          else
            text ""
        ]
