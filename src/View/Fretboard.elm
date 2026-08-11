module View.Fretboard exposing (Config, HoverConfig, formData, view, viewReadOnly)

import Data.Chord.Format as Format
import Data.GuitarForm as GuitarForm
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import Set exposing (Set)
import View.Style as Style
import View.Theme as Theme


rowHeight : Int
rowHeight =
    20


cellWidth : Int
cellWidth =
    24


nutWidth : Int
nutWidth =
    28


openLabelWidth : Int
openLabelWidth =
    28


fretCount : Int
fretCount =
    20


fretMarkers : List Int
fretMarkers =
    [ 3, 5, 7, 9, 12 ]


colorReachable : String
colorReachable =
    Theme.outline


type alias Config msg =
    { pressedFret : Int -> Int -> msg -- 弦インデックス, フレット
    , doubleClickedFret : Int -> Int -> msg
    }


{-| ホバー時のコールバック。`pitch` は絶対ピッチ（音名表示用）、`interval` は `modBy 12 (pitch - rootPitch)`（度数表示用。
呼び出し側は rootPitch を知らなくてもよいよう Fretboard 側で計算して渡す）、`x`/`y` はマウスの clientX/clientY。
全セル（未選択も含む）で常時有効にし、クリック/ダブルクリックとは独立に扱う（config のように Maybe で無効化できない）。
-}
type alias HoverConfig msg =
    { hoveredFret : { pitch : Int, interval : Int, x : Float, y : Float } -> msg
    , unhoveredFret : msg
    }


pitchName : Int -> String
pitchName pitch =
    Format.pitchName False pitch ++ String.fromInt (pitch // 12 - 1)


{-| Form をこのモジュールの view/viewReadOnly に渡す入力に変換する。rootPitch は呼び出し側が
（通常 `Data.Voicing.anchorPitch + modBy 12 chord.root` で）求める。`View.ChordEditor.formToFretboardData`
と `Data.StrumExpand.voicingFromForm` の両方がこれと同じロジックを使う。
-}
formData : Int -> GuitarForm.Form -> { rootPitch : Int, selected : Set Int, picks : GuitarForm.StringPicks }
formData rootPitch form =
    { rootPitch = rootPitch
    , selected = Set.fromList (GuitarForm.toPitches form)
    , picks =
        List.map2 Tuple.pair GuitarForm.openStrings form.frets
            |> List.indexedMap (\i ( openPitch, maybeFret ) -> Maybe.map (\fret -> ( openPitch + fret - rootPitch, i )) maybeFret)
            |> List.filterMap identity
            |> Set.fromList
    }


{-| ボイシングの選択中ピッチ集合をクリック可能な指板図として描画する。`View.VoicingKeyboard` と対称な
`{ rootPitch, selected }` に加えて、「どの弦・フレットで実際に押したか」の一時的な運指メモ `picks` を受け取る。
同じ音に到達できる位置のうち、`picks` に入っている位置だけを青丸で、他を灰丸で描画する。
弦は高音側（最後の要素）が上に来るように反転して描画する（TAB 譜・鍵盤の上下と向きを揃えるため）。
-}
view : Config msg -> HoverConfig msg -> { rootPitch : Int, selected : Set Int, picks : GuitarForm.StringPicks } -> Html msg
view config hover sel =
    viewInternal (Just config) hover sel


{-| クリック不可の読み取り専用表示。config を Nothing にした viewInternal を呼ぶだけ。
「今のコードの運指を見せるだけ」の用途（コード進行サイドバーの現在コード表示、FormPicker の候補カード）に使う。
読み取り専用でもホバー（hover）は常時有効。
-}
viewReadOnly : HoverConfig msg -> { rootPitch : Int, selected : Set Int, picks : GuitarForm.StringPicks } -> Html msg
viewReadOnly =
    viewInternal Nothing


viewInternal : Maybe (Config msg) -> HoverConfig msg -> { rootPitch : Int, selected : Set Int, picks : GuitarForm.StringPicks } -> Html msg
viewInternal config hover { rootPitch, selected, picks } =
    let
        indexedStrings =
            List.indexedMap Tuple.pair GuitarForm.openStrings

        displayStrings =
            List.reverse indexedStrings
    in
    div
        [ HA.style "margin-top" "0.4rem"
        , HA.style "border" ("1px solid " ++ Theme.outlineVariant)
        , HA.style "padding" "0.4rem"
        , HA.style "display" "inline-block"
        ]
        (List.map (stringRow config hover rootPitch selected picks) displayStrings
            ++ [ markerRow, numberRow ]
        )


stringRow : Maybe (Config msg) -> HoverConfig msg -> Int -> Set Int -> GuitarForm.StringPicks -> ( Int, Int ) -> Html msg
stringRow config hover rootPitch selected picks ( stringIndex, openPitch ) =
    div [ HA.style "display" "flex" ]
        (openLabelCell openPitch
            :: List.map (fretCell config hover rootPitch selected picks stringIndex openPitch) (List.range 0 fretCount)
        )


openLabelCell : Int -> Html msg
openLabelCell openPitch =
    div
        [ HA.style "width" (String.fromInt openLabelWidth ++ "px")
        , HA.style "height" (String.fromInt rowHeight ++ "px")
        , HA.style "box-sizing" "border-box"
        , HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "flex-end"
        , HA.style "padding-right" "4px"
        , HA.style "font-size" "10px"
        , HA.style "color" Theme.onSurfaceVariant
        , HA.style "user-select" "none"
        ]
        [ text (pitchName openPitch) ]


fretCell : Maybe (Config msg) -> HoverConfig msg -> Int -> Set Int -> GuitarForm.StringPicks -> Int -> Int -> Int -> Html msg
fretCell config hover rootPitch selected picks stringIndex openPitch column =
    let
        pitch =
            openPitch + column

        isSelected =
            Set.member pitch selected

        isPicked =
            Set.member ( pitch - rootPitch, stringIndex ) picks

        isNut =
            column == 0

        isBelowRoot =
            pitch < rootPitch && not isSelected

        interactiveAttrs =
            case config of
                Just c ->
                    [ HA.class "m3-btn"
                    , HA.style "cursor" "pointer"
                    , HE.onClick (c.pressedFret stringIndex column)
                    , HE.onDoubleClick (c.doubleClickedFret stringIndex column)
                    ]

                Nothing ->
                    []

        hoverAttrs =
            [ HE.on "mouseover"
                (Decode.map
                    (\pos -> hover.hoveredFret { pitch = pitch, interval = modBy 12 (pitch - rootPitch), x = pos.clientX, y = pos.clientY })
                    fretHoverDecoder
                )
            , HE.on "mouseout" (Decode.succeed hover.unhoveredFret)
            ]
    in
    div
        ([ HA.style "width"
            (String.fromInt
                (if isNut then
                    nutWidth

                 else
                    cellWidth
                )
                ++ "px"
            )
         , HA.style "height" (String.fromInt rowHeight ++ "px")
         , HA.style "box-sizing" "border-box"
         , HA.style "border-right"
            (if isNut then
                "3px solid " ++ Theme.onSurface

             else
                "1px solid " ++ Theme.outline
            )
         , HA.style "opacity"
            (if isBelowRoot then
                "0.35"

             else
                "1"
            )
         , HA.style "position" "relative"
         , HA.style "display" "flex"
         , HA.style "align-items" "center"
         , HA.style "justify-content" "center"
         , HA.style "user-select" "none"
         ]
            ++ interactiveAttrs
            ++ hoverAttrs
        )
        [ div
            [ HA.style "position" "absolute"
            , HA.style "left" "0"
            , HA.style "right" "0"
            , HA.style "top" "50%"
            , HA.style "height" "1px"
            , HA.style "background" Theme.outlineVariant
            , HA.style "pointer-events" "none"
            ]
            []
        , if isSelected then
            div
                [ HA.style "width" "16px"
                , HA.style "height" "16px"
                , HA.style "border-radius" "50%"
                , HA.style "background"
                    (if isPicked then
                        Style.colorPrimary

                     else
                        colorReachable
                    )
                , HA.style "position" "relative"
                , HA.style "display" "flex"
                , HA.style "align-items" "center"
                , HA.style "justify-content" "center"
                , HA.style "font-size" "8px"
                , HA.style "font-weight" "600"
                , HA.style "color" Theme.onPrimary
                , HA.style "line-height" "1"
                ]
                [ text (Format.degreeLabel (modBy 12 (pitch - rootPitch))) ]

          else
            text ""
        ]


{-| `View.PianoRoll` の `noteHoverDecoder` と同型。マウスイベントから clientX/clientY だけを取り出す。
-}
fretHoverDecoder : Decode.Decoder { clientX : Float, clientY : Float }
fretHoverDecoder =
    Decode.map2 (\cx cy -> { clientX = cx, clientY = cy })
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


markerRow : Html msg
markerRow =
    div [ HA.style "display" "flex" ]
        (div [ HA.style "width" (String.fromInt (openLabelWidth + nutWidth) ++ "px") ] []
            :: List.map markerCell (List.range 1 fretCount)
        )


markerCell : Int -> Html msg
markerCell column =
    div
        [ HA.style "width" (String.fromInt cellWidth ++ "px")
        , HA.style "height" "8px"
        , HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HA.style "font-size" "8px"
        , HA.style "color" Theme.onSurfaceVariant
        ]
        [ if List.member column fretMarkers then
            text "•"

          else
            text ""
        ]


numberRow : Html msg
numberRow =
    div [ HA.style "display" "flex" ]
        (div [ HA.style "width" (String.fromInt openLabelWidth ++ "px") ] []
            :: numberCell True 0
            :: List.map (numberCell False) (List.range 1 fretCount)
        )


numberCell : Bool -> Int -> Html msg
numberCell isNut column =
    div
        [ HA.style "width"
            (String.fromInt
                (if isNut then
                    nutWidth

                 else
                    cellWidth
                )
                ++ "px"
            )
        , HA.style "height" "12px"
        , HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HA.style "font-size" "8px"
        , HA.style "color"
            (if List.member column fretMarkers then
                Theme.onSurfaceVariant

             else
                Theme.outlineVariant
            )
        ]
        [ text (String.fromInt column) ]
