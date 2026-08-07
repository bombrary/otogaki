module View.Fretboard exposing (Config, view)

import Data.GuitarForm as GuitarForm
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events as HE
import Set exposing (Set)
import View.Style as Style


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
    "#bbb"


type alias Config msg =
    { pressedFret : Int -> Int -> msg -- 弦インデックス, フレット
    , doubleClickedFret : Int -> Int -> msg
    }


noteNames : List String
noteNames =
    [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]


pitchName : Int -> String
pitchName pitch =
    let
        name =
            List.drop (modBy 12 pitch) noteNames |> List.head |> Maybe.withDefault "?"
    in
    name ++ String.fromInt (pitch // 12 - 1)


{-| ボイシングの選択中ピッチ集合をクリック可能な指板図として描画する。`View.VoicingKeyboard` と対称な
`{ rootPitch, selected }` に加えて、「どの弦・フレットで実際に押したか」の一時的な運指メモ `picks` を受け取る。
同じ音に到達できる位置のうち、`picks` に入っている位置だけを青丸で、他を灰丸で描画する。
弦は高音側（最後の要素）が上に来るように反転して描画する（TAB 譜・鍵盤の上下と向きを揃えるため）。
-}
view : Config msg -> { rootPitch : Int, selected : Set Int, picks : GuitarForm.StringPicks } -> Html msg
view config { rootPitch, selected, picks } =
    let
        indexedStrings =
            List.indexedMap Tuple.pair GuitarForm.openStrings

        displayStrings =
            List.reverse indexedStrings
    in
    div
        [ HA.style "margin-top" "0.4rem"
        , HA.style "border" "1px solid #ccc"
        , HA.style "padding" "0.4rem"
        , HA.style "display" "inline-block"
        ]
        (List.map (stringRow config rootPitch selected picks) displayStrings
            ++ [ markerRow, numberRow ]
        )


stringRow : Config msg -> Int -> Set Int -> GuitarForm.StringPicks -> ( Int, Int ) -> Html msg
stringRow config rootPitch selected picks ( stringIndex, openPitch ) =
    div [ HA.style "display" "flex" ]
        (openLabelCell openPitch
            :: List.map (fretCell config rootPitch selected picks stringIndex openPitch) (List.range 0 fretCount)
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
        , HA.style "color" "#666"
        , HA.style "user-select" "none"
        ]
        [ text (pitchName openPitch) ]


fretCell : Config msg -> Int -> Set Int -> GuitarForm.StringPicks -> Int -> Int -> Int -> Html msg
fretCell config rootPitch selected picks stringIndex openPitch column =
    let
        pitch =
            openPitch + column

        isSelected =
            Set.member pitch selected

        isPicked =
            Set.member ( pitch - rootPitch, stringIndex ) picks

        isNut =
            column == 0
    in
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
        , HA.style "height" (String.fromInt rowHeight ++ "px")
        , HA.style "box-sizing" "border-box"
        , HA.style "border-right"
            (if isNut then
                "3px solid #333"

             else
                "1px solid #999"
            )
        , HA.style "border-bottom" "1px solid #ddd"
        , HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HA.style "cursor" "pointer"
        , HA.style "user-select" "none"
        , HE.onClick (config.pressedFret stringIndex column)
        , HE.onDoubleClick (config.doubleClickedFret stringIndex column)
        ]
        [ if isSelected then
            div
                [ HA.style "width" "12px"
                , HA.style "height" "12px"
                , HA.style "border-radius" "50%"
                , HA.style "background"
                    (if isPicked then
                        Style.colorPrimary

                     else
                        colorReachable
                    )
                ]
                []

          else
            text ""
        ]


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
        , HA.style "color" "#aaa"
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
                "#888"

             else
                "#ccc"
            )
        ]
        [ text (String.fromInt column) ]
