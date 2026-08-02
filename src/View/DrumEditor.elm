module View.DrumEditor exposing (Config, view)

import Data.DrumPattern
import Data.Note exposing (Note)
import Data.Time
import Html exposing (Html, button, div, option, select, span, text)
import Html.Attributes as HA
import Html.Events
import Json.Decode as Decode
import Svg
import Svg.Attributes as SA


type alias Config msg =
    { toggledCell : Int -> Int -> msg
    , appliedPreset : String -> msg
    , changedFillBars : String -> msg
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


cellWidth : Int
cellWidth =
    20


rowHeight : Int
rowHeight =
    22


totalSteps : Int -> Int
totalSteps totalBars =
    totalBars * 16


gridWidth : Int -> Int
gridWidth totalBars =
    totalSteps totalBars * cellWidth


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


view : Config msg -> Int -> Int -> List Note -> Int -> Html msg
view config totalBars fillBars notes playheadTicks =
    div [ HA.style "margin-top" "1rem" ]
        [ div [ HA.style "display" "flex", HA.style "gap" "0.4rem", HA.style "align-items" "center", HA.style "flex-wrap" "wrap" ]
            (span [ HA.style "font-size" "0.85rem" ] [ text "プリセット（選択セクションがあればその範囲、なければ先頭から右の長さ）: " ]
                :: List.map
                    (\pattern ->
                        button [ Html.Events.onClick (config.appliedPreset pattern.name) ] [ text pattern.name ]
                    )
                    Data.DrumPattern.patterns
                ++ [ span [ HA.style "font-size" "0.85rem" ] [ text " 長さ:" ]
                   , select [ Html.Events.onInput config.changedFillBars ]
                        (List.map
                            (\n ->
                                option
                                    [ HA.value (String.fromInt n)
                                    , HA.selected (n == fillBars)
                                    ]
                                    [ text (String.fromInt n ++ "小節") ]
                            )
                            [ 1, 2, 4, 8, 16, 32, 64 ]
                        )
                   ]
            )
        , div [ HA.style "display" "flex", HA.style "margin-top" "0.4rem" ]
            [ labelColumn
            , gridView config totalBars notes playheadTicks
            ]
        ]


labelColumn : Html msg
labelColumn =
    div [ HA.style "flex" "0 0 90px" ]
        (List.map
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
        )


gridView : Config msg -> Int -> List Note -> Int -> Html msg
gridView config totalBars notes playheadTicks =
    div
        [ HA.style "overflow-x" "auto"
        , HA.style "border" "1px solid #ccc"
        ]
        [ Svg.svg
            [ SA.width (String.fromInt (gridWidth totalBars))
            , SA.height (String.fromInt gridHeight)
            , SA.viewBox ("0 0 " ++ String.fromInt (gridWidth totalBars) ++ " " ++ String.fromInt gridHeight)
            , HA.style "display" "block"
            , HA.style "cursor" "pointer"
            , Html.Events.on "mousedown"
                (Decode.map2
                    (\ox oy ->
                        let
                            step =
                                floor (ox / toFloat cellWidth)

                            rowIdx =
                                floor (oy / toFloat rowHeight)

                            pitch =
                                rows
                                    |> List.drop rowIdx
                                    |> List.head
                                    |> Maybe.map Tuple.first
                                    |> Maybe.withDefault 36
                        in
                        config.toggledCell pitch (step * Data.Time.ticksPerSixteenth)
                    )
                    (Decode.field "offsetX" Decode.float)
                    (Decode.field "offsetY" Decode.float)
                )
            ]
            (backgroundRows totalBars ++ verticalLines totalBars ++ List.filterMap activeCell notes ++ playheadView playheadTicks)
        ]


backgroundRows : Int -> List (Svg.Svg msg)
backgroundRows totalBars =
    List.indexedMap
        (\i _ ->
            Svg.rect
                [ SA.x "0"
                , SA.y (String.fromInt (i * rowHeight))
                , SA.width (String.fromInt (gridWidth totalBars))
                , SA.height (String.fromInt rowHeight)
                , SA.fill
                    (if modBy 2 i == 0 then
                        "#fdfdfd"

                     else
                        "#f3f3f3"
                    )
                , SA.stroke "#e5e5e5"
                , SA.strokeWidth "0.5"
                ]
                []
        )
        rows


verticalLines : Int -> List (Svg.Svg msg)
verticalLines totalBars =
    List.range 0 (totalSteps totalBars)
        |> List.map
            (\i ->
                Svg.line
                    [ SA.x1 (String.fromInt (i * cellWidth))
                    , SA.y1 "0"
                    , SA.x2 (String.fromInt (i * cellWidth))
                    , SA.y2 (String.fromInt gridHeight)
                    , SA.stroke
                        (if modBy 16 i == 0 then
                            "#999"

                         else if modBy 4 i == 0 then
                            "#ccc"

                         else
                            "#eee"
                        )
                    ]
                    []
            )


activeCell : Note -> Maybe (Svg.Svg msg)
activeCell note =
    rowIndexOf note.pitch
        |> Maybe.map
            (\rowIdx ->
                Svg.rect
                    [ SA.x (String.fromInt (note.start // Data.Time.ticksPerSixteenth * cellWidth + 2))
                    , SA.y (String.fromInt (rowIdx * rowHeight + 3))
                    , SA.width (String.fromInt (cellWidth - 4))
                    , SA.height (String.fromInt (rowHeight - 6))
                    , SA.rx "3"
                    , SA.fill "#e67e22"
                    ]
                    []
            )


playheadView : Int -> List (Svg.Svg msg)
playheadView ticks =
    [ Svg.line
        [ SA.x1 (String.fromFloat (toFloat ticks * toFloat cellWidth / toFloat Data.Time.ticksPerSixteenth))
        , SA.y1 "0"
        , SA.x2 (String.fromFloat (toFloat ticks * toFloat cellWidth / toFloat Data.Time.ticksPerSixteenth))
        , SA.y2 (String.fromInt gridHeight)
        , SA.stroke "#e74c3c"
        , SA.strokeWidth "2"
        ]
        []
    ]
