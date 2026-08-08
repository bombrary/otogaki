module View.SectionBar exposing (Config, regionPxPerBar, sectionDragTargetIndex, view)

import Data.Key
import Data.Meter
import Data.Section exposing (Section)
import Html exposing (Html, button, div, input, span, text, textarea)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import View.Palette as Palette
import View.Style as Style


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
    }


{-| セクション行（リージョン行）の横幅スケール。ピアノロールの `pxPerSixteenth`とは意図的に別々
（セクション行とピアノロールは現状別々のスクロール領域なので、合わせても画面上で位置が整合しない）。
-}
regionPxPerBar : Int
regionPxPerBar =
    40


{-| ドラッグ中の累積 dx から、進行方向の隣接セクション幅の半分を超えたら入れ替え先 index と、次の判定に
持ち越す分の dx を返す。超えていなければ `Nothing`（まだ入れ替えない）。
古典的なソータブルリストの「隣の中間点を超えたら入れ替える」しきい値判定。
-}
sectionDragTargetIndex : List Section -> Int -> Float -> Maybe ( Int, Float )
sectionDragTargetIndex sections currentIndex accumDx =
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
                        toFloat (neighbor.lengthBars * regionPxPerBar)
                in
                if abs accumDx >= widthPx / 2 then
                    Just ( currentIndex + dir, accumDx - toFloat dir * widthPx )

                else
                    Nothing


view : Config msg -> Maybe Int -> String -> List Section -> Maybe Int -> Maybe { sectionId : Int, lengthBars : Int } -> Html msg
view config selectedId insertCountInput sections pendingDeleteId resizePreview =
    div [ HA.style "margin-top" "1rem" ]
        [ div
            [ HA.style "display" "flex"
            , HA.style "gap" "2px"
            , HA.style "align-items" "stretch"
            , HA.style "flex-wrap" "wrap"
            ]
            (List.indexedMap (blockView config selectedId resizePreview) sections
                ++ [ button (Style.baseButton ++ [ HE.onClick config.add ]) [ text "+ セクション" ] ]
            )
        , case selectedId |> Maybe.andThen (\sid -> sections |> List.filter (\s -> s.id == sid) |> List.head) of
            Just section ->
                editPanel config insertCountInput section pendingDeleteId

            Nothing ->
                text ""
        ]


blockView : Config msg -> Maybe Int -> Maybe { sectionId : Int, lengthBars : Int } -> Int -> Section -> Html msg
blockView config selectedId resizePreview idx section =
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
    in
    div
        [ HA.style "width" (String.fromInt (displayLengthBars * regionPxPerBar) ++ "px")
        , HA.style "padding" "0.3rem"
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
        , HA.style "position" "relative"
        , HA.style "cursor" "grab"
        , HA.style "font-size" "0.85rem"
        , HA.style "overflow" "hidden"
        , HA.style "white-space" "nowrap"
        , HE.on "mousedown" (Decode.map (config.pressedBlock section.id) (Decode.field "clientX" Decode.float))
        , HA.title
            (if section.memo == "" then
                section.name

             else
                section.name ++ ": " ++ section.memo
            )
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
