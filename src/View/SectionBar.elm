module View.SectionBar exposing (Config, view)

import Data.Key
import Data.Meter
import Data.Section exposing (Section)
import Html exposing (Html, button, div, input, span, text, textarea)
import Html.Attributes as HA
import Html.Events as HE
import View.Palette as Palette


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
    }


view : Config msg -> Maybe Int -> String -> List Section -> Html msg
view config selectedId insertCountInput sections =
    div [ HA.style "margin-top" "1rem" ]
        [ div
            [ HA.style "display" "flex"
            , HA.style "gap" "2px"
            , HA.style "align-items" "stretch"
            , HA.style "flex-wrap" "wrap"
            ]
            (List.indexedMap (blockView config selectedId) sections
                ++ [ button [ HE.onClick config.add ] [ text "+ セクション" ] ]
            )
        , case selectedId |> Maybe.andThen (\sid -> sections |> List.filter (\s -> s.id == sid) |> List.head) of
            Just section ->
                editPanel config insertCountInput section

            Nothing ->
                text ""
        ]


blockView : Config msg -> Maybe Int -> Int -> Section -> Html msg
blockView config selectedId idx section =
    let
        selected =
            selectedId == Just section.id
    in
    div
        [ HA.style "width" (String.fromInt (section.lengthBars * 40) ++ "px")
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
        , HA.style "cursor" "pointer"
        , HA.style "font-size" "0.85rem"
        , HA.style "overflow" "hidden"
        , HA.style "white-space" "nowrap"
        , HE.onClick (config.select section.id)
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
        ]


editPanel : Config msg -> String -> Section -> Html msg
editPanel config insertCountInput section =
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
                , HE.onInput (config.changeBars section.id)
                , HA.style "width" "3.5rem"
                ]
                []
            , button [ HE.onClick (config.move section.id (negate 1)), HA.title "左へ移動" ] [ text "←" ]
            , button [ HE.onClick (config.move section.id 1), HA.title "右へ移動" ] [ text "→" ]
            , button [ HE.onClick (config.remove section.id) ] [ text "✕ 削除" ]
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
                [ HE.onClick (config.insertBefore section.id)
                , HA.title "このセクションの前に無音を指定小節数挿入する（コード進行の改行は崩れることがあります）"
                ]
                [ text "▸ 挿入" ]
            , button
                [ HE.onClick (config.removeFromStart section.id)
                , HA.title "このセクションの先頭から指定小節数を削除する（コード進行の改行は崩れることがあります）"
                ]
                [ text "✂ 削除" ]
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
