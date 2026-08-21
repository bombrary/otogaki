module View.ScrapShelf exposing (Config, view)

import Data.Help as Help
import Data.Note exposing (Note)
import Data.Scrap exposing (Scrap)
import Data.Time
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Svg
import Svg.Attributes as SA
import View.Style as Style
import View.Theme as Theme


type alias Config msg =
    { addFromSelection : msg
    , place : Int -> msg
    , remove : Int -> msg
    , rename : Int -> String -> msg
    , preview : Int -> msg
    , openedHelp : Help.TopicId -> msg
    }


view : Config msg -> Int -> List Scrap -> Maybe Int -> Html msg
view config selectionCount scraps pendingDeleteId =
    Html.details
        [ HA.attribute "open" ""
        , HA.style "margin-top" "1rem"
        , HA.style "padding" "0.5rem"
        , HA.style "border" ("1px dashed " ++ Theme.outline)
        , HA.style "border-radius" "4px"
        ]
        [ Html.summary (HA.style "cursor" "pointer" :: Style.headingText) [ text "📋 断片棚", Style.helpButton { onClick = config.openedHelp Help.ScrapOps, label = "断片棚" } ]
        , span [ HA.style "font-size" "0.75rem", HA.style "color" Theme.onSurfaceVariant, HA.style "display" "block", HA.style "margin-top" "0.3rem" ]
            [ text "思いついたフレーズを曲に置く前にここへ貯めておける。「配置」で選択中のトラックの再生位置に置く" ]
        , div [ HA.style "display" "flex", HA.style "gap" "0.5rem", HA.style "flex-wrap" "wrap", HA.style "margin-top" "0.4rem" ]
            (List.map (scrapCard config pendingDeleteId) scraps
                ++ [ button
                        (Style.baseButton
                            ++ [ HE.onClick config.addFromSelection
                               , HA.disabled (selectionCount == 0)
                               , HA.title "ピアノロールで選択したノートを断片として保存"
                               ]
                        )
                        [ text "+ 断片" ]
                   ]
            )
        ]


scrapCard : Config msg -> Maybe Int -> Scrap -> Html msg
scrapCard config pendingDeleteId scrap =
    div
        [ HA.style "border-radius" "4px"
        , HA.style "padding" "0.3rem"
        , HA.style "background" Theme.surfaceContainerLow
        ]
        [ input
            [ HA.value scrap.name
            , HE.onInput (config.rename scrap.id)
            , HA.style "width" "8rem"
            , HA.style "font-size" "0.8rem"
            , HA.style "border" "none"
            , HA.style "background" "transparent"
            ]
            []
        , preview scrap.notes
        , div [ HA.style "display" "flex", HA.style "gap" "0.5rem", HA.style "margin-top" "0.2rem", HA.style "align-items" "center" ]
            [ button (Style.baseButton ++ [ HE.onClick (config.preview scrap.id), HA.title "断片を試し聞き" ]) [ text "▶" ]
            , button (Style.baseButton ++ [ HE.onClick (config.place scrap.id), HA.title "選択中のトラックの再生位置に配置" ]) [ text "配置" ]
            , Style.divider
            , if pendingDeleteId == Just scrap.id then
                button (Style.armedDangerButton ++ [ HE.onClick (config.remove scrap.id), HA.title "断片を削除", HA.attribute "aria-label" "断片を本当に削除" ]) [ text "本当に？" ]

              else
                button (Style.dangerButton ++ [ HE.onClick (config.remove scrap.id), HA.title "断片を削除", HA.attribute "aria-label" "断片を削除" ]) [ text "✕" ]
            ]
        ]


preview : List Note -> Html msg
preview notes =
    let
        width =
            140

        height =
            36

        endTicks =
            notes
                |> List.map (\n -> n.start + n.duration)
                |> List.maximum
                |> Maybe.withDefault Data.Time.ticksPerBar
                |> Basics.max Data.Time.ticksPerBar

        minP =
            notes |> List.map .pitch |> List.minimum |> Maybe.withDefault 48

        maxP =
            notes |> List.map .pitch |> List.maximum |> Maybe.withDefault 72

        range =
            Basics.max 1 (maxP - minP)

        noteRect n =
            Svg.rect
                [ SA.x (String.fromFloat (toFloat n.start / toFloat endTicks * toFloat width))
                , SA.y (String.fromFloat (toFloat (maxP - n.pitch) / toFloat range * toFloat (height - 4)))
                , SA.width (String.fromFloat (Basics.max 2 (toFloat n.duration / toFloat endTicks * toFloat width)))
                , SA.height "3"
                , SA.fill Theme.primary
                ]
                []
    in
    Svg.svg
        [ SA.width (String.fromInt width)
        , SA.height (String.fromInt height)
        , HA.style "display" "block"
        , HA.style "background" Theme.surfaceContainerLowest
        , HA.style "border" ("1px solid " ++ Theme.surfaceContainerHighest)
        , HA.style "margin-top" "0.2rem"
        ]
        (List.map noteRect notes)
