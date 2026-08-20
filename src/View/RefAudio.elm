module View.RefAudio exposing (Config, view)

import Data.ReferenceAudio exposing (ReferenceAudio)
import Html exposing (Html, button, div, input, label, span, text)
import Html.Attributes as HA
import Html.Events as HE
import View.Style as Style
import View.Theme as Theme


type alias Config msg =
    { changedOffset : String -> msg
    , blurredOffset : msg
    , changedVolume : String -> msg
    , toggledMute : msg
    , clearedRefAudio : msg
    }


view : Config msg -> String -> Bool -> ReferenceAudio -> Html msg
view config offsetInput loaded ra =
    Html.details
        [ HA.attribute "open" ""
        , HA.style "margin-top" "1rem"
        , HA.style "padding" "0.5rem"
        , HA.style "border" ("1px solid " ++ Theme.outlineVariant)
        , HA.style "border-radius" "4px"
        ]
        [ Html.summary (HA.style "cursor" "pointer" :: Style.headingText) [ text "🎧 参考オーディオ" ]
        , span [ HA.style "font-size" "0.75rem", HA.style "color" Theme.onSurfaceVariant, HA.style "display" "block", HA.style "margin-top" "0.3rem" ]
            [ text "耳コピ・分析用。音声データはブラウザ内（IndexedDB）に保存されるので、リロード後も自動で読み込まれるよ" ]
        , div [ HA.style "display" "flex", HA.style "gap" "0.7rem", HA.style "align-items" "center", HA.style "flex-wrap" "wrap", HA.style "margin-top" "0.4rem" ]
            [ input
                [ HA.type_ "file"
                , HA.id "ref-audio-input"
                , HA.accept "audio/*"
                ]
                []
            , statusView loaded ra
            , clearButton config ra
            , label [ HA.style "font-size" "0.85rem" ]
                [ text "オフセット(ms): "
                , input
                    [ HA.type_ "number"
                    , HA.value offsetInput
                    , HE.onInput config.changedOffset
                    , HE.onBlur config.blurredOffset
                    , HA.step "10"
                    , HA.style "width" "5.5rem"
                    , HA.title "オーディオのこの位置(ms)を小節1・拍1に合わせる。負の値ならオーディオを遅らせる"
                    ]
                    []
                ]
            , label [ HA.style "font-size" "0.85rem" ]
                [ text "音量: "
                , input
                    [ HA.type_ "range"
                    , HA.min "0"
                    , HA.max "100"
                    , HA.value (String.fromInt ra.volume)
                    , HE.onInput config.changedVolume
                    , HA.style "width" "70px"
                    , HA.style "vertical-align" "middle"
                    ]
                    []
                ]
            , button
                (Style.toggleButton ra.muted
                    ++ [ HE.onClick config.toggledMute
                       , HA.title "参考オーディオをミュート"
                       ]
                )
                [ text "M" ]
            ]
        ]


statusView : Bool -> ReferenceAudio -> Html msg
statusView loaded ra =
    case ( loaded, ra.fileName ) of
        ( True, Just name ) ->
            span [ HA.style "font-size" "0.8rem", HA.style "color" Theme.success ] [ text ("読込済: " ++ name) ]

        ( True, Nothing ) ->
            span [ HA.style "font-size" "0.8rem", HA.style "color" Theme.success ] [ text "読込済" ]

        ( False, Just name ) ->
            span [ HA.style "font-size" "0.8rem", HA.style "color" Theme.onPendingContainer ] [ text ("前回: " ++ name ++ "（要再読込）") ]

        ( False, Nothing ) ->
            text ""


{-| 読み込み済み・前回のファイル名が残っている間は「解除」ボタンを出す。曲データは消えないので、
断片棚・トラック削除のような二段階確認は不要。
-}
clearButton : Config msg -> ReferenceAudio -> Html msg
clearButton config ra =
    if ra.fileName == Nothing then
        text ""

    else
        button
            (Style.baseButton
                ++ [ HE.onClick config.clearedRefAudio
                   , HA.title "参考オーディオの読み込みを解除"
                   ]
            )
            [ text "解除" ]
