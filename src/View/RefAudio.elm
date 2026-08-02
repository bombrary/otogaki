module View.RefAudio exposing (Config, view)

import Data.ReferenceAudio exposing (ReferenceAudio)
import Html exposing (Html, button, div, input, label, span, text)
import Html.Attributes as HA
import Html.Events as HE


type alias Config msg =
    { changedOffset : String -> msg
    , blurredOffset : msg
    , changedVolume : String -> msg
    , toggledMute : msg
    }


view : Config msg -> String -> Bool -> ReferenceAudio -> Html msg
view config offsetInput loaded ra =
    div
        [ HA.style "margin-top" "1rem"
        , HA.style "padding" "0.5rem"
        , HA.style "border" "1px solid #ddd"
        , HA.style "border-radius" "4px"
        ]
        [ div [ HA.style "display" "flex", HA.style "gap" "0.5rem", HA.style "align-items" "center", HA.style "flex-wrap" "wrap" ]
            [ span [ HA.style "font-size" "0.9rem", HA.style "font-weight" "bold" ] [ text "🎧 参考オーディオ" ]
            , span [ HA.style "font-size" "0.75rem", HA.style "color" "#888" ]
                [ text "耳コピ・分析用。音声データは保存されないので、リロード後はファイルを選び直してね" ]
            ]
        , div [ HA.style "display" "flex", HA.style "gap" "0.7rem", HA.style "align-items" "center", HA.style "flex-wrap" "wrap", HA.style "margin-top" "0.4rem" ]
            [ input
                [ HA.type_ "file"
                , HA.id "ref-audio-input"
                , HA.accept "audio/*"
                ]
                []
            , statusView loaded ra
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
                [ HE.onClick config.toggledMute
                , HA.style "background"
                    (if ra.muted then
                        "#e74c3c"

                     else
                        "#eee"
                    )
                , HA.style "color"
                    (if ra.muted then
                        "#fff"

                     else
                        "#333"
                    )
                , HA.title "参考オーディオをミュート"
                ]
                [ text "M" ]
            ]
        ]


statusView : Bool -> ReferenceAudio -> Html msg
statusView loaded ra =
    case ( loaded, ra.fileName ) of
        ( True, Just name ) ->
            span [ HA.style "font-size" "0.8rem", HA.style "color" "#2c7a2c" ] [ text ("読込済: " ++ name) ]

        ( True, Nothing ) ->
            span [ HA.style "font-size" "0.8rem", HA.style "color" "#2c7a2c" ] [ text "読込済" ]

        ( False, Just name ) ->
            span [ HA.style "font-size" "0.8rem", HA.style "color" "#e67e22" ] [ text ("前回: " ++ name ++ "（要再読込）") ]

        ( False, Nothing ) ->
            text ""
