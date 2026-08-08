module View.Modal exposing (view)

import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events as HE


{-| 画面中央に表示する固定オーバーレイ。380px サイドバーに収まらない編集 UI
（コード進行のテキストエリア、ボイシングのピアノ＋指板など）を広く表示するために使う。
-}
view : msg -> List (Html msg) -> Html msg
view onClose body =
    div
        [ HA.style "position" "fixed"
        , HA.style "top" "0"
        , HA.style "left" "0"
        , HA.style "right" "0"
        , HA.style "bottom" "0"
        , HA.style "background" "rgba(0, 0, 0, 0.4)"
        , HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HA.style "z-index" "1000"
        ]
        [ div
            [ HA.style "background" "#fff"
            , HA.style "border-radius" "6px"
            , HA.style "max-width" "900px"
            , HA.style "width" "90vw"
            , HA.style "max-height" "90vh"
            , HA.style "overflow-y" "auto"
            , HA.style "padding" "1rem"
            , HA.style "box-shadow" "0 4px 24px rgba(0, 0, 0, 0.3)"
            ]
            (div [ HA.style "display" "flex", HA.style "justify-content" "flex-end" ]
                [ button
                    [ HE.onClick onClose
                    , HA.style "border" "none"
                    , HA.style "background" "transparent"
                    , HA.style "font-size" "1.2rem"
                    , HA.style "cursor" "pointer"
                    , HA.title "閉じる"
                    ]
                    [ text "✕" ]
                ]
                :: body
            )
        ]
