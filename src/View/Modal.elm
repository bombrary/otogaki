module View.Modal exposing (view)

import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events as HE
import View.Theme as Theme


{-| 画面中央に表示する固定オーバーレイ。380px サイドバーに収まらない編集 UI
（コード進行のテキストエリア、ボイシングのピアノ＋指板など）を広く表示するために使う。

M3 の dialog 相当（shapeXL の大きな角丸、surfaceContainerHigh、elevation3）。
-}
view : msg -> List (Html msg) -> Html msg
view onClose body =
    div
        [ HA.style "position" "fixed"
        , HA.style "top" "0"
        , HA.style "left" "0"
        , HA.style "right" "0"
        , HA.style "bottom" "0"
        , HA.style "background" (Theme.withAlpha 0.32 Theme.scrim)
        , HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HA.style "z-index" "1000"
        ]
        [ div
            [ HA.style "background" Theme.surfaceContainerHigh
            , HA.style "border-radius" Theme.shapeXL
            , HA.style "max-width" "900px"
            , HA.style "width" "90vw"
            , HA.style "max-height" "90vh"
            , HA.style "overflow-y" "auto"
            , HA.style "padding" "1.5rem"
            , HA.style "box-shadow" Theme.elevation3
            ]
            (div [ HA.style "display" "flex", HA.style "justify-content" "flex-end" ]
                [ button
                    [ HE.onClick onClose
                    , HA.class "m3-btn"
                    , HA.style "border" "none"
                    , HA.style "background" "transparent"
                    , HA.style "border-radius" Theme.shapeFull
                    , HA.style "color" Theme.onSurfaceVariant
                    , HA.style "font-size" "1.2rem"
                    , HA.style "cursor" "pointer"
                    , HA.title "閉じる"
                    ]
                    [ text "✕" ]
                ]
                :: body
            )
        ]
