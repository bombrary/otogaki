module View.HelpPanel exposing (view)

import Html exposing (Html, div, table, tbody, td, text, tr)
import Html.Attributes as HA
import View.Theme as Theme


{-| README.md の「キーボード」表と同一内容を単一情報源として持つ。
アプリ内ヘルプ（? ボタン / ? キー）と README を乖離させないよう、両方をこのリストから更新すること。
-}
shortcuts : List { keys : String, desc : String }
shortcuts =
    [ { keys = "Space", desc = "再生 / 停止（現在のループモードに従う。フォーカス位置によらず常にこの動作。ボタンは Enter で押せる）" }
    , { keys = "Ctrl/Cmd+Z", desc = "元に戻す（+Shift または Ctrl/Cmd+Y でやり直し）" }
    , { keys = "Home / End", desc = "曲の先頭 / 末尾へシーク" }
    , { keys = "↑↓", desc = "選択ノートを半音移動（+Shift でオクターブ）" }
    , { keys = "←→", desc = "隣のノートを選択（音付き）" }
    , { keys = "Ctrl/Cmd+←→", desc = "選択ノートを 1/16 移動（+Shift で 1 小節）" }
    , { keys = "n", desc = "再生位置にノートを追加（鍵盤表示中は無効）" }
    , { keys = "c", desc = "カットツールの切替（ノートをクリックした位置で分割）" }
    , { keys = "g", desc = "隣接する選択ノートをマージ（カットの逆操作）" }
    , { keys = "[ / ]", desc = "ループ範囲の開始 / 終了を再生位置に設定" }
    , { keys = "Ctrl/Cmd+C / X / V", desc = "コピー / カット / 再生位置に貼付" }
    , { keys = "Ctrl/Cmd+A", desc = "選択中トラックの全ノートを選択" }
    , { keys = "Ctrl/Cmd+Shift+A", desc = "選択中のセクション内のノートを選択" }
    , { keys = "1〜9", desc = "その番目のセクションの先頭へジャンプ" }
    , { keys = "Delete", desc = "選択ノートを削除" }
    , { keys = "Shift+ドラッグ", desc = "矩形選択" }
    , { keys = "Escape", desc = "選択解除（セクション/トラック/断片の削除確認待ちも解除）、開いているモーダルを閉じる" }
    , { keys = "?", desc = "このヘルプを開閉" }
    , { keys = "Tab", desc = "ボタンやピアノロール領域にフォーカスを移動" }
    ]


view : Html msg
view =
    div []
        [ Html.h2 [] [ text "ショートカット一覧" ]
        , table [ HA.style "border-collapse" "collapse", HA.style "width" "100%" ]
            [ tbody [] (List.map row shortcuts) ]
        ]


row : { keys : String, desc : String } -> Html msg
row s =
    tr []
        [ td
            [ HA.style "padding" "0.3rem 0.8rem 0.3rem 0"
            , HA.style "white-space" "nowrap"
            , HA.style "font-family" "monospace"
            , HA.style "color" Theme.primary
            , HA.style "vertical-align" "top"
            ]
            [ text s.keys ]
        , td [ HA.style "padding" "0.3rem 0", HA.style "color" Theme.onSurface ] [ text s.desc ]
        ]
