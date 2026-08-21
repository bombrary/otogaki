module View.HelpPanel exposing (view)

import Data.Help as Help
import Html exposing (Html, div, table, tbody, td, text, tr)
import Html.Attributes as HA
import View.Theme as Theme


{-| ショートカット一覧テーブル。内容は `Data.Help`（ShortcutsTab の全トピック）の単一情報源から
引く。タブ切替・文脈ジャンプは次のコミットで追加する（ここでは見た目をほぼ変えない）。
-}
view : Html msg
view =
    div []
        [ Html.h2 [] [ text "ショートカット一覧" ]
        , table [ HA.style "border-collapse" "collapse", HA.style "width" "100%" ]
            [ tbody []
                (Help.topicsIn Help.ShortcutsTab
                    |> List.concatMap .lines
                    |> List.map row
                )
            ]
        ]


row : Help.Line -> Html msg
row l =
    tr []
        [ td
            [ HA.style "padding" "0.3rem 0.8rem 0.3rem 0"
            , HA.style "white-space" "nowrap"
            , HA.style "font-family" "monospace"
            , HA.style "color" Theme.primary
            , HA.style "vertical-align" "top"
            ]
            [ text l.term ]
        , td [ HA.style "padding" "0.3rem 0", HA.style "color" Theme.onSurface ] [ text l.desc ]
        ]
