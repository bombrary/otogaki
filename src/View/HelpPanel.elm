module View.HelpPanel exposing (Config, view)

import Data.Help as Help
import Html exposing (Html, button, div, h2, table, tbody, td, text, tr)
import Html.Attributes as HA
import Html.Events as HE
import View.Style as Style
import View.Theme as Theme


type alias Config msg =
    { selectedTab : Help.Tab -> msg
    , selectedTopic : Help.TopicId -> msg
    }


{-| タブ付き総合ヘルプ。`focus` があればそのトピックを先頭に移して強調枠で囲む（DOM スクロールではなく
並べ替えでジャンプを実現する。`Data.Help` の doc comment 参照）。
-}
view : Config msg -> { tab : Help.Tab, focus : Maybe Help.TopicId } -> Html msg
view config state =
    div []
        [ h2 [] [ text "ヘルプ" ]
        , tabBar config state.tab
        , topicNav config state
        , div [] (List.map (topicCard state.focus) (orderedTopics state.focus (Help.topicsIn state.tab)))
        , div (HA.style "margin-top" "0.8rem" :: Style.hintText) [ text "`?` キーでいつでも開けます。閉じるのは Esc か ✕。" ]
        ]


tabBar : Config msg -> Help.Tab -> Html msg
tabBar config current =
    div [ HA.style "display" "flex", HA.style "gap" "0.4rem", HA.style "flex-wrap" "wrap", HA.style "margin-bottom" "0.6rem" ]
        (List.map
            (\tab ->
                button
                    (Style.toggleButton (tab == current) ++ [ HE.onClick (config.selectedTab tab) ])
                    [ text (Help.tabLabel tab) ]
            )
            Help.tabs
        )


topicNav : Config msg -> { tab : Help.Tab, focus : Maybe Help.TopicId } -> Html msg
topicNav config state =
    div [ HA.style "display" "flex", HA.style "gap" "0.3rem", HA.style "flex-wrap" "wrap", HA.style "margin-bottom" "0.6rem" ]
        (Help.topicsIn state.tab
            |> List.map
                (\topic ->
                    button
                        (Style.toggleButton (state.focus == Just topic.id) ++ [ HE.onClick (config.selectedTopic topic.id) ])
                        [ text topic.title ]
                )
        )


orderedTopics : Maybe Help.TopicId -> List Help.Topic -> List Help.Topic
orderedTopics focus topicList =
    case focus of
        Nothing ->
            topicList

        Just topicId ->
            let
                ( matching, rest ) =
                    List.partition (\t -> t.id == topicId) topicList
            in
            matching ++ rest


topicCard : Maybe Help.TopicId -> Help.Topic -> Html msg
topicCard focus topic =
    let
        highlighted =
            focus == Just topic.id
    in
    div
        [ HA.id (Help.domId topic.id)
        , HA.style "margin-bottom" "1rem"
        , HA.style "padding" "0.5rem"
        , HA.style "border-radius" "6px"
        , HA.style "border"
            ("1px solid "
                ++ (if highlighted then
                        Theme.primary

                    else
                        "transparent"
                   )
            )
        , HA.style "background"
            (if highlighted then
                Theme.highlightContainer

             else
                "transparent"
            )
        ]
        [ div Style.headingText [ text topic.title ]
        , table [ HA.style "border-collapse" "collapse", HA.style "width" "100%", HA.style "margin-top" "0.3rem" ]
            [ tbody [] (List.map (row topic.tab) topic.lines) ]
        ]


row : Help.Tab -> Help.Line -> Html msg
row tab l =
    tr []
        [ td
            [ HA.style "padding" "0.3rem 0.8rem 0.3rem 0"
            , HA.style "white-space" "nowrap"
            , HA.style "font-family"
                (if tab == Help.ShortcutsTab then
                    "monospace"

                 else
                    "inherit"
                )
            , HA.style "color" Theme.primary
            , HA.style "vertical-align" "top"
            ]
            [ text l.term ]
        , td [ HA.style "padding" "0.3rem 0", HA.style "color" Theme.onSurface ] [ text l.desc ]
        ]
