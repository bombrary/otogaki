module View.SectionBar exposing (Config, view)

import Data.Section exposing (Section)
import Html exposing (Html, button, div, input, span, text, textarea)
import Html.Attributes as HA
import Html.Events as HE


type alias Config msg =
    { select : Int -> msg
    , add : msg
    , remove : Int -> msg
    , rename : Int -> String -> msg
    , changeBars : Int -> String -> msg
    , changeMemo : Int -> String -> msg
    , move : Int -> Int -> msg
    }


view : Config msg -> Maybe Int -> List Section -> Html msg
view config selectedId sections =
    div [ HA.style "margin-top" "1rem" ]
        [ div
            [ HA.style "display" "flex"
            , HA.style "gap" "2px"
            , HA.style "align-items" "stretch"
            , HA.style "flex-wrap" "wrap"
            ]
            (List.map (blockView config selectedId) sections
                ++ [ button [ HE.onClick config.add ] [ text "+ セクション" ] ]
            )
        , case selectedId |> Maybe.andThen (\sid -> sections |> List.filter (\s -> s.id == sid) |> List.head) of
            Just section ->
                editPanel config section

            Nothing ->
                text ""
        ]


blockView : Config msg -> Maybe Int -> Section -> Html msg
blockView config selectedId section =
    let
        selected =
            selectedId == Just section.id
    in
    div
        [ HA.style "width" (String.fromInt (section.lengthBars * 40) ++ "px")
        , HA.style "padding" "0.3rem"
        , HA.style "text-align" "center"
        , HA.style "border"
            (if selected then
                "2px solid #4a90d9"

             else
                "1px solid #bbb"
            )
        , HA.style "border-radius" "4px"
        , HA.style "background"
            (if selected then
                "#eef4fb"

             else
                "#f7f7f7"
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


editPanel : Config msg -> Section -> Html msg
editPanel config section =
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
        , textarea
            [ HA.value section.memo
            , HE.onInput (config.changeMemo section.id)
            , HA.placeholder "メモ:「ここは切なく」「サビは転調したい」など"
            , HA.style "width" "98%"
            , HA.style "margin-top" "0.4rem"
            , HA.style "min-height" "3rem"
            , HA.style "font-family" "inherit"
            ]
            []
        ]
