module View.Keyboard exposing (Config, view)

import Html exposing (Html, button, div, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Set exposing (Set)
import View.Style as Style
import View.Theme as Theme


type alias Config msg =
    { pressedKey : Int -> msg
    , toggled : msg
    }


lowPitch : Int
lowPitch =
    48


highPitch : Int
highPitch =
    72


isWhite : Int -> Bool
isWhite pitch =
    List.member (modBy 12 pitch) [ 0, 2, 4, 5, 7, 9, 11 ]


whiteKeyWidth : Int
whiteKeyWidth =
    34


view : Config msg -> Set Int -> Bool -> Html msg
view config highlighted open =
    div [ HA.style "margin-top" "1rem" ]
        (button (Style.toggleButton open ++ [ HE.onClick config.toggled ])
            [ text
                (if open then
                    "🎹 鍵盤を閉じる"

                 else
                    "🎹 鍵盤を開く"
                )
            ]
            :: (if open then
                    [ pianoView config highlighted
                    , div [ HA.style "font-size" "0.75rem", HA.style "color" Theme.onSurfaceVariant, HA.style "margin-top" "0.3rem" ]
                        [ text "キーボードでも弾ける: Z〜M = C3〜B3 / Q,2,W,3,E,R,5,T,6,Y,7,U = C4〜B4 / I = C5（選択中のトラックの音色）" ]
                    ]

                else
                    []
               )
        )


whitesBelow : Int -> Int
whitesBelow pitch =
    List.range lowPitch (pitch - 1)
        |> List.filter isWhite
        |> List.length


pianoView : Config msg -> Set Int -> Html msg
pianoView config highlighted =
    let
        pitches =
            List.range lowPitch highPitch

        whiteCount =
            List.filter isWhite pitches |> List.length

        whiteKey pitch =
            div
                [ HA.style "position" "absolute"
                , HA.style "left" (String.fromInt (whitesBelow pitch * whiteKeyWidth) ++ "px")
                , HA.style "top" "0"
                , HA.style "width" (String.fromInt (whiteKeyWidth - 1) ++ "px")
                , HA.style "height" "110px"
                , HA.style "background"
                    (if Set.member pitch highlighted then
                        Style.colorHighlight

                     else
                        Theme.keyWhite
                    )
                , HA.style "border" ("1px solid " ++ Theme.outline)
                , HA.style "box-sizing" "border-box"
                , HA.style "cursor" "pointer"
                , HA.style "user-select" "none"
                , HA.style "display" "flex"
                , HA.style "align-items" "flex-end"
                , HA.style "justify-content" "center"
                , HA.style "font-size" "9px"
                , HA.style "color" Theme.outline
                , HE.onMouseDown (config.pressedKey pitch)
                ]
                [ text
                    (if modBy 12 pitch == 0 then
                        "C" ++ String.fromInt (pitch // 12 - 1)

                     else
                        ""
                    )
                ]

        blackKey pitch =
            div
                [ HA.style "position" "absolute"
                , HA.style "left" (String.fromInt (whitesBelow pitch * whiteKeyWidth - 11) ++ "px")
                , HA.style "top" "0"
                , HA.style "width" "22px"
                , HA.style "height" "68px"
                , HA.style "background"
                    (if Set.member pitch highlighted then
                        Theme.highlight

                     else
                        Theme.keyBlack
                    )
                , HA.style "border" ("1px solid " ++ Theme.keyBlack)
                , HA.style "border-radius" "0 0 3px 3px"
                , HA.style "box-sizing" "border-box"
                , HA.style "cursor" "pointer"
                , HA.style "z-index" "2"
                , HE.onMouseDown (config.pressedKey pitch)
                ]
                []
    in
    div
        [ HA.style "position" "relative"
        , HA.style "height" "110px"
        , HA.style "width" (String.fromInt (whiteCount * whiteKeyWidth) ++ "px")
        , HA.style "margin-top" "0.4rem"
        ]
        (List.map whiteKey (List.filter isWhite pitches)
            ++ List.map blackKey (List.filter (\p -> not (isWhite p)) pitches)
        )
