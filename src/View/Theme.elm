module View.Theme exposing
    ( elevation1
    , elevation2
    , elevation3
    , error
    , errorContainer
    , fontFamily
    , globalCss
    , highlight
    , highlightContainer
    , inversePrimary
    , inverseOnSurface
    , inverseSurface
    , keyBlack
    , keyWhite
    , loop
    , loopHandle
    , onError
    , onErrorContainer
    , onHighlightContainer
    , onPendingContainer
    , onPrimary
    , onPrimaryContainer
    , onSecondary
    , onSecondaryContainer
    , onSelection
    , onSuccessContainer
    , onSurface
    , onSurfaceVariant
    , onTertiary
    , onTertiaryContainer
    , outline
    , outlineVariant
    , pendingContainer
    , playhead
    , primary
    , primaryContainer
    , scrim
    , secondary
    , secondaryContainer
    , selection
    , selectionDeep
    , shapeFull
    , shapeL
    , shapeM
    , shapeS
    , shapeXL
    , shapeXS
    , success
    , successContainer
    , surface
    , surfaceContainer
    , surfaceContainerHigh
    , surfaceContainerHighest
    , surfaceContainerLow
    , surfaceContainerLowest
    , surfaceDim
    , tertiary
    , tertiaryContainer
    , typeBodyMedium
    , typeBodySmall
    , typeLabelLarge
    , typeLabelSmall
    , typeTitleLarge
    , typeTitleSmall
    , withAlpha
    )

{-| Material Design 3 のデザイントークン。全トークンの単一情報源。

シードカラーは現行プライマリ青 #4a90d9。M3 のライトテーマのみをフラット定数として公開する
（内部の `Scheme` レコードは将来のダークテーマ拡張のための下準備）。カラー値は
[Material Theme Builder](https://material-foundation.github.io/material-theme-builder/) に
#4A90D9 を入力した場合の青系標準出力に準拠する近似値。

`selection`/`highlight`/`loop`/`playhead`/`success`/`pending` は M3 の標準ロール（primary/secondary/
tertiary/error）には入り切らないこのアプリ固有の機能色で、M3 の custom colors の考え方に沿い
color/onColor/container ペアで定義する。`keyWhite`/`keyBlack` は鍵盤・指板の実物メタファであり、
surface 系トークンにはマップしない意図的な例外。

-}

import Char
import Html
import Html.Attributes as HA



-- Primary


primary : String
primary =
    "#0061A4"


onPrimary : String
onPrimary =
    "#FFFFFF"


primaryContainer : String
primaryContainer =
    "#D1E4FF"


onPrimaryContainer : String
onPrimaryContainer =
    "#001D36"


inversePrimary : String
inversePrimary =
    "#9ECAFF"



-- Secondary


secondary : String
secondary =
    "#526070"


onSecondary : String
onSecondary =
    "#FFFFFF"


secondaryContainer : String
secondaryContainer =
    "#D5E4F7"


onSecondaryContainer : String
onSecondaryContainer =
    "#0E1D2A"



-- Tertiary


tertiary : String
tertiary =
    "#6A5779"


onTertiary : String
onTertiary =
    "#FFFFFF"


tertiaryContainer : String
tertiaryContainer =
    "#F1DAFF"


onTertiaryContainer : String
onTertiaryContainer =
    "#251432"



-- Error


error : String
error =
    "#BA1A1A"


onError : String
onError =
    "#FFFFFF"


errorContainer : String
errorContainer =
    "#FFDAD6"


onErrorContainer : String
onErrorContainer =
    "#410002"



-- Surface / Neutral


surface : String
surface =
    "#F7F9FF"


onSurface : String
onSurface =
    "#191C20"


surfaceDim : String
surfaceDim =
    "#D8DAE0"


onSurfaceVariant : String
onSurfaceVariant =
    "#42474E"


surfaceContainerLowest : String
surfaceContainerLowest =
    "#FFFFFF"


surfaceContainerLow : String
surfaceContainerLow =
    "#F1F3F9"


surfaceContainer : String
surfaceContainer =
    "#EBEEF3"


surfaceContainerHigh : String
surfaceContainerHigh =
    "#E6E8EE"


surfaceContainerHighest : String
surfaceContainerHighest =
    "#E0E2E8"


outline : String
outline =
    "#72777F"


outlineVariant : String
outlineVariant =
    "#C2C7CF"


inverseSurface : String
inverseSurface =
    "#2D3135"


inverseOnSurface : String
inverseOnSurface =
    "#EFF1F7"


scrim : String
scrim =
    "#000000"



-- Custom colors（このアプリ固有の機能色。M3 の extended colors）


selection : String
selection =
    "#B42B62"


onSelection : String
onSelection =
    "#FFFFFF"


{-| 選択ノートの枠線など、selection よりさらに濃い強調用（旧 #9c1f52 の後継）。
-}
selectionDeep : String
selectionDeep =
    "#8E1D4C"


highlight : String
highlight =
    "#F5C518"


highlightContainer : String
highlightContainer =
    "#FFF0C9"


onHighlightContainer : String
onHighlightContainer =
    "#584400"


loop : String
loop =
    "#5E60CE"


loopHandle : String
loopHandle =
    "#3F3D9E"


{-| 再生位置マーカーの色。M3 の error ロールはエラー状態専用なので流用せず、
プレイヘッド専用の custom color として独立させる。
-}
playhead : String
playhead =
    "#D32F2F"


success : String
success =
    "#2C7A2C"


successContainer : String
successContainer =
    "#E8F8EE"


onSuccessContainer : String
onSuccessContainer =
    "#1E5A1E"


{-| badge の "loading" トーンなど、進行中を示すオレンジ系（View.Palette のセクション橙と同系統）。
-}
pendingContainer : String
pendingContainer =
    "#FFDCBE"


onPendingContainer : String
onPendingContainer =
    "#8F4C00"


{-| ピアノ鍵盤・ギター指板の実物色。楽器の見た目を表す例外トークンで、surface 系にはマップしない。
-}
keyWhite : String
keyWhite =
    "#FFFFFF"


keyBlack : String
keyBlack =
    "#222222"



-- Shape


shapeXS : String
shapeXS =
    "4px"


shapeS : String
shapeS =
    "8px"


shapeM : String
shapeM =
    "12px"


shapeL : String
shapeL =
    "16px"


shapeXL : String
shapeXL =
    "28px"


shapeFull : String
shapeFull =
    "999px"



-- Elevation


elevation1 : String
elevation1 =
    "0 1px 2px rgba(0, 0, 0, 0.3), 0 1px 3px 1px rgba(0, 0, 0, 0.15)"


elevation2 : String
elevation2 =
    "0 1px 2px rgba(0, 0, 0, 0.3), 0 2px 6px 2px rgba(0, 0, 0, 0.15)"


elevation3 : String
elevation3 =
    "0 1px 3px rgba(0, 0, 0, 0.3), 0 4px 8px 3px rgba(0, 0, 0, 0.15)"



-- Typography


fontFamily : String
fontFamily =
    "\"Roboto\", \"Helvetica Neue\", \"Noto Sans JP\", system-ui, sans-serif"


typeTitleLarge : List (Html.Attribute msg)
typeTitleLarge =
    [ HA.style "font-size" "1.375rem"
    , HA.style "font-weight" "400"
    , HA.style "line-height" "1.75rem"
    , HA.style "letter-spacing" "0"
    ]


typeTitleSmall : List (Html.Attribute msg)
typeTitleSmall =
    [ HA.style "font-size" "0.875rem"
    , HA.style "font-weight" "500"
    , HA.style "line-height" "1.25rem"
    , HA.style "letter-spacing" "0.00625rem"
    ]


{-| M3 の Label Large（14sp/weight 500/letter-spacing 0.1px/line-height 20sp）。
ボタンラベルなど、押せる要素のテキストに使う。
-}
typeLabelLarge : List (Html.Attribute msg)
typeLabelLarge =
    [ HA.style "font-size" "0.875rem"
    , HA.style "font-weight" "500"
    , HA.style "line-height" "1.25rem"
    , HA.style "letter-spacing" "0.00625rem"
    ]


typeBodyMedium : List (Html.Attribute msg)
typeBodyMedium =
    [ HA.style "font-size" "0.875rem"
    , HA.style "font-weight" "400"
    , HA.style "line-height" "1.25rem"
    , HA.style "letter-spacing" "0.015625rem"
    ]


typeBodySmall : List (Html.Attribute msg)
typeBodySmall =
    [ HA.style "font-size" "0.75rem"
    , HA.style "font-weight" "400"
    , HA.style "line-height" "1rem"
    , HA.style "letter-spacing" "0.025rem"
    ]


{-| M3 の Label Small（11sp/weight 500/letter-spacing 0.5px/line-height 16sp）。
-}
typeLabelSmall : List (Html.Attribute msg)
typeLabelSmall =
    [ HA.style "font-size" "0.6875rem"
    , HA.style "font-weight" "500"
    , HA.style "line-height" "1rem"
    , HA.style "letter-spacing" "0.03125rem"
    ]



-- Alpha 合成


{-| `#RRGGBB` 形式の HEX 文字列を、指定した不透明度の `rgba(...)` 文字列に変換する。
SVG の `SA.fill`/`SA.stroke` や state layer など、直書きの `rgba(...)` リテラルを
トークン由来にするために使う。

    withAlpha 0.5 "#0061A4" == "rgba(0, 97, 164, 0.5)"

-}
withAlpha : Float -> String -> String
withAlpha alpha hex =
    let
        clean =
            if String.startsWith "#" hex then
                String.dropLeft 1 hex

            else
                hex

        channel start =
            String.slice start (start + 2) clean
                |> hexPairToInt
                |> String.fromInt
    in
    "rgba(" ++ channel 0 ++ ", " ++ channel 2 ++ ", " ++ channel 4 ++ ", " ++ String.fromFloat alpha ++ ")"


hexPairToInt : String -> Int
hexPairToInt pair =
    String.toList pair
        |> List.map hexCharToInt
        |> List.foldl (\d acc -> acc * 16 + d) 0


hexCharToInt : Char -> Int
hexCharToInt c =
    case Char.toUpper c of
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
        'A' -> 10
        'B' -> 11
        'C' -> 12
        'D' -> 13
        'E' -> 14
        'F' -> 15
        _ -> 0



-- Global CSS


{-| ページ全体に注入するグローバル CSS。`View.Style.focusCss` の後継。
reset・body のフォント/配色・:focus-visible・ボタンの state layer（`.m3-btn`）をまとめる。

インラインスタイルの `background` は CSS の `:hover` に優先度で勝ってしまうため、
state layer は `::after` 擬似要素 + `currentColor` で重ねる方式にする。これにより
ボタンの背景色・文字色は引き続き Elm 側（Theme の定数）が単一の情報源のまま、
クラス名を1つ足すだけで hover/active の見た目が付く。
-}
globalCss : Html.Html msg
globalCss =
    Html.node "style"
        []
        [ Html.text (String.join "\n" cssRules) ]


cssRules : List String
cssRules =
    [ "html, body { margin: 0; height: 100%; }"
    , "body { font-family: " ++ fontFamily ++ "; color: " ++ onSurface ++ "; background: " ++ surface ++ "; }"
    , "button:focus-visible, [tabindex]:focus-visible, input:focus-visible, select:focus-visible { outline: 2px solid "
        ++ primary
        ++ "; outline-offset: 1px; }"
    , ".m3-btn { position: relative; }"
    , ".m3-btn::after { content: \"\"; position: absolute; inset: 0; border-radius: inherit; background: currentColor; opacity: 0; pointer-events: none; transition: opacity 0.1s; }"
    , ".m3-btn:hover::after { opacity: 0.08; }"
    , ".m3-btn:active::after { opacity: 0.10; }"
    , ".m3-btn:focus-visible::after { opacity: 0.10; }"
    , ".m3-hit { transition: filter 0.1s; }"
    , ".m3-hit:hover { filter: brightness(0.94); }"
    ]
