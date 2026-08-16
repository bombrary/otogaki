module View.Theme exposing
    ( ThemePreference(..)
    , elevation1
    , elevation2
    , elevation3
    , error
    , errorContainer
    , fontFamily
    , globalCss
    , gridRowBlackKey
    , gridRowWhiteKey
    , highlight
    , highlightContainer
    , inversePrimary
    , inverseOnSurface
    , inverseSurface
    , keyBlack
    , keyWhite
    , loop
    , loopHandle
    , noteFill
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
    , themeFromString
    , themeToString
    , typeBodyMedium
    , typeBodySmall
    , typeLabelLarge
    , typeLabelSmall
    , typeTitleLarge
    , typeTitleSmall
    , withAlpha
    )

{-| Material Design 3 のデザイントークン。全トークンの単一情報源。

シードカラーは現行プライマリ青 #4a90d9。ライト/ダーク両方の M3 スキームを CSS カスタムプロパティ
（`--md-xxx`）として `globalCss` が注入し、色トークンの Elm 側の値はすべて `var(--md-xxx)` 参照文字列
になる。実際のライト/ダーク値は `colorTokens` に一本化されている（この単一リストから `:root`・
`prefers-color-scheme: dark`・`[data-theme=dark]` の3ブロックを生成する）。カラー値は
[Material Theme Builder](https://material-foundation.github.io/material-theme-builder/) に
#4A90D9 を入力した場合の青系標準出力に準拠する近似値。

`selection`/`highlight`/`loop`/`playhead`/`success`/`pending` は M3 の標準ロール（primary/secondary/
tertiary/error）には入り切らないこのアプリ固有の機能色で、M3 の custom colors の考え方に沿い
color/onColor/container ペアで定義する。`keyWhite`/`keyBlack` は鍵盤・指板の実物メタファであり、
surface 系トークンにはマップしない意図的な例外（ダークでも不変）。

-}

import Html
import Html.Attributes as HA



-- テーマ切替


{-| 手動テーマ選択。`SystemTheme` は OS の `prefers-color-scheme` に追従する。
-}
type ThemePreference
    = SystemTheme
    | LightTheme
    | DarkTheme


themeToString : ThemePreference -> String
themeToString theme =
    case theme of
        SystemTheme ->
            "system"

        LightTheme ->
            "light"

        DarkTheme ->
            "dark"


themeFromString : String -> Maybe ThemePreference
themeFromString raw =
    case raw of
        "system" ->
            Just SystemTheme

        "light" ->
            Just LightTheme

        "dark" ->
            Just DarkTheme

        _ ->
            Nothing



-- カラートークン


{-| 色トークン1件のライト/ダークのペア。`css` は CSS カスタムプロパティ名の末尾（`--md-` の後ろ）。
-}
type alias ColorToken =
    { css : String
    , light : String
    , dark : String
    }


{-| 全カラートークンの単一情報源。`globalCss` はここから `:root`・
`@media (prefers-color-scheme: dark)`・`[data-theme=dark]` の3ブロックを生成する。
-}
colorTokens : List ColorToken
colorTokens =
    [ ColorToken "primary" "#0061A4" "#9ECAFF"
    , ColorToken "on-primary" "#FFFFFF" "#003258"
    , ColorToken "primary-container" "#D1E4FF" "#00497D"
    , ColorToken "on-primary-container" "#001D36" "#D1E4FF"
    , ColorToken "inverse-primary" "#9ECAFF" "#0061A4"
    , ColorToken "secondary" "#526070" "#BAC8DA"
    , ColorToken "on-secondary" "#FFFFFF" "#24323F"
    , ColorToken "secondary-container" "#D5E4F7" "#3A4857"
    , ColorToken "on-secondary-container" "#0E1D2A" "#D5E4F7"
    , ColorToken "tertiary" "#6A5779" "#D6BEE4"
    , ColorToken "on-tertiary" "#FFFFFF" "#3B2948"
    , ColorToken "tertiary-container" "#F1DAFF" "#52405F"
    , ColorToken "on-tertiary-container" "#251432" "#F1DAFF"
    , ColorToken "error" "#BA1A1A" "#FFB4AB"
    , ColorToken "on-error" "#FFFFFF" "#690005"
    , ColorToken "error-container" "#FFDAD6" "#93000A"
    , ColorToken "on-error-container" "#410002" "#FFDAD6"
    , ColorToken "surface" "#F7F9FF" "#12131A"
    , ColorToken "on-surface" "#191C20" "#E2E2E9"
    , ColorToken "surface-dim" "#D8DAE0" "#12131A"
    , ColorToken "on-surface-variant" "#42474E" "#C3C6CF"
    , ColorToken "surface-container-lowest" "#FFFFFF" "#0C0D13"
    , ColorToken "surface-container-low" "#F1F3F9" "#1A1B22"
    , ColorToken "surface-container" "#EBEEF3" "#1E2027"
    , ColorToken "surface-container-high" "#E6E8EE" "#282A31"
    , ColorToken "surface-container-highest" "#E0E2E8" "#33353C"
    , ColorToken "outline" "#72777F" "#8D9199"
    , ColorToken "outline-variant" "#C2C7CF" "#43474E"
    , ColorToken "inverse-surface" "#2D3135" "#E2E2E9"
    , ColorToken "inverse-on-surface" "#EFF1F7" "#2F3036"
    , ColorToken "scrim" "#000000" "#000000"
    , ColorToken "note-fill" "#4A90D9" "#6FA8E0"
    , ColorToken "selection" "#B42B62" "#FFB1C8"
    , ColorToken "on-selection" "#FFFFFF" "#5E1133"
    , ColorToken "selection-deep" "#8E1D4C" "#FFD9E2"
    , ColorToken "highlight" "#F5C518" "#F5C518"
    , ColorToken "highlight-container" "#FFF0C9" "#4D3C00"
    , ColorToken "on-highlight-container" "#584400" "#FFE38C"
    , ColorToken "loop" "#5E60CE" "#C2C1FF"
    , ColorToken "loop-handle" "#3F3D9E" "#A6A4FF"
    , ColorToken "playhead" "#D32F2F" "#FFB4A9"
    , ColorToken "success" "#2C7A2C" "#8FDB8F"
    , ColorToken "success-container" "#E8F8EE" "#0F3D0F"
    , ColorToken "on-success-container" "#1E5A1E" "#B8F0B8"
    , ColorToken "pending-container" "#FFDCBE" "#6B3600"
    , ColorToken "on-pending-container" "#8F4C00" "#FFCA9E"
    , ColorToken "grid-row-white-key" "#FFFFFF" "#1A1B22"
    , ColorToken "grid-row-black-key" "#E6E8EE" "#0C0D13"
    ]


{-| CSS カスタムプロパティ参照文字列を組み立てる。`colorTokens` の `css` 名と対応させること。
-}
tokenVar : String -> String
tokenVar name =
    "var(--md-" ++ name ++ ")"



-- Primary


primary : String
primary =
    tokenVar "primary"


onPrimary : String
onPrimary =
    tokenVar "on-primary"


primaryContainer : String
primaryContainer =
    tokenVar "primary-container"


onPrimaryContainer : String
onPrimaryContainer =
    tokenVar "on-primary-container"


inversePrimary : String
inversePrimary =
    tokenVar "inverse-primary"



-- Secondary


secondary : String
secondary =
    tokenVar "secondary"


onSecondary : String
onSecondary =
    tokenVar "on-secondary"


secondaryContainer : String
secondaryContainer =
    tokenVar "secondary-container"


onSecondaryContainer : String
onSecondaryContainer =
    tokenVar "on-secondary-container"



-- Tertiary


tertiary : String
tertiary =
    tokenVar "tertiary"


onTertiary : String
onTertiary =
    tokenVar "on-tertiary"


tertiaryContainer : String
tertiaryContainer =
    tokenVar "tertiary-container"


onTertiaryContainer : String
onTertiaryContainer =
    tokenVar "on-tertiary-container"



-- Error


error : String
error =
    tokenVar "error"


onError : String
onError =
    tokenVar "on-error"


errorContainer : String
errorContainer =
    tokenVar "error-container"


onErrorContainer : String
onErrorContainer =
    tokenVar "on-error-container"



-- Surface / Neutral


surface : String
surface =
    tokenVar "surface"


onSurface : String
onSurface =
    tokenVar "on-surface"


surfaceDim : String
surfaceDim =
    tokenVar "surface-dim"


onSurfaceVariant : String
onSurfaceVariant =
    tokenVar "on-surface-variant"


surfaceContainerLowest : String
surfaceContainerLowest =
    tokenVar "surface-container-lowest"


surfaceContainerLow : String
surfaceContainerLow =
    tokenVar "surface-container-low"


surfaceContainer : String
surfaceContainer =
    tokenVar "surface-container"


surfaceContainerHigh : String
surfaceContainerHigh =
    tokenVar "surface-container-high"


surfaceContainerHighest : String
surfaceContainerHighest =
    tokenVar "surface-container-highest"


outline : String
outline =
    tokenVar "outline"


outlineVariant : String
outlineVariant =
    tokenVar "outline-variant"


inverseSurface : String
inverseSurface =
    tokenVar "inverse-surface"


inverseOnSurface : String
inverseOnSurface =
    tokenVar "inverse-on-surface"


scrim : String
scrim =
    tokenVar "scrim"



-- Custom colors（このアプリ固有の機能色。M3 の extended colors）


{-| MIDI ノート本体の塗り。シードカラー #4A90D9 そのもの。枠線・リサイズ尻尾には
primary を使い、Studio One 流の「明るい本体 + 同系濃色の縁」を作る。
-}
noteFill : String
noteFill =
    tokenVar "note-fill"


selection : String
selection =
    tokenVar "selection"


onSelection : String
onSelection =
    tokenVar "on-selection"


{-| 選択ノートの枠線など、selection よりさらに濃い強調用（旧 #9c1f52 の後継）。
-}
selectionDeep : String
selectionDeep =
    tokenVar "selection-deep"


highlight : String
highlight =
    tokenVar "highlight"


highlightContainer : String
highlightContainer =
    tokenVar "highlight-container"


onHighlightContainer : String
onHighlightContainer =
    tokenVar "on-highlight-container"


loop : String
loop =
    tokenVar "loop"


loopHandle : String
loopHandle =
    tokenVar "loop-handle"


{-| 再生位置マーカーの色。M3 の error ロールはエラー状態専用なので流用せず、
プレイヘッド専用の custom color として独立させる。
-}
playhead : String
playhead =
    tokenVar "playhead"


success : String
success =
    tokenVar "success"


successContainer : String
successContainer =
    tokenVar "success-container"


onSuccessContainer : String
onSuccessContainer =
    tokenVar "on-success-container"


{-| badge の "loading" トーンなど、進行中を示すオレンジ系（View.Palette のセクション橙と同系統）。
-}
pendingContainer : String
pendingContainer =
    tokenVar "pending-container"


onPendingContainer : String
onPendingContainer =
    tokenVar "on-pending-container"


{-| ピアノ鍵盤・ギター指板の実物色。楽器の見た目を表す例外トークンで、surface 系にはマップしない。
ダークモードでも不変。
-}
keyWhite : String
keyWhite =
    "#FFFFFF"


keyBlack : String
keyBlack =
    "#222222"


{-| ピアノロールのグリッド背景行。白鍵行/黒鍵行の対応を示すガイドで、鍵盤実物色（keyWhite/keyBlack）とは
別物。ダークでも「黒鍵行が白鍵行より暗い」関係を保つ。
-}
gridRowWhiteKey : String
gridRowWhiteKey =
    tokenVar "grid-row-white-key"


gridRowBlackKey : String
gridRowBlackKey =
    tokenVar "grid-row-black-key"



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


{-| CSS カスタムプロパティ参照（`var(--md-xxx)` 文字列）を、指定した不透明度で透明と混色する
`color-mix(...)` 文字列に変換する。SVG の `SA.fill`/`SA.stroke` や state layer など、直書きの
`rgba(...)` リテラルの代わりに使う。トークンがライト/ダークで切り替わっても、この関数自体は
トークンの実色を知らなくて良い（ブラウザの `color-mix` がその時点の実効値で計算する）。

    withAlpha 0.5 Theme.primary == "color-mix(in srgb, var(--md-primary) 50%, transparent)"

-}
withAlpha : Float -> String -> String
withAlpha alpha token =
    "color-mix(in srgb, " ++ token ++ " " ++ String.fromInt (Basics.round (alpha * 100)) ++ "%, transparent)"



-- Global CSS


{-| ページ全体に注入するグローバル CSS。`View.Style.focusCss` の後継。
reset・body のフォント/配色・:focus-visible・ボタンの state layer（`.m3-btn`）に加え、
色トークンの CSS カスタムプロパティ（`--md-xxx`）を3ブロックで定義する:

  - `:root {}` — ライト値（既定）
  - `@media (prefers-color-scheme: dark) { :root:not([data-theme=light]) {} }` — OS のダーク設定に自動追従
  - `:root[data-theme=dark] {}` — 手動トグルによる上書き（OS 設定より優先されるよう、上の media ブロックより後に置く）

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


{-| `colorTokens` から `:root {}` 用の宣言だけを取り出す（ライト値）。
-}
lightDeclarations : String
lightDeclarations =
    colorTokens
        |> List.map (\t -> "  --md-" ++ t.css ++ ": " ++ t.light ++ ";")
        |> String.join "\n"


{-| `colorTokens` から `[data-theme=dark]` 用の宣言だけを取り出す（ダーク値）。
-}
darkDeclarations : String
darkDeclarations =
    colorTokens
        |> List.map (\t -> "  --md-" ++ t.css ++ ": " ++ t.dark ++ ";")
        |> String.join "\n"


rootBlock : String
rootBlock =
    ":root {\n" ++ lightDeclarations ++ "\n}"


{-| OS のダーク設定に自動追従するブロック。`[data-theme=light]` が明示されている時だけ除外するので、
`SystemTheme`（属性なし、または "system"）でも `DarkTheme`（"dark"）でも OS がダークならここが効く。
手動 `[data-theme=dark]` 上書きより先に置く必要がある（同specificityのため、後勝ち）。
-}
darkMediaBlock : String
darkMediaBlock =
    "@media (prefers-color-scheme: dark) {\n"
        ++ ":root:not([data-theme=light]) {\n"
        ++ darkDeclarations
        ++ "\n}\n"
        ++ ":root:not([data-theme=light]) .m3-hit:hover { filter: brightness(1.15); }\n"
        ++ "}"


{-| 手動トグルで `dark` を選んだ時の上書きブロック。OS 設定に関わらずダーク配色にする。
-}
darkOverrideBlock : String
darkOverrideBlock =
    ":root[data-theme=dark] {\n"
        ++ darkDeclarations
        ++ "\n}\n"
        ++ ":root[data-theme=dark] .m3-hit:hover { filter: brightness(1.15); }"


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
    , ".touch-ui .m3-btn { padding: 0.55rem 0.9rem !important; }"
    , ".touch-ui { -webkit-touch-callout: none; -webkit-user-select: none; user-select: none; }"
    , ".touch-ui input, .touch-ui textarea, .touch-ui select { -webkit-user-select: text; user-select: text; }"
    , ".m3-hit { transition: filter 0.1s; }"
    , ".m3-hit:hover { filter: brightness(0.94); }"
    , rootBlock
    , darkMediaBlock
    , darkOverrideBlock
    ]
