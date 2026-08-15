module View.Palette exposing (globalCss, neutral, sectionColor, sectionTint)

{-| セクション/ゴーストトラックの色分けパレット。PianoRoll のセクション帯・ ChordEditor のセル背景・
 SectionBar のブロック、ゴーストトラック表示で、同じセクション/トラックは同じ色に見えるようにするために共有する。
 引数はセクションの「リスト内インデックス」（ id ではない）。

 Song Maker 風の「おもちゃ的」な高彩度パレット。sectionColor は濃色（文字/枠線用）、
 sectionTint は淡色（背景用）という tone ペアの構造は Material Design 3 の custom colors
 （拡張色）の考え方に沿うが、値自体は View.Theme のロールカラー（primary/error等）とは
意図的に切り離してある（別の CSS カスタムプロパティ名前空間 `--palette-xxx` を持つ）。

 ダーク値は、セクション色（sectionColor）は明度をやや上げて彩度を保つ方針、ティント
（sectionTint）は白と混ぜる代わりに暗い背景色と混ぜる方針で選んでいる。
-}

import Html



-- カラートークン


type alias PaletteToken =
    { css : String
    , light : String
    , dark : String
    }


paletteTokens : List PaletteToken
paletteTokens =
    [ PaletteToken "section-0" "#E8483A" "#FF7A68"
    , PaletteToken "section-1" "#D97A00" "#FFA733"
    , PaletteToken "section-2" "#3F9142" "#63B868"
    , PaletteToken "section-3" "#8452C9" "#A47EE0"
    , PaletteToken "section-4" "#D93B87" "#FF6FAE"
    , PaletteToken "section-5" "#00958A" "#12C2B4"
    , PaletteToken "tint-0" "#FFE0DB" "#3D2420"
    , PaletteToken "tint-1" "#FFE8C2" "#3D2E14"
    , PaletteToken "tint-2" "#D8F5D6" "#1F3320"
    , PaletteToken "tint-3" "#EBDDFF" "#2C2440"
    , PaletteToken "tint-4" "#FFDCEF" "#3D2030"
    , PaletteToken "tint-5" "#C7F5EE" "#123330"
    , PaletteToken "neutral" "#EBEEF3" "#1E2027"
    ]


tokenVar : String -> String
tokenVar name =
    "var(--palette-" ++ name ++ ")"



-- 公開 API


{-| ベース色。6色循回。強めの彩度で、PianoRoll のセクション帯や文字色にも使える。
-}
sectionColor : Int -> String
sectionColor i =
    tokenVar ("section-" ++ String.fromInt (modBy 6 i))


{-| 背景塗りに使う淡色版（ライトでは白と混ぜて低彩度化、ダークでは暗い背景色と混ぜて低彩度化）。
sectionColor と同じインデックス体系。
-}
sectionTint : Int -> String
sectionTint i =
    tokenVar ("tint-" ++ String.fromInt (modBy 6 i))


{-| セクションに属さない区間（末尾余白など）の地色。
-}
neutral : String
neutral =
    tokenVar "neutral"



-- Global CSS


{-| このパレットの CSS カスタムプロパティ（`--palette-xxx`）を定義するグローバル CSS。
`View.Theme.globalCss` とは別の名前空間として独立に注入する（View.Theme と値を意図的に
分離しているため）。Main.elm の view で `View.Theme.globalCss` と並べて描画すること。
-}
globalCss : Html.Html msg
globalCss =
    Html.node "style"
        []
        [ Html.text (String.join "\n" cssRules) ]


lightDeclarations : String
lightDeclarations =
    paletteTokens
        |> List.map (\t -> "  --palette-" ++ t.css ++ ": " ++ t.light ++ ";")
        |> String.join "\n"


darkDeclarations : String
darkDeclarations =
    paletteTokens
        |> List.map (\t -> "  --palette-" ++ t.css ++ ": " ++ t.dark ++ ";")
        |> String.join "\n"


cssRules : List String
cssRules =
    [ ":root {\n" ++ lightDeclarations ++ "\n}"
    , "@media (prefers-color-scheme: dark) {\n:root:not([data-theme=light]) {\n" ++ darkDeclarations ++ "\n}\n}"
    , ":root[data-theme=dark] {\n" ++ darkDeclarations ++ "\n}"
    ]
