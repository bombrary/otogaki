module View.Palette exposing (neutral, sectionColor, sectionTint)

{-| セクション/ゴーストトラックの色分けパレット。PianoRoll のセクション帯・ ChordEditor のセル背景・
 SectionBar のブロック、ゴーストトラック表示で、同じセクション/トラックは同じ色に見えるようにするために共有する。
 引数はセクションの「リスト内インデックス」（ id ではない）。

 Song Maker 風の「おもちゃ的」な高彩度パレット。sectionColor は濃色（文字/枠線用）、
 sectionTint は淡色（背景用）という tone ペアの構造は Material Design 3 の custom colors
 （拡張色）の考え方に沿うが、値自体は View.Theme のロールカラー（primary/error等）とは
意図的に切り離してある。
-}


{-| ベース色。6色循回。強めの彩度で、PianoRoll のセクション帯や文字色にも使える。
-}
sectionColor : Int -> String
sectionColor i =
    case modBy 6 i of
        0 ->
            "#E8483A"

        1 ->
            "#D97A00"

        2 ->
            "#3F9142"

        3 ->
            "#8452C9"

        4 ->
            "#D93B87"

        _ ->
            "#00958A"


{-| 背景塗りに使う淡色版（白と混ぶことで低彩度化）。sectionColor と同じインデックス体系。
-}
sectionTint : Int -> String
sectionTint i =
    case modBy 6 i of
        0 ->
            "#FFE0DB"

        1 ->
            "#FFE8C2"

        2 ->
            "#D8F5D6"

        3 ->
            "#EBDDFF"

        4 ->
            "#FFDCEF"

        _ ->
            "#C7F5EE"


{-| セクションに属さない区間（末尾余白など）の地色。
-}
neutral : String
neutral =
    "#EBEEF3"
