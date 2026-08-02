module View.Palette exposing (neutral, sectionColor, sectionTint)

{-| セクションの色分けパレット。PianoRoll のセクション帯・ ChordEditor のセル背景・
 SectionBar のブロックで、同じセクションは同じ色に見えるようにするために3箇所で共有する。
 引数はセクションの「リスト内インデックス」（id ではない）。
-}


{-| ベース色。6色循回。強めの彩度で、PianoRoll のセクション帯や文字色にも使える。
-}
sectionColor : Int -> String
sectionColor i =
    case modBy 6 i of
        0 ->
            "#4a90d9"

        1 ->
            "#e67e22"

        2 ->
            "#2ecc71"

        3 ->
            "#9b59b6"

        4 ->
            "#e74c3c"

        _ ->
            "#1abc9c"


{-| 背景塗りに使う薄色版（白と混ぶことで低彩度化）。sectionColor と同じインデックス体系。
-}
sectionTint : Int -> String
sectionTint i =
    case modBy 6 i of
        0 ->
            "#eaf2fb"

        1 ->
            "#fdf0e4"

        2 ->
            "#e8f8ee"

        3 ->
            "#f3ebf7"

        4 ->
            "#fceaea"

        _ ->
            "#e3f8f4"


{-| セクションに属さない区間（末尾余白など）の地色。
-}
neutral : String
neutral =
    "#fafafa"
