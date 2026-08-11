module Data.GuitarForm exposing
    ( Candidate
    , Form
    , Position(..)
    , StringPicks
    , Variant(..)
    , bassSuffix
    , bestForm
    , candidateLabel
    , candidateSuffix
    , candidatesFor
    , extensionsSuffix
    , forChord
    , formFromPicks
    , maxFret
    , openStrings
    , positionLabel
    , positionSuffix
    , removePicks
    , shiftPicks
    , toPitches
    )

import Data.Chord exposing (Chord, Extension(..), Quality(..))
import Data.Chord.Format as ChordFormat
import Set exposing (Set)


{-| 6弦分の押弦位置。低音弦（E）から高音弦（e）の順。`Nothing` = ミュート、`Just 0` = 開放弦。
-}
type alias Form =
    { frets : List (Maybe Int) }


openStrings : List Int
openStrings =
    [ 40, 45, 50, 55, 59, 64 ]


toPitches : Form -> List Int
toPitches f =
    List.map2 (\open fret -> Maybe.map (\fr -> open + fr) fret) openStrings f.frets
        |> List.filterMap identity


form : List (Maybe Int) -> Form
form frets =
    { frets = frets }


{-| root ピッチクラス・quality・extensions の組で完全一致したときに使う実フォーム。
extensions は `supportedExtensions` でゲートされた形（`[]` か `[ Add9 ]`）のみが実際に照合される。
-}
openChordTable : List ( ( Int, Quality, List Extension ), Form )
openChordTable =
    [ ( ( 0, Maj, [] ), form [ Nothing, Just 3, Just 2, Just 0, Just 1, Just 0 ] ) -- C
    , ( ( 9, Maj, [] ), form [ Nothing, Just 0, Just 2, Just 2, Just 2, Just 0 ] ) -- A
    , ( ( 7, Maj, [] ), form [ Just 3, Just 2, Just 0, Just 0, Just 0, Just 3 ] ) -- G
    , ( ( 4, Maj, [] ), form [ Just 0, Just 2, Just 2, Just 1, Just 0, Just 0 ] ) -- E
    , ( ( 2, Maj, [] ), form [ Nothing, Nothing, Just 0, Just 2, Just 3, Just 2 ] ) -- D
    , ( ( 9, Min, [] ), form [ Nothing, Just 0, Just 2, Just 2, Just 1, Just 0 ] ) -- Am
    , ( ( 4, Min, [] ), form [ Just 0, Just 2, Just 2, Just 0, Just 0, Just 0 ] ) -- Em
    , ( ( 2, Min, [] ), form [ Nothing, Nothing, Just 0, Just 2, Just 3, Just 1 ] ) -- Dm
    , ( ( 9, Dom7, [] ), form [ Nothing, Just 0, Just 2, Just 0, Just 2, Just 0 ] ) -- A7
    , ( ( 4, Dom7, [] ), form [ Just 0, Just 2, Just 0, Just 1, Just 0, Just 0 ] ) -- E7
    , ( ( 2, Dom7, [] ), form [ Nothing, Nothing, Just 0, Just 2, Just 1, Just 2 ] ) -- D7
    , ( ( 7, Dom7, [] ), form [ Just 3, Just 2, Just 0, Just 0, Just 0, Just 1 ] ) -- G7
    , ( ( 0, Dom7, [] ), form [ Nothing, Just 3, Just 2, Just 3, Just 1, Just 0 ] ) -- C7
    , ( ( 0, Maj7, [] ), form [ Nothing, Just 3, Just 2, Just 0, Just 0, Just 0 ] ) -- Cmaj7
    , ( ( 7, Maj7, [] ), form [ Just 3, Just 2, Just 0, Just 0, Just 0, Just 2 ] ) -- Gmaj7
    , ( ( 9, Min7, [] ), form [ Nothing, Just 0, Just 2, Just 0, Just 1, Just 0 ] ) -- Am7
    , ( ( 4, Min7, [] ), form [ Just 0, Just 2, Just 0, Just 0, Just 0, Just 0 ] ) -- Em7
    , ( ( 2, Min7, [] ), form [ Nothing, Nothing, Just 0, Just 2, Just 1, Just 1 ] ) -- Dm7
    , ( ( 0, Maj, [ Add9 ] ), form [ Nothing, Just 3, Just 2, Just 0, Just 3, Nothing ] ) -- Cadd9
    , ( ( 2, Maj, [ Add9 ] ), form [ Nothing, Just 5, Just 4, Just 2, Just 3, Just 0 ] ) -- Dadd9

    -- 優先度高
    , ( ( 11, Dom7, [] ), form [ Nothing, Just 2, Just 1, Just 2, Just 0, Just 2 ] ) -- B7
    , ( ( 9, Maj7, [] ), form [ Nothing, Just 0, Just 2, Just 1, Just 2, Just 0 ] ) -- Amaj7
    , ( ( 2, Maj7, [] ), form [ Nothing, Nothing, Just 0, Just 2, Just 2, Just 2 ] ) -- Dmaj7
    , ( ( 4, Maj7, [] ), form [ Just 0, Just 2, Just 1, Just 1, Just 0, Just 0 ] ) -- Emaj7
    , ( ( 9, Sus2, [] ), form [ Nothing, Just 0, Just 2, Just 2, Just 0, Just 0 ] ) -- Asus2
    , ( ( 2, Sus2, [] ), form [ Nothing, Nothing, Just 0, Just 2, Just 3, Just 0 ] ) -- Dsus2
    , ( ( 4, Sus4, [] ), form [ Just 0, Just 2, Just 2, Just 2, Just 0, Just 0 ] ) -- Esus4
    , ( ( 9, Sus4, [] ), form [ Nothing, Just 0, Just 2, Just 2, Just 3, Just 0 ] ) -- Asus4
    , ( ( 2, Sus4, [] ), form [ Nothing, Nothing, Just 0, Just 2, Just 3, Just 3 ] ) -- Dsus4

    -- 優先度中
    , ( ( 9, Dom7Sus4, [] ), form [ Nothing, Just 0, Just 2, Just 0, Just 3, Just 0 ] ) -- A7sus4
    , ( ( 2, Dom7Sus4, [] ), form [ Nothing, Nothing, Just 0, Just 2, Just 1, Just 3 ] ) -- D7sus4
    , ( ( 4, Dom7Sus4, [] ), form [ Just 0, Just 2, Just 0, Just 2, Just 0, Just 0 ] ) -- E7sus4
    , ( ( 7, Sixth, [] ), form [ Just 3, Just 2, Just 0, Just 0, Just 0, Just 0 ] ) -- G6
    , ( ( 0, Sixth, [] ), form [ Nothing, Just 3, Just 2, Just 2, Just 1, Just 0 ] ) -- C6
    , ( ( 2, Sixth, [] ), form [ Nothing, Nothing, Just 0, Just 2, Just 0, Just 2 ] ) -- D6
    , ( ( 9, Sixth, [] ), form [ Nothing, Just 0, Just 2, Just 2, Just 2, Just 2 ] ) -- A6
    , ( ( 9, MinSixth, [] ), form [ Nothing, Just 0, Just 2, Just 2, Just 1, Just 2 ] ) -- Am6
    , ( ( 4, MinSixth, [] ), form [ Just 0, Just 2, Just 2, Just 0, Just 2, Just 0 ] ) -- Em6
    , ( ( 2, MinSixth, [] ), form [ Nothing, Nothing, Just 0, Just 2, Just 0, Just 1 ] ) -- Dm6
    , ( ( 9, MinMaj7, [] ), form [ Nothing, Just 0, Just 2, Just 1, Just 1, Just 0 ] ) -- AmM7
    , ( ( 7, Maj, [ Add9 ] ), form [ Just 3, Just 2, Just 0, Just 2, Just 0, Just 3 ] ) -- Gadd9
    , ( ( 11, HalfDim7, [] ), form [ Nothing, Just 2, Just 0, Just 2, Just 0, Just 1 ] ) -- Bm7-5

    -- 優先度低
    , ( ( 4, Sixth, [] ), form [ Just 0, Just 2, Just 2, Just 1, Just 2, Just 0 ] ) -- E6
    , ( ( 4, MinMaj7, [] ), form [ Just 0, Just 2, Just 1, Just 0, Just 0, Just 0 ] ) -- EmM7
    , ( ( 2, MinMaj7, [] ), form [ Nothing, Nothing, Just 0, Just 2, Just 2, Just 1 ] ) -- DmM7
    , ( ( 4, Maj, [ Add9 ] ), form [ Just 0, Just 2, Just 2, Just 1, Just 0, Just 2 ] ) -- Eadd9
    , ( ( 9, Maj, [ Add9 ] ), form [ Nothing, Just 0, Just 2, Just 4, Just 2, Just 0 ] ) -- Aadd9
    , ( ( 7, Sus4, [] ), form [ Just 3, Just 3, Just 0, Just 0, Just 1, Just 3 ] ) -- Gsus4
    , ( ( 7, Sus2, [] ), form [ Just 3, Just 0, Just 0, Just 0, Just 3, Just 3 ] ) -- Gsus2
    , ( ( 0, Sus4, [] ), form [ Nothing, Just 3, Just 3, Just 0, Just 1, Just 1 ] ) -- Csus4
    , ( ( 0, Aug, [] ), form [ Nothing, Just 3, Just 2, Just 1, Just 1, Just 0 ] ) -- Caug
    , ( ( 9, Aug, [] ), form [ Nothing, Just 0, Just 3, Just 2, Just 2, Just 1 ] ) -- Aaug

    -- テンション系オープン形（Phase4）
    , ( ( 4, Dom7, [ Nine ] ), form [ Just 0, Just 2, Just 0, Just 1, Just 0, Just 2 ] ) -- E9
    , ( ( 4, Min7, [ Nine ] ), form [ Just 0, Just 2, Just 0, Just 0, Just 0, Just 2 ] ) -- Em9
    , ( ( 0, Maj7, [ Nine ] ), form [ Nothing, Just 3, Just 0, Just 0, Just 0, Just 0 ] ) -- Cmaj9
    , ( ( 4, Min7, [ Eleven ] ), form [ Just 0, Just 0, Just 0, Just 0, Just 0, Just 0 ] ) -- Em11
    , ( ( 9, Min7, [ Eleven ] ), form [ Nothing, Just 0, Just 0, Just 0, Just 1, Just 0 ] ) -- Am11
    , ( ( 2, Min7, [ Eleven ] ), form [ Nothing, Nothing, Just 0, Just 0, Just 1, Just 1 ] ) -- Dm11
    , ( ( 9, Dom7, [ Nine ] ), form [ Nothing, Just 0, Just 2, Just 4, Just 2, Just 3 ] ) -- A9
    ]


{-| ゲートしている extensions の組み合わせだけを通す。`[]`（拡張なし）、`[ Add9 ]`、
テンション系（`[ Nine ]`/`[ FlatNine ]`/`[ SharpNine ]`/`[ Eleven ]`/`[ SharpEleven ]`/`[ Thirteen ]`/
`[ FlatThirteen ]`）の単独指定のみ対応。それ以外（複数 extension 同時指定等）は `Nothing` を返し、
`candidatesFor` が空リストになって呼び出し側は `Data.Chord.toPitchesWith`/`bestForm` にフォールバックする。
-}
supportedExtensions : List Extension -> Maybe (List Extension)
supportedExtensions exts =
    case exts of
        [] ->
            Just []

        [ Add9 ] ->
            Just [ Add9 ]

        [ Nine ] ->
            Just [ Nine ]

        [ FlatNine ] ->
            Just [ FlatNine ]

        [ SharpNine ] ->
            Just [ SharpNine ]

        [ Eleven ] ->
            Just [ Eleven ]

        [ SharpEleven ] ->
            Just [ SharpEleven ]

        [ Thirteen ] ->
            Just [ Thirteen ]

        [ FlatThirteen ] ->
            Just [ FlatThirteen ]

        _ ->
            Nothing


{-| フォーム候補の「引き方の系統」。同じ Position でも複数の運指がありうる
（例: FM7 の E型はバレー形 Standard とコンパクト形 Compact の2通り）。
-}
type Variant
    = Standard
    | Compact


{-| E型（低音弦ルート）バレーコードの、rootからの相対フレットオフセット（6弦分）。
`Nothing` はその弦をミュートすることを表す（バレーの中間弦をミュートする形、あるいはバレー基準より
低いフレットに置けない場合に使う）。同じ quality/extensions でも複数の Variant を返しうる。
対応していない quality/extensions の組は空リスト。
-}
eShapeOffsets : Quality -> List Extension -> List ( Variant, List (Maybe Int) )
eShapeOffsets quality exts =
    case ( quality, exts ) of
        ( Maj, [] ) ->
            [ ( Standard, [ Just 0, Just 2, Just 2, Just 1, Just 0, Just 0 ] ) ]

        ( Min, [] ) ->
            [ ( Standard, [ Just 0, Just 2, Just 2, Just 0, Just 0, Just 0 ] ) ]

        ( Dom7, [] ) ->
            [ ( Standard, [ Just 0, Just 2, Just 0, Just 1, Just 0, Just 0 ] ) ]

        ( Min7, [] ) ->
            [ ( Standard, [ Just 0, Just 2, Just 0, Just 0, Just 0, Just 0 ] ) ]

        ( Maj7, [] ) ->
            -- Standard: FM7 = 1x3210 / Compact: FM7 = 1x221x
            [ ( Standard, [ Just 0, Nothing, Just 2, Just 1, Just 0, Just -1 ] )
            , ( Compact, [ Just 0, Nothing, Just 1, Just 1, Just 0, Nothing ] )
            ]

        ( HalfDim7, [] ) ->
            -- Bm7-5 = 7x776x
            [ ( Standard, [ Just 0, Nothing, Just 0, Just 0, Just -1, Nothing ] ) ]

        ( Dim7, [] ) ->
            -- Bdim7 = 7x676x
            [ ( Standard, [ Just 0, Nothing, Just -1, Just 0, Just -1, Nothing ] ) ]

        ( Maj, [ Add9 ] ) ->
            -- Fadd9 別形 = 1x3213
            [ ( Standard, [ Just 0, Nothing, Just 2, Just 1, Just 0, Just 2 ] ) ]

        ( Power, [] ) ->
            -- G5 = 355xxx
            [ ( Standard, [ Just 0, Just 2, Just 2, Nothing, Nothing, Nothing ] ) ]

        ( Sus4, [] ) ->
            -- F#sus4 = 244422
            [ ( Standard, [ Just 0, Just 2, Just 2, Just 2, Just 0, Just 0 ] ) ]

        ( Dom7Sus4, [] ) ->
            -- G7sus4 = 353533
            [ ( Standard, [ Just 0, Just 2, Just 0, Just 2, Just 0, Just 0 ] ) ]

        ( Sixth, [] ) ->
            -- G6 = 3x243x
            [ ( Standard, [ Just 0, Nothing, Just -1, Just 1, Just 0, Nothing ] ) ]

        ( MinSixth, [] ) ->
            -- Gm6 = 3x233x
            [ ( Standard, [ Just 0, Nothing, Just -1, Just 0, Just 0, Nothing ] ) ]

        ( MinMaj7, [] ) ->
            -- FmM7 = 132111
            [ ( Standard, [ Just 0, Just 2, Just 1, Just 0, Just 0, Just 0 ] ) ]

        ( Aug, [] ) ->
            -- Gaug = 3x544x
            [ ( Standard, [ Just 0, Nothing, Just 2, Just 1, Just 1, Nothing ] ) ]

        ( Dom7, [ Thirteen ] ) ->
            -- G13 = 3x3450... 検算例: 3x3450 → G,F,B,E
            [ ( Standard, [ Just 0, Nothing, Just 0, Just 1, Just 2, Nothing ] ) ]

        ( Dom7, [ FlatThirteen ] ) ->
            -- G7b13 = 3x344x（7#5シェル）
            [ ( Standard, [ Just 0, Nothing, Just 0, Just 1, Just 1, Nothing ] ) ]

        ( Maj7, [ Thirteen ] ) ->
            -- Gmaj13 = 3x445x
            [ ( Standard, [ Just 0, Nothing, Just 1, Just 1, Just 2, Nothing ] ) ]

        ( Maj7, [ SharpEleven ] ) ->
            -- Gmaj7#11 = 3x442x
            [ ( Standard, [ Just 0, Nothing, Just 1, Just 1, Just -1, Nothing ] ) ]

        ( Min7, [ Thirteen ] ) ->
            -- Gm13 = 3x335x
            [ ( Standard, [ Just 0, Nothing, Just 0, Just 0, Just 2, Nothing ] ) ]

        _ ->
            []


{-| A型（A弦ルート）バレーコードの、rootからの相対フレットオフセット（5弦分、低音弦はミュート）。
-}
aShapeOffsets : Quality -> List Extension -> List ( Variant, List (Maybe Int) )
aShapeOffsets quality exts =
    case ( quality, exts ) of
        ( Maj, [] ) ->
            [ ( Standard, [ Just 0, Just 2, Just 2, Just 2, Just 0 ] ) ]

        ( Min, [] ) ->
            [ ( Standard, [ Just 0, Just 2, Just 2, Just 1, Just 0 ] ) ]

        ( Dom7, [] ) ->
            [ ( Standard, [ Just 0, Just 2, Just 0, Just 2, Just 0 ] ) ]

        ( Min7, [] ) ->
            [ ( Standard, [ Just 0, Just 2, Just 0, Just 1, Just 0 ] ) ]

        ( Maj7, [] ) ->
            [ ( Standard, [ Just 0, Just 2, Just 1, Just 2, Just 0 ] ) ]

        ( HalfDim7, [] ) ->
            -- Bm7-5 = x2323x
            [ ( Standard, [ Just 0, Just 1, Just 0, Just 1, Nothing ] ) ]

        ( Dim7, [] ) ->
            -- Bdim7 = x2313x
            [ ( Standard, [ Just 0, Just 1, Just -1, Just 1, Nothing ] ) ]

        ( Power, [] ) ->
            -- C5 = x355xx
            [ ( Standard, [ Just 0, Just 2, Just 2, Nothing, Nothing ] ) ]

        ( Sus2, [] ) ->
            -- Bsus2 = x24422
            [ ( Standard, [ Just 0, Just 2, Just 2, Just 0, Just 0 ] ) ]

        ( Sus4, [] ) ->
            -- Bsus4 = x24452
            [ ( Standard, [ Just 0, Just 2, Just 2, Just 3, Just 0 ] ) ]

        ( Dom7Sus4, [] ) ->
            -- B7sus4 = x24252
            [ ( Standard, [ Just 0, Just 2, Just 0, Just 3, Just 0 ] ) ]

        ( Sixth, [] ) ->
            -- C6 = x35555
            [ ( Standard, [ Just 0, Just 2, Just 2, Just 2, Just 2 ] ) ]

        ( MinSixth, [] ) ->
            -- Cm6 = x31213
            [ ( Standard, [ Just 0, Just -2, Just -1, Just -2, Just 0 ] ) ]

        ( MinMaj7, [] ) ->
            -- CmM7 = x35443
            [ ( Standard, [ Just 0, Just 2, Just 1, Just 1, Just 0 ] ) ]

        ( Dom7, [ Nine ] ) ->
            -- C9 = x32333
            [ ( Standard, [ Just 0, Just -1, Just 0, Just 0, Just 0 ] ) ]

        ( Dom7, [ FlatNine ] ) ->
            -- C7b9 = x3232x
            [ ( Standard, [ Just 0, Just -1, Just 0, Just -1, Nothing ] ) ]

        ( Dom7, [ SharpNine ] ) ->
            -- C7#9 = x3234x（Hendrixコード）
            [ ( Standard, [ Just 0, Just -1, Just 0, Just 1, Nothing ] ) ]

        ( Dom7, [ Eleven ] ) ->
            -- C11 = x3333x（9susグリップ）
            [ ( Standard, [ Just 0, Just 0, Just 0, Just 0, Nothing ] ) ]

        ( Dom7, [ SharpEleven ] ) ->
            -- C7#11 = x3435x
            [ ( Standard, [ Just 0, Just 1, Just 0, Just 2, Nothing ] ) ]

        ( Maj7, [ Nine ] ) ->
            -- Cmaj9 = x3243x
            [ ( Standard, [ Just 0, Just -1, Just 1, Just 0, Nothing ] ) ]

        ( Min7, [ Nine ] ) ->
            -- Cm9 = x31333
            [ ( Standard, [ Just 0, Just -2, Just 0, Just 0, Just 0 ] ) ]

        ( Min7, [ Eleven ] ) ->
            -- Cm11 = x31311
            [ ( Standard, [ Just 0, Just -2, Just 0, Just -2, Just -2 ] ) ]

        _ ->
            []


{-| D型（4弦=D弦ルート）フォームの、rootからの相対フレットオフセット（D/G/B/e の4弦分。
低音のE/A弦はミュート）。base は D弦（開放ピッチクラス 2）から見た root の位置なので
`modBy 12 (rootPc - 2)`。GM7 の「弾けない６弦バレー形」の代替として実機フィードバックで追加された。
-}
dShapeOffsets : Quality -> List Extension -> List ( Variant, List (Maybe Int) )
dShapeOffsets quality exts =
    case ( quality, exts ) of
        ( Maj7, [] ) ->
            -- GM7 4弦ルート = xx5432
            [ ( Standard, [ Just 0, Just -1, Just -2, Just -3 ] ) ]

        ( Maj, [ Add9 ] ) ->
            -- Fadd9 4弦ルート = xx3213
            [ ( Standard, [ Just 0, Just -1, Just -2, Just 0 ] ) ]

        ( Maj, [] ) ->
            -- F 4弦ルート = xx3565
            [ ( Standard, [ Just 0, Just 2, Just 3, Just 2 ] ) ]

        ( Dom7, [] ) ->
            -- F7 4弦ルート = xx3545
            [ ( Standard, [ Just 0, Just 2, Just 1, Just 2 ] ) ]

        ( Min7, [] ) ->
            -- Fm7 4弦ルート = xx3544
            [ ( Standard, [ Just 0, Just 2, Just 1, Just 1 ] ) ]

        ( HalfDim7, [] ) ->
            -- Fm7-5 4弦ルート = xx3444
            [ ( Standard, [ Just 0, Just 1, Just 1, Just 1 ] ) ]

        ( Dim7, [] ) ->
            -- D#dim7 4弦ルート = xx1212
            [ ( Standard, [ Just 0, Just 1, Just 0, Just 1 ] ) ]

        ( Min, [] ) ->
            -- Fm 4弦ルート = xx3564
            [ ( Standard, [ Just 0, Just 2, Just 3, Just 1 ] ) ]

        ( Sixth, [] ) ->
            -- F6 4弦ルート = xx3535
            [ ( Standard, [ Just 0, Just 2, Just 0, Just 2 ] ) ]

        ( MinMaj7, [] ) ->
            -- FmM7 4弦ルート = xx3554
            [ ( Standard, [ Just 0, Just 2, Just 2, Just 1 ] ) ]

        ( Sus2, [] ) ->
            -- Esus2（可動） 4弦ルート = xx2452
            [ ( Standard, [ Just 0, Just 2, Just 3, Just 0 ] ) ]

        ( Sus4, [] ) ->
            -- Esus4（可動） 4弦ルート = xx2455
            [ ( Standard, [ Just 0, Just 2, Just 3, Just 3 ] ) ]

        ( Power, [] ) ->
            -- F5 4弦ルート = xx356x
            [ ( Standard, [ Just 0, Just 2, Just 3, Nothing ] ) ]

        _ ->
            []


{-| E型/A型/D型シェイプの共通組み立てヘルパー。`prefix` はシェイプが弾かない低音弦の分（E型なら
`[]`、A型なら低音Eをミュートする `[ Nothing ]`、D型なら低音E/Aをミュートする `[ Nothing, Nothing ]`）、
`offsets` はシェイプ本体のオフセット。`base + offset` が負のフレットになる弦があれば
（バレー基準より低いオフセットを含む Maj7/HalfDim7/Dim7 で起こりうる）、押弦不可能なのでフォーム全体を
1オクターブ（+12）押し上げる。
-}
shapeForm : Int -> List (Maybe Int) -> List (Maybe Int) -> Form
shapeForm base prefix offsets =
    let
        raw =
            prefix ++ List.map (Maybe.map ((+) base)) offsets

        liftNeeded =
            raw
                |> List.filterMap identity
                |> List.minimum
                |> Maybe.map (\m -> m < 0)
                |> Maybe.withDefault False
    in
    if liftNeeded then
        form (List.map (Maybe.map ((+) 12)) raw)

    else
        form raw


eShapeForms : Int -> Quality -> List Extension -> List ( Variant, Form )
eShapeForms rootPc quality exts =
    eShapeOffsets quality exts
        |> List.map (\( variant, offsets ) -> ( variant, shapeForm (modBy 12 (rootPc - 4)) [] offsets ))


aShapeForms : Int -> Quality -> List Extension -> List ( Variant, Form )
aShapeForms rootPc quality exts =
    aShapeOffsets quality exts
        |> List.map (\( variant, offsets ) -> ( variant, shapeForm (modBy 12 (rootPc - 9)) [ Nothing ] offsets ))


dShapeForms : Int -> Quality -> List Extension -> List ( Variant, Form )
dShapeForms rootPc quality exts =
    dShapeOffsets quality exts
        |> List.map (\( variant, offsets ) -> ( variant, shapeForm (modBy 12 (rootPc - 2)) [ Nothing, Nothing ] offsets ))


{-| コードのベース音（`chord.bass`）を root からの半音インターバル（1〜11）として返す。bass が無い、
または bass のピッチクラスが root と同じ（例: C/C）なら `Nothing`（従来経路＝スラッシュ扱いしない）。
転回形（interval が構成音の一部）・ハイブリッドコード（構成音外）・blackadder（`(Aug, 2)`）は
すべてこの「ベース・インターバル」で統一的に扱える。
-}
bassInterval : Chord -> Maybe Int
bassInterval chord =
    chord.bass
        |> Maybe.map (\b -> modBy 12 (b - chord.root))
        |> Maybe.andThen
            (\i ->
                if i == 0 then
                    Nothing

                else
                    Just i
            )


{-| `( rootPc, Quality, bassInterval )` の組で完全一致したときに使うオープン形（頻出の定番スラッシュ・
ハイブリッドコード）。転回形（1st/2nd inversion）とハイブリッドコード（F/G 等）の両方をここに集める。
blackadder は可動シェイプのみで足りるため、このテーブルへの追加は無い。
-}
openSlashTable : List ( ( Int, Quality, Int ), Form )
openSlashTable =
    [ -- 転回形: /3rd（1st inversion, Maj）
      ( ( 0, Maj, 4 ), form [ Just 0, Just 3, Just 2, Just 0, Just 1, Just 0 ] ) -- C/E
    , ( ( 2, Maj, 4 ), form [ Just 2, Just 0, Just 0, Just 2, Just 3, Just 2 ] ) -- D/F#
    , ( ( 4, Maj, 4 ), form [ Just 4, Nothing, Just 2, Just 1, Just 0, Just 0 ] ) -- E/G#
    , ( ( 7, Maj, 4 ), form [ Nothing, Just 2, Just 0, Just 0, Just 3, Just 3 ] ) -- G/B
    , ( ( 9, Maj, 4 ), form [ Nothing, Just 4, Just 2, Just 2, Just 2, Just 0 ] ) -- A/C#
    , ( ( 5, Maj, 4 ), form [ Nothing, Just 0, Just 3, Just 2, Just 1, Just 1 ] ) -- F/A

    -- 転回形: /5th（2nd inversion, Maj）
    , ( ( 0, Maj, 7 ), form [ Just 3, Just 3, Just 2, Just 0, Just 1, Just 0 ] ) -- C/G
    , ( ( 2, Maj, 7 ), form [ Nothing, Just 0, Just 0, Just 2, Just 3, Just 2 ] ) -- D/A
    , ( ( 7, Maj, 7 ), form [ Nothing, Nothing, Just 0, Just 0, Just 0, Just 3 ] ) -- G/D
    , ( ( 9, Maj, 7 ), form [ Just 0, Just 0, Just 2, Just 2, Just 2, Just 0 ] ) -- A/E
    , ( ( 5, Maj, 7 ), form [ Nothing, Just 3, Just 3, Just 2, Just 1, Just 1 ] ) -- F/C
    , ( ( 4, Maj, 7 ), form [ Nothing, Just 2, Just 2, Just 1, Just 0, Just 0 ] ) -- E/B

    -- 転回形: Min
    , ( ( 9, Min, 3 ), form [ Nothing, Just 3, Just 2, Just 2, Just 1, Just 0 ] ) -- Am/C
    , ( ( 2, Min, 3 ), form [ Just 1, Nothing, Just 0, Just 2, Just 3, Just 1 ] ) -- Dm/F
    , ( ( 4, Min, 3 ), form [ Just 3, Nothing, Just 2, Just 0, Just 3, Just 0 ] ) -- Em/G（5th省略）
    , ( ( 9, Min, 7 ), form [ Just 0, Just 0, Just 2, Just 2, Just 1, Just 0 ] ) -- Am/E
    , ( ( 2, Min, 7 ), form [ Nothing, Just 0, Just 0, Just 2, Just 3, Just 1 ] ) -- Dm/A
    , ( ( 4, Min, 7 ), form [ Nothing, Just 2, Just 2, Just 0, Just 0, Just 0 ] ) -- Em/B

    -- 転回形: Dom7
    , ( ( 2, Dom7, 4 ), form [ Just 2, Nothing, Just 0, Just 2, Just 1, Just 2 ] ) -- D7/F#
    , ( ( 7, Dom7, 4 ), form [ Nothing, Just 2, Just 0, Just 0, Just 0, Just 1 ] ) -- G7/B
    , ( ( 4, Dom7, 4 ), form [ Just 4, Nothing, Just 0, Just 1, Just 0, Just 0 ] ) -- E7/G#
    , ( ( 9, Dom7, 7 ), form [ Just 0, Just 0, Just 2, Just 0, Just 2, Just 0 ] ) -- A7/E
    , ( ( 2, Dom7, 7 ), form [ Nothing, Just 0, Just 0, Just 2, Just 1, Just 2 ] ) -- D7/A
    , ( ( 4, Dom7, 7 ), form [ Nothing, Just 2, Just 2, Just 1, Just 3, Just 0 ] ) -- E7/B

    -- 転回形: Maj7
    , ( ( 0, Maj7, 4 ), form [ Just 0, Just 3, Just 2, Just 0, Just 0, Just 0 ] ) -- CM7/E
    , ( ( 7, Maj7, 4 ), form [ Nothing, Just 2, Just 0, Just 0, Just 0, Just 2 ] ) -- GM7/B
    , ( ( 0, Maj7, 7 ), form [ Just 3, Just 3, Just 2, Just 0, Just 0, Just 0 ] ) -- CM7/G
    , ( ( 7, Maj7, 7 ), form [ Nothing, Nothing, Just 0, Just 0, Just 0, Just 2 ] ) -- GM7/D
    , ( ( 2, Maj7, 7 ), form [ Nothing, Just 0, Just 0, Just 2, Just 2, Just 2 ] ) -- DM7/A

    -- 転回形: Min7
    , ( ( 9, Min7, 7 ), form [ Just 0, Just 0, Just 2, Just 0, Just 1, Just 0 ] ) -- Am7/E
    , ( ( 4, Min7, 7 ), form [ Nothing, Just 2, Just 0, Just 0, Just 3, Just 0 ] ) -- Em7/B
    , ( ( 2, Min7, 7 ), form [ Nothing, Just 0, Just 0, Just 2, Just 1, Just 1 ] ) -- Dm7/A

    -- ハイブリッドコード（Phase2）
    , ( ( 5, Maj, 2 ), form [ Just 3, Nothing, Just 3, Just 2, Just 1, Just 1 ] ) -- F/G
    , ( ( 2, Min7, 5 ), form [ Just 3, Nothing, Just 0, Just 2, Just 1, Just 1 ] ) -- Dm7/G
    , ( ( 7, Maj, 5 ), form [ Nothing, Just 3, Just 0, Just 0, Just 0, Just 3 ] ) -- G/C
    , ( ( 2, Maj, 10 ), form [ Nothing, Just 3, Just 0, Just 2, Just 3, Just 2 ] ) -- D/C
    , ( ( 0, Maj, 2 ), form [ Nothing, Nothing, Just 0, Just 5, Just 5, Just 3 ] ) -- C/D
    , ( ( 9, Maj, 10 ), form [ Just 3, Nothing, Just 2, Just 2, Just 2, Just 0 ] ) -- A/G
    , ( ( 2, Maj, 5 ), form [ Just 3, Nothing, Just 0, Just 2, Just 3, Just 2 ] ) -- D/G
    ]


{-| E型（6弦バス）の可動スラッシュシェイプ。offset は「ベース音を置く6弦のフレット」基準
（`base = modBy 12 (rootPc + interval - 4)`）。対応していない quality/interval の組は空リスト。
-}
eSlashOffsets : Quality -> Int -> List ( Variant, List (Maybe Int) )
eSlashOffsets quality interval =
    case ( quality, interval ) of
        ( Dom7, 4 ) ->
            -- 9th系グリップ（例: G7/B = 7x5767）
            [ ( Standard, [ Just 0, Nothing, Just -2, Just 0, Just -1, Just 0 ] ) ]

        ( Maj, 2 ) ->
            -- V9sus グリップ（例: F/G = 3x321x）
            [ ( Standard, [ Just 0, Nothing, Just 0, Just -1, Just -2, Nothing ] ) ]

        ( Min7, 5 ) ->
            -- V11 / V9sus グリップ（例: Dm7/G = 3x0211）
            [ ( Standard, [ Just 0, Nothing, Just -3, Just -1, Just -2, Just -2 ] ) ]

        ( Maj, 5 ) ->
            -- maj9(no3) グリップ（例: D/G = 3x0232）
            [ ( Standard, [ Just 0, Nothing, Just -3, Just -1, Just 0, Just -1 ] ) ]

        ( Maj, 10 ) ->
            -- ドミナント7th第3転回の響き（例: A/G = 3x2220）
            [ ( Standard, [ Just 0, Nothing, Just -1, Just -1, Just -1, Just -3 ] ) ]

        ( Aug, 2 ) ->
            -- blackadder コード6弦バス（例: Eaug/F# = 2x211x）
            [ ( Standard, [ Just 0, Nothing, Just 0, Just -1, Just -1, Nothing ] ) ]

        _ ->
            []


{-| A型（5弦バス）の可動スラッシュシェイプ。offset は「ベース音を置く5弦のフレット」基準
（`base = modBy 12 (rootPc + interval - 9)`）。
-}
aSlashOffsets : Quality -> Int -> List ( Variant, List (Maybe Int) )
aSlashOffsets quality interval =
    case ( quality, interval ) of
        ( Maj, 4 ) ->
            [ ( Standard, [ Just 0, Just -2, Just -2, Just -2, Nothing ] ) ]

        ( Min, 3 ) ->
            [ ( Standard, [ Just 0, Just -1, Just -1, Just -2, Nothing ] ) ]

        ( Min7, 3 ) ->
            -- C6バレーの再解釈（例: Am7/C = x35555）
            [ ( Standard, [ Just 0, Just 2, Just 2, Just 2, Just 2 ] ) ]

        ( Maj, 10 ) ->
            -- ドミナント7th第3転回の響き（例: D/C = x30212）
            [ ( Standard, [ Just 0, Just -3, Just -1, Just -2, Just -1 ] ) ]

        ( Aug, 2 ) ->
            -- blackadder コード5弦バス（例: G#aug/Bb = x1211x）
            [ ( Standard, [ Just 0, Just 1, Just 0, Just 0, Nothing ] ) ]

        _ ->
            []


{-| D型（4弦バス）の可動スラッシュシェイプ。offset は「ベース音を置く4弦のフレット」基準
（`base = modBy 12 (rootPc + interval - 2)`）。
-}
dSlashOffsets : Quality -> Int -> List ( Variant, List (Maybe Int) )
dSlashOffsets quality interval =
    case ( quality, interval ) of
        ( Maj, 4 ) ->
            [ ( Standard, [ Just 0, Just -2, Just -1, Just -2 ] ) ]

        ( Min, 3 ) ->
            [ ( Standard, [ Just 0, Just -1, Just 0, Just -2 ] ) ]

        ( Aug, 2 ) ->
            -- blackadder コード4弦バス（例: Dbaug/Eb = xx1221）
            [ ( Standard, [ Just 0, Just 1, Just 1, Just 0 ] ) ]

        _ ->
            []


maxFret : Form -> Int
maxFret f =
    f.frets |> List.filterMap identity |> List.maximum |> Maybe.withDefault 0


{-| フォーム候補の「どのポジションか」。指板上どの弦をルートに据えるかを表す。
-}
type Position
    = Open
    | ERoot
    | ARoot
    | DRoot


{-| コードを鳴らすためのフォーム候補1件。UI の選択肢はこの型のリストとして提示する。
`variant` は同じ position 内での運指の系統（Standard/Compact）を区別する。
-}
type alias Candidate =
    { position : Position, variant : Variant, form : Form }


shapeCandidates : Position -> List ( Variant, Form ) -> List Candidate
shapeCandidates position variantForms =
    List.map (\( variant, f ) -> { position = position, variant = variant, form = f }) variantForms


{-| root/quality/extensions（スラッシュ・ bass を無視した通常経路）から引けるフォーム候補を全て返す。
Open（オープンコード表）→ E型バレー → A型バレー → D型（4弦ルート） の順。extensions は
`supportedExtensions` でゲートされ、非対応（例: 複数 extension 同時指定）は空リストになる。
`candidatesFor` の本体（bass 無し・ alterations 無しのときの唯一の経路であり、bass 指定時も
未対応スラッシュのフォールバック先として使う。
-}
rootCandidates : Int -> Quality -> List Extension -> List Candidate
rootCandidates root quality exts =
    let
        -- Dim（トライアド）は実務上 Dim7 の押さえ方を代用するのが慣習なので、
        -- テーブル・シェイプ検索の直前で Dim7 に正規化する。
        normalizedQuality =
            if quality == Dim then
                Dim7

            else
                quality

        openCandidates =
            openChordTable
                |> List.filter (\( key, _ ) -> key == ( root, normalizedQuality, exts ))
                |> List.map (\( _, f ) -> { position = Open, variant = Standard, form = f })
    in
    openCandidates
        ++ shapeCandidates ERoot (eShapeForms root normalizedQuality exts)
        ++ shapeCandidates ARoot (aShapeForms root normalizedQuality exts)
        ++ shapeCandidates DRoot (dShapeForms root normalizedQuality exts)


{-| bass interval（`bassInterval` が返す root からの半音差）を伴うフォーム候補を全て返す。
転回形（interval が構成音の一部）・ハイブリッドコード（構成音外）・blackadder（`Aug` + interval 2）を
すべて同じ仅組みで扱う。openSlashTable（オープン形の完全一致）→ E/A/D 型の可動スラッシュシェイプ →
/5th（2nd inversion。既存 `aShapeOffsets` の先頭に `Just 0` を足し6弦をベースと同フレットで鳣らす
再利用則。Maj/Min/Dom7/Maj7/Min7 のみ）の順に集める。該当が無ければ空リスト（呼び出し側が
`rootCandidates` にフォールバックする）。
-}
slashCandidates : Int -> Quality -> Int -> List Candidate
slashCandidates rootPc quality interval =
    let
        normalizedQuality =
            if quality == Dim then
                Dim7

            else
                quality

        openCandidates =
            openSlashTable
                |> List.filter (\( key, _ ) -> key == ( rootPc, normalizedQuality, interval ))
                |> List.map (\( _, f ) -> { position = Open, variant = Standard, form = f })

        eBase =
            modBy 12 (rootPc + interval - 4)

        aBase =
            modBy 12 (rootPc + interval - 9)

        dBase =
            modBy 12 (rootPc + interval - 2)

        eCandidates =
            eSlashOffsets normalizedQuality interval
                |> List.map (\( variant, offsets ) -> { position = ERoot, variant = variant, form = shapeForm eBase [] offsets })

        aCandidates =
            aSlashOffsets normalizedQuality interval
                |> List.map (\( variant, offsets ) -> { position = ARoot, variant = variant, form = shapeForm aBase [ Nothing ] offsets })

        dCandidates =
            dSlashOffsets normalizedQuality interval
                |> List.map (\( variant, offsets ) -> { position = DRoot, variant = variant, form = shapeForm dBase [ Nothing, Nothing ] offsets })

        fifthReuseCandidates =
            if interval == 7 && List.member normalizedQuality [ Maj, Min, Dom7, Maj7, Min7 ] then
                aShapeOffsets normalizedQuality []
                    |> List.map
                        (\( variant, offsets ) ->
                            { position = ERoot
                            , variant = variant
                            , form = shapeForm eBase [] (Just 0 :: offsets)
                            }
                        )

            else
                []
    in
    openCandidates ++ eCandidates ++ aCandidates ++ dCandidates ++ fifthReuseCandidates


{-| コードの root/quality/extensions/bass から引けるフォーム候補を全て返す。extensions は
`supportedExtensions` でゲートされ、非対応（例: 複数 extension 同時指定）は空リストになる。
bass が構成音・非構成音のどちらでも `bassInterval`/`slashCandidates` で転回形・ハイブリッド・
blackadder を統一的に処理する（テンションと bass の併用や未対応スラッシュは `rootCandidates` の
通常経路にフォールバックし、`Data.StrumExpand.withSlashBass` がベース音を後付けする）。
呼び出し側は `forChord`（後方互換の単一候補版）か、ここで複数候補から選ばせるかを選べる。
-}
candidatesFor : Chord -> List Candidate
candidatesFor chord =
    if chord.alterations /= [] then
        []

    else
        case supportedExtensions chord.extensions of
            Nothing ->
                []

            Just exts ->
                case ( bassInterval chord, exts ) of
                    ( Just interval, [] ) ->
                        case slashCandidates chord.root chord.quality interval of
                            [] ->
                                rootCandidates chord.root chord.quality []

                            candidates ->
                                candidates

                    _ ->
                        rootCandidates chord.root chord.quality exts


positionLabel : Position -> String
positionLabel position =
    case position of
        Open ->
            "オープン"

        ERoot ->
            "6弦ルート"

        ARoot ->
            "5弦ルート"

        DRoot ->
            "4弦ルート"


positionSuffix : Position -> String
positionSuffix position =
    case position of
        Open ->
            "open"

        ERoot ->
            "6弦"

        ARoot ->
            "5弦"

        DRoot ->
            "4弦"


{-| UI 表示用のラベル。Standard は `positionLabel` のまま、Compact は「（コンパクト）」を付け足して区別する。
-}
candidateLabel : Candidate -> String
candidateLabel candidate =
    case candidate.variant of
        Standard ->
            positionLabel candidate.position

        Compact ->
            positionLabel candidate.position ++ "（コンパクト）"


{-| 保存名（ボイシング名）に使うサフィックス。Standard/Compact で異なる文字列を返すことで、
同じ Position に複数候補があっても保存名が衝突しない。
-}
candidateSuffix : Candidate -> String
candidateSuffix candidate =
    case candidate.variant of
        Standard ->
            positionSuffix candidate.position

        Compact ->
            positionSuffix candidate.position ++ "コンパクト"


{-| extensions を保存名に含めるためのサフィックス。`supportedExtensions` がゲートする範囲だけを
想定しているが、非対応の組み合わせが渡っても例外は投げず空文字を返す。
-}
extensionsSuffix : List Extension -> String
extensionsSuffix exts =
    case exts of
        [] ->
            ""

        [ Add9 ] ->
            "add9"

        [ Nine ] ->
            "9"

        [ FlatNine ] ->
            "b9"

        [ SharpNine ] ->
            "#9"

        [ Eleven ] ->
            "11"

        [ SharpEleven ] ->
            "#11"

        [ Thirteen ] ->
            "13"

        [ FlatThirteen ] ->
            "b13"

        _ ->
            ""


{-| bass ピッチクラスを保存名に含めるためのサフィックス。`Just b` なら "on" + ピッチ名（例: "onE"）、
`Nothing` なら空文字。C と C/E のように bass の有無だけが違うコードの保存名が衝突しないようにする。
-}
bassSuffix : Maybe Int -> String
bassSuffix maybeBass =
    case maybeBass of
        Just b ->
            "on" ++ ChordFormat.pitchName False b

        Nothing ->
            ""


{-| コードの root/quality/extensions からフォームを引く。`candidatesFor` の後方互換ラッパー：
オープンコードがあれば最優先、なければ maxFret が最小の候補（同点なら Open → E型 → A型 → D型、
かつ各シェイプ内では Standard → Compact の順で安定ソートした先頭）を返す。どちらも引けない場合は
Nothing（呼び出し側は `Data.Chord.toPitchesWith` にフォールバックする）。
-}
forChord : Chord -> Maybe Form
forChord chord =
    case candidatesFor chord of
        [] ->
            Nothing

        candidates ->
            case List.filter (\c -> c.position == Open) candidates |> List.head of
                Just openCand ->
                    Just openCand.form

                Nothing ->
                    candidates
                        |> List.sortBy (\c -> maxFret c.form)
                        |> List.head
                        |> Maybe.map .form


maxFretSearch : Int
maxFretSearch =
    15


{-| 弦のリストと代入すべきピッチのリストから、全ピッチを使い切る完全割当を全探索する。
弦ごとに「ミュート」または「残りのいずれかのピッチを押弦」を選ぶ分岐を総当たりする。
弦で６、候補音高高々数個の小規模な探索空間なので素朴な全探索で十分。
-}
assignStrings : List Int -> List Int -> List (List (Maybe Int))
assignStrings strings pitches =
    case strings of
        [] ->
            if List.isEmpty pitches then
                [ [] ]

            else
                []

        openPitch :: restStrings ->
            let
                muteBranch =
                    assignStrings restStrings pitches |> List.map ((::) Nothing)

                assignBranches =
                    pitches
                        |> List.filter (\p -> let fret = p - openPitch in fret >= 0 && fret <= maxFretSearch)
                        |> List.concatMap
                            (\p ->
                                let
                                    fret =
                                        p - openPitch

                                    remaining =
                                        List.filter ((/=) p) pitches
                                in
                                assignStrings restStrings remaining |> List.map ((::) (Just fret))
                            )
            in
            muteBranch ++ assignBranches


{-| フォームの「押弦しやすさ」を比較するキー。(フレットの散らばり, 使用フレットの合計) の順で小さい方が良い。
開放弦（fret=0）は散らばりの計算からは除外し、押弦位置の集中度だけを見る。
-}
formScore : Form -> ( Int, Int )
formScore f =
    let
        frets =
            List.filterMap identity f.frets

        pressedFrets =
            List.filter ((/=) 0) frets

        span =
            case ( List.maximum pressedFrets, List.minimum pressedFrets ) of
                ( Just hi, Just lo ) ->
                    hi - lo

                _ ->
                    0

        total =
            List.sum frets
    in
    ( span, total )


{-| 任意のピッチ列を弦・フレットへ割り当てる。同じ音が複数の弦で鳴らせるため解は一意ではなく、
「フレットの散らばりが小さい」「開放弦を優先する」を基準に1つ選ぶ。
6音を超える、またはどの弦にも収まらない音がある場合は Nothing。
-}
bestForm : List Int -> Maybe Form
bestForm pitches =
    let
        uniquePitches =
            List.foldl (\p acc -> if List.member p acc then acc else acc ++ [ p ]) [] pitches
    in
    if List.isEmpty uniquePitches || List.length uniquePitches > List.length openStrings then
        Nothing

    else
        assignStrings openStrings uniquePitches
            |> List.map form
            |> List.sortBy formScore
            |> List.head


{-| ボイシング編集中の「どの弦のどのフレットで押したか」という一時的な運指メモ。
(offset, 弦インデックス) の集合で持つ。offset で持つのは、試聴キーを変えても
フレット番号を作り直さずに済むため（描画時に `rootPitch + offset - 開放弦ピッチ` でフレットを求めればよい）。
`Voicing` の型・保存 JSON とは無関係（保存しない）。
-}
type alias StringPicks =
    Set ( Int, Int )


{-| 選択中の offset に紐づく運指メモを delta 分ずらす。`Data.Voicing.shiftOffsets` と同じクランプを掛ける。
行き先が重複したら（同じ弦で同じ offset）Set が自然に1つにまとめてくれる。offset の下限は -12（ベース音が
root より1オクターブ低い転回形・ハイブリッドコードを許容するため）。
-}
shiftPicks : Int -> Int -> Set Int -> StringPicks -> StringPicks
shiftPicks delta maxOffset selected picks =
    picks
        |> Set.map
            (\( o, s ) ->
                if Set.member o selected then
                    ( clamp -12 maxOffset (o + delta), s )

                else
                    ( o, s )
            )


{-| 消えた offset の運指メモを捨てる。残すと、同じ offset を後から鍵盤で足し直したときに
古い青丸が復活してしまう。
-}
removePicks : Set Int -> StringPicks -> StringPicks
removePicks selected picks =
    Set.filter (\( o, _ ) -> not (Set.member o selected)) picks


{-| 保存済みの運指メモから Form を組み立てる。offsets 全てに運指が割り当てられている
場合のみ Just を返す（一部しか運指がなければ、どの弦にするかを勢いで決めず `bestForm` にフォールバックさせる）。
同じ弦に複数の運指が重なっていたら最初の1つを採用する。
-}
formFromPicks : Int -> List Int -> StringPicks -> Maybe Form
formFromPicks rootPitch offsets picks =
    let
        pickList =
            Set.toList picks

        coversAll =
            not (List.isEmpty offsets)
                && List.all (\o -> List.any (\( po, _ ) -> po == o) pickList) offsets

        stringOpenPitch s =
            List.drop s openStrings |> List.head |> Maybe.withDefault 0

        fretForString s =
            pickList
                |> List.filter (\( _, ps ) -> ps == s)
                |> List.map (\( o, _ ) -> rootPitch + o - stringOpenPitch s)
                |> List.filter (\f -> f >= 0)
                |> List.head
    in
    if coversAll then
        Just { frets = List.map fretForString (List.range 0 (List.length openStrings - 1)) }

    else
        Nothing
