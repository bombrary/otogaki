module Data.GuitarForm exposing (Form, StringPicks, bestForm, forChord, openStrings, removePicks, shiftPicks, toPitches)

import Data.Chord exposing (Chord, Quality(..))
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


{-| root ピッチクラスと quality の組で完全一致したときに使う実フォーム。
-}
openChordTable : List ( ( Int, Quality ), Form )
openChordTable =
    [ ( ( 0, Maj ), form [ Nothing, Just 3, Just 2, Just 0, Just 1, Just 0 ] ) -- C
    , ( ( 9, Maj ), form [ Nothing, Just 0, Just 2, Just 2, Just 2, Just 0 ] ) -- A
    , ( ( 7, Maj ), form [ Just 3, Just 2, Just 0, Just 0, Just 0, Just 3 ] ) -- G
    , ( ( 4, Maj ), form [ Just 0, Just 2, Just 2, Just 1, Just 0, Just 0 ] ) -- E
    , ( ( 2, Maj ), form [ Nothing, Nothing, Just 0, Just 2, Just 3, Just 2 ] ) -- D
    , ( ( 9, Min ), form [ Nothing, Just 0, Just 2, Just 2, Just 1, Just 0 ] ) -- Am
    , ( ( 4, Min ), form [ Just 0, Just 2, Just 2, Just 0, Just 0, Just 0 ] ) -- Em
    , ( ( 2, Min ), form [ Nothing, Nothing, Just 0, Just 2, Just 3, Just 1 ] ) -- Dm
    , ( ( 9, Dom7 ), form [ Nothing, Just 0, Just 2, Just 0, Just 2, Just 0 ] ) -- A7
    , ( ( 4, Dom7 ), form [ Just 0, Just 2, Just 0, Just 1, Just 0, Just 0 ] ) -- E7
    , ( ( 2, Dom7 ), form [ Nothing, Nothing, Just 0, Just 2, Just 1, Just 2 ] ) -- D7
    , ( ( 7, Dom7 ), form [ Just 3, Just 2, Just 0, Just 0, Just 0, Just 1 ] ) -- G7
    , ( ( 0, Maj7 ), form [ Nothing, Just 3, Just 2, Just 0, Just 0, Just 0 ] ) -- Cmaj7
    , ( ( 9, Min7 ), form [ Nothing, Just 0, Just 2, Just 0, Just 1, Just 0 ] ) -- Am7
    , ( ( 4, Min7 ), form [ Just 0, Just 2, Just 0, Just 0, Just 0, Just 0 ] ) -- Em7
    , ( ( 2, Min7 ), form [ Nothing, Nothing, Just 0, Just 2, Just 1, Just 1 ] ) -- Dm7
    ]


{-| E型（低音弦ルート）バレーコードの、rootからの相対フレットオフセット（6弦分）。
引けない quality（Maj7 を含む）は Nothing。
-}
eShapeOffsets : Quality -> Maybe (List Int)
eShapeOffsets quality =
    case quality of
        Maj ->
            Just [ 0, 2, 2, 1, 0, 0 ]

        Min ->
            Just [ 0, 2, 2, 0, 0, 0 ]

        Dom7 ->
            Just [ 0, 2, 0, 1, 0, 0 ]

        Min7 ->
            Just [ 0, 2, 0, 0, 0, 0 ]

        _ ->
            Nothing


{-| A型（A弦ルート）バレーコードの、rootからの相対フレットオフセット（5弦分、低音弦はミュート）。
-}
aShapeOffsets : Quality -> Maybe (List Int)
aShapeOffsets quality =
    case quality of
        Maj ->
            Just [ 0, 2, 2, 2, 0 ]

        Min ->
            Just [ 0, 2, 2, 1, 0 ]

        Dom7 ->
            Just [ 0, 2, 0, 2, 0 ]

        Min7 ->
            Just [ 0, 2, 0, 1, 0 ]

        Maj7 ->
            Just [ 0, 2, 1, 2, 0 ]

        _ ->
            Nothing


eShapeForm : Int -> Quality -> Maybe Form
eShapeForm rootPc quality =
    eShapeOffsets quality
        |> Maybe.map
            (\offsets ->
                let
                    r =
                        modBy 12 (rootPc - 4)
                in
                form (List.map (\o -> Just (r + o)) offsets)
            )


aShapeForm : Int -> Quality -> Maybe Form
aShapeForm rootPc quality =
    aShapeOffsets quality
        |> Maybe.map
            (\offsets ->
                let
                    g =
                        modBy 12 (rootPc - 9)
                in
                form (Nothing :: List.map (\o -> Just (g + o)) offsets)
            )


maxFret : Form -> Int
maxFret f =
    f.frets |> List.filterMap identity |> List.maximum |> Maybe.withDefault 0


{-| コードの root/quality からフォームを引く。オープンコード表 → ムーバブルフォーム（低フレット優先）の順。
どちらも引けない quality は Nothing（呼び出し側は `Data.Chord.toPitchesWith` にフォールバックする）。
拡張・アルタレーションは無視し、root/quality のみで判定する。
-}
forChord : Chord -> Maybe Form
forChord chord =
    case List.filter (\( key, _ ) -> key == ( chord.root, chord.quality )) openChordTable |> List.head of
        Just ( _, f ) ->
            Just f

        Nothing ->
            case ( eShapeForm chord.root chord.quality, aShapeForm chord.root chord.quality ) of
                ( Just e, Just a ) ->
                    Just
                        (if maxFret e <= maxFret a then
                            e

                         else
                            a
                        )

                ( Just e, Nothing ) ->
                    Just e

                ( Nothing, Just a ) ->
                    Just a

                ( Nothing, Nothing ) ->
                    Nothing


maxFretSearch : Int
maxFretSearch =
    15


{-| 弦のリストと代入すべきピッチのリストから、全ピッチを使い切る完全割当を全探索する。
弦ごとに「ミュート」または「残りのいずれかのピッチを押弦」を選ぶ分岐を総当たりする。
弦で6、候補音高高々数個の小規模な探索空間なので素朴な全探索で十分。
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
行き先が重複したら（同じ弦で同じ offset）Set が自然に 1 つにまとめてくれる。
-}
shiftPicks : Int -> Int -> Set Int -> StringPicks -> StringPicks
shiftPicks delta maxOffset selected picks =
    picks
        |> Set.map
            (\( o, s ) ->
                if Set.member o selected then
                    ( clamp 0 maxOffset (o + delta), s )

                else
                    ( o, s )
            )


{-| 消えた offset の運指メモを捨てる。残すと、同じ offset を後から鍵盤で足し直したときに
古い青丸が復活してしまう。
-}
removePicks : Set Int -> StringPicks -> StringPicks
removePicks selected picks =
    Set.filter (\( o, _ ) -> not (Set.member o selected)) picks
