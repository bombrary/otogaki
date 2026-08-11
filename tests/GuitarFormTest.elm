module GuitarFormTest exposing (suite)

import Data.Chord exposing (Alteration(..), Chord, Extension(..), Quality(..), qualityIntervals)
import Data.GuitarForm as GuitarForm
import Expect
import Set
import Test exposing (Test, describe, test)


chord : Int -> Quality -> Chord
chord root quality =
    { root = root, quality = quality, extensions = [], alterations = [], bass = Nothing, voicing = Nothing }


chordExt : Int -> Quality -> List Extension -> Chord
chordExt root quality extensions =
    { root = root, quality = quality, extensions = extensions, alterations = [], bass = Nothing, voicing = Nothing }


chordSlash : Int -> Quality -> Int -> Chord
chordSlash root quality bass =
    { root = root, quality = quality, extensions = [], alterations = [], bass = Just bass, voicing = Nothing }


{-| 検証対象 quality の「必須音」（root, 3rd/b3, 7th系は7th）を root からのインターバルで返す。
5thは省略形があるため意図的に除外。
-}
requiredIntervals : Quality -> List Int
requiredIntervals quality =
    case quality of
        Maj ->
            [ 0, 4 ]

        Min ->
            [ 0, 3 ]

        Dom7 ->
            [ 0, 4, 10 ]

        Maj7 ->
            [ 0, 4, 11 ]

        Min7 ->
            [ 0, 3, 10 ]

        Aug ->
            [ 0, 4 ]

        _ ->
            [ 0 ]


{-| スラッシュ・ハイブリッド・blackadder フォームの共通検証。
(a) 最低音pcがbassPcと一致 (b) 発音pc集合が構成音∩{bass}に含まれる (c) 必須音を含む。
-}
expectValidSlashForm : Int -> Quality -> Int -> GuitarForm.Form -> Expect.Expectation
expectValidSlashForm root quality bass f =
    let
        pitches =
            GuitarForm.toPitches f

        pcs =
            List.map (modBy 12) pitches

        lowestPc =
            List.minimum pitches |> Maybe.map (modBy 12)

        chordTonePcs =
            qualityIntervals quality |> List.map (\i -> modBy 12 (root + i))

        allowedPcs =
            bass :: chordTonePcs

        allInAllowed =
            List.all (\p -> List.member p allowedPcs) pcs

        requiredPcs =
            requiredIntervals quality |> List.map (\i -> modBy 12 (root + i))

        requiredPresent =
            List.all (\r -> List.member r pcs) requiredPcs
    in
    Expect.equal ( Just bass, True, True ) ( lowestPc, allInAllowed, requiredPresent )


{-| Elm のタプルは2または3要素までなので、(name, root, quality, bass) の4項はレコードで持つ。
-}
type alias SlashCase =
    { name : String, root : Int, quality : Quality, bass : Int }


{-| ケースから、指定した Position の候補が1件以上あり、すべて expectValidSlashForm を満たすことを
検証する Test を生成する。候補が0件なら失敗する（実装バグで候補が消えるケースを
すり抜けさせないため）。
-}
slashCaseTests : String -> (GuitarForm.Position -> Bool) -> SlashCase -> Test
slashCaseTests groupLabel positionFilter { name, root, quality, bass } =
    let
        candidates =
            GuitarForm.candidatesFor (chordSlash root quality bass)
                |> List.filter (\c -> positionFilter c.position)
    in
    describe (groupLabel ++ ": " ++ name)
        (test "候補が1件以上存在する" (\_ -> Expect.equal True (List.length candidates > 0))
            :: List.indexedMap
                (\i c -> test ("候補#" ++ String.fromInt i ++ " が妥当") (\_ -> expectValidSlashForm root quality bass c.form))
                candidates
        )


openSlashCases : List SlashCase
openSlashCases =
    [ { name = "C/E", root = 0, quality = Maj, bass = 4 }
    , { name = "D/F#", root = 2, quality = Maj, bass = 6 }
    , { name = "E/G#", root = 4, quality = Maj, bass = 8 }
    , { name = "G/B", root = 7, quality = Maj, bass = 11 }
    , { name = "A/C#", root = 9, quality = Maj, bass = 1 }
    , { name = "F/A", root = 5, quality = Maj, bass = 9 }
    , { name = "C/G", root = 0, quality = Maj, bass = 7 }
    , { name = "D/A", root = 2, quality = Maj, bass = 9 }
    , { name = "G/D", root = 7, quality = Maj, bass = 2 }
    , { name = "A/E", root = 9, quality = Maj, bass = 4 }
    , { name = "F/C", root = 5, quality = Maj, bass = 0 }
    , { name = "E/B", root = 4, quality = Maj, bass = 11 }
    , { name = "Am/C", root = 9, quality = Min, bass = 0 }
    , { name = "Dm/F", root = 2, quality = Min, bass = 5 }
    , { name = "Am/E", root = 9, quality = Min, bass = 4 }
    , { name = "Dm/A", root = 2, quality = Min, bass = 9 }
    , { name = "Em/B", root = 4, quality = Min, bass = 11 }
    , { name = "D7/F#", root = 2, quality = Dom7, bass = 6 }
    , { name = "G7/B", root = 7, quality = Dom7, bass = 11 }
    , { name = "E7/G#", root = 4, quality = Dom7, bass = 8 }
    , { name = "A7/E", root = 9, quality = Dom7, bass = 4 }
    , { name = "D7/A", root = 2, quality = Dom7, bass = 9 }
    , { name = "E7/B", root = 4, quality = Dom7, bass = 11 }
    , { name = "CM7/E", root = 0, quality = Maj7, bass = 4 }
    , { name = "GM7/B", root = 7, quality = Maj7, bass = 11 }
    , { name = "CM7/G", root = 0, quality = Maj7, bass = 7 }
    , { name = "GM7/D", root = 7, quality = Maj7, bass = 2 }
    , { name = "DM7/A", root = 2, quality = Maj7, bass = 9 }
    , { name = "Am7/E", root = 9, quality = Min7, bass = 4 }
    , { name = "Em7/B", root = 4, quality = Min7, bass = 11 }
    , { name = "Dm7/A", root = 2, quality = Min7, bass = 9 }
    , { name = "F/G", root = 5, quality = Maj, bass = 7 }
    , { name = "Dm7/G", root = 2, quality = Min7, bass = 7 }
    , { name = "G/C", root = 7, quality = Maj, bass = 0 }
    , { name = "D/C", root = 2, quality = Maj, bass = 0 }
    , { name = "C/D", root = 0, quality = Maj, bass = 2 }
    , { name = "A/G", root = 9, quality = Maj, bass = 7 }
    , { name = "D/G", root = 2, quality = Maj, bass = 7 }
    ]


movableSlashCases : List SlashCase
movableSlashCases =
    [ { name = "Bb/D (可動/3rd Maj)", root = 10, quality = Maj, bass = 2 }
    , { name = "Bb/F (可動/5th Maj)", root = 10, quality = Maj, bass = 5 }
    , { name = "Gm/Bb (可動/3rd Min)", root = 7, quality = Min, bass = 10 }
    , { name = "Gm/D (可動/5th Min)", root = 7, quality = Min, bass = 2 }
    , { name = "C7/E (可動/3rd Dom7)", root = 0, quality = Dom7, bass = 4 }
    , { name = "G7/D (可動/5th Dom7)", root = 7, quality = Dom7, bass = 2 }
    , { name = "EM7/B (可動/5th Maj7)", root = 4, quality = Maj7, bass = 11 }
    , { name = "Am7/C (可動/3rd Min7)", root = 9, quality = Min7, bass = 0 }
    , { name = "G/A (ハイブリッド Maj+2)", root = 7, quality = Maj, bass = 9 }
    , { name = "Gm7/C (ハイブリッド Min7+5)", root = 7, quality = Min7, bass = 0 }
    , { name = "C/F (ハイブリッド Maj+5)", root = 0, quality = Maj, bass = 5 }
    , { name = "C/Bb (ハイブリッド Maj+10)", root = 0, quality = Maj, bass = 10 }
    ]


openSlashTests : List Test
openSlashTests =
    List.map (slashCaseTests "openSlashTable" (\p -> p == GuitarForm.Open)) openSlashCases


movableSlashTests : List Test
movableSlashTests =
    List.map (slashCaseTests "movable slash shape" (\p -> p /= GuitarForm.Open)) movableSlashCases


suite : Test
suite =
    describe "Data.GuitarForm"
        ([ test "オープン C のピッチ列が期待値と一致する" <|
            \_ ->
                case GuitarForm.forChord (chord 0 Maj) of
                    Just form ->
                        Expect.equal [ 48, 52, 55, 60, 64 ] (GuitarForm.toPitches form)

                    Nothing ->
                        Expect.fail "C major のフォームが引けなかった"
        , test "F が E型1フレットバレーになる" <|
            \_ ->
                case GuitarForm.forChord (chord 5 Maj) of
                    Just form ->
                        Expect.equal [ 41, 48, 53, 57, 60, 65 ] (GuitarForm.toPitches form)

                    Nothing ->
                        Expect.fail "F major のフォームが引けなかった"
        , test "複数 extension 同時指定は非対応（forChord が Nothing を返す）" <|
            \_ ->
                Expect.equal Nothing (GuitarForm.forChord (chordExt 0 Dom7 [ Nine, FlatNine ]))
        , test "bestForm の結果を toPitches に通すと元のピッチ集合に戻る（往復性）" <|
            \_ ->
                let
                    pitches =
                        [ 48, 52, 55, 60, 64 ]
                in
                case GuitarForm.bestForm pitches of
                    Just resultForm ->
                        Expect.equal (List.sort pitches) (GuitarForm.toPitches resultForm |> List.sort)

                    Nothing ->
                        Expect.fail "bestForm が Nothing を返した"
        , test "7音以上のピッチ列では Nothing" <|
            \_ ->
                Expect.equal Nothing (GuitarForm.bestForm [ 40, 45, 50, 55, 59, 64, 69 ])
        , test "どの弦にも収まらない極端に低いピッチでは Nothing" <|
            \_ ->
                Expect.equal Nothing (GuitarForm.bestForm [ 20 ])
        , test "どの弦にも収まらない極端に高いピッチでは Nothing" <|
            \_ ->
                Expect.equal Nothing (GuitarForm.bestForm [ 120 ])
        , test "開放弦そのものを渡すと全弦が開放で選ばれる" <|
            \_ ->
                Expect.equal (Just (List.repeat 6 (Just 0))) (GuitarForm.bestForm GuitarForm.openStrings |> Maybe.map .frets)
        , test "openStrings はレギュラーチューニング（E2/A2/D3/G3/B3/E4）" <|
            \_ ->
                Expect.equal [ 40, 45, 50, 55, 59, 64 ] GuitarForm.openStrings
        , test "shiftPicks は選択中 offset の運指だけを動かす" <|
            \_ ->
                Expect.equal
                    (Set.fromList [ ( 2, 0 ), ( 7, 1 ) ])
                    (GuitarForm.shiftPicks 2 100 (Set.singleton 0) (Set.fromList [ ( 0, 0 ), ( 7, 1 ) ]))
        , test "shiftPicks は未選択の運指を動かさない" <|
            \_ ->
                Expect.equal
                    (Set.fromList [ ( 0, 0 ), ( 9, 1 ) ])
                    (GuitarForm.shiftPicks 2 100 (Set.singleton 7) (Set.fromList [ ( 0, 0 ), ( 7, 1 ) ]))
        , test "shiftPicks は -12 未満にはクランプされる（転回形・ハイブリッドコード対応で下限が -12 に緩和）" <|
            \_ ->
                Expect.equal
                    (Set.fromList [ ( -12, 0 ) ])
                    (GuitarForm.shiftPicks -100 100 (Set.singleton 0) (Set.fromList [ ( 0, 0 ) ]))
        , test "shiftPicks は maxOffset を超えないようにクランプされる" <|
            \_ ->
                Expect.equal
                    (Set.fromList [ ( 10, 0 ) ])
                    (GuitarForm.shiftPicks 100 10 (Set.singleton 0) (Set.fromList [ ( 0, 0 ) ]))
        , test "removePicks は指定 offset の運指を、弦が違っても全部捨てる" <|
            \_ ->
                Expect.equal
                    (Set.fromList [ ( 0, 0 ), ( 16, 2 ) ])
                    (GuitarForm.removePicks (Set.singleton 7) (Set.fromList [ ( 0, 0 ), ( 7, 1 ), ( 7, 3 ), ( 16, 2 ) ]))
        , test "removePicks は選択が空なら何も削除しない" <|
            \_ ->
                Expect.equal
                    (Set.fromList [ ( 0, 0 ), ( 7, 1 ) ])
                    (GuitarForm.removePicks Set.empty (Set.fromList [ ( 0, 0 ), ( 7, 1 ) ]))
        , test "formFromPicks は offsets 全てに運指があれば Just を返す" <|
            \_ ->
                -- root=40(E2と同じピッチ)、offset 0 を 0弦開放、offset 7 を 1弦 0フレット（open=45, fret=2）
                Expect.equal
                    (Just { frets = [ Just 0, Just 2, Nothing, Nothing, Nothing, Nothing ] })
                    (GuitarForm.formFromPicks 40 [ 0, 7 ] (Set.fromList [ ( 0, 0 ), ( 7, 1 ) ]))
        , test "formFromPicks は一部の offset に運指がなければ Nothing を返す" <|
            \_ ->
                Expect.equal
                    Nothing
                    (GuitarForm.formFromPicks 40 [ 0, 7 ] (Set.fromList [ ( 0, 0 ) ]))
        , test "formFromPicks は offsets が空なら Nothing を返す" <|
            \_ ->
                Expect.equal
                    Nothing
                    (GuitarForm.formFromPicks 40 [] (Set.fromList [ ( 0, 0 ) ]))
        , test "candidatesFor は Bm7-5 で Open・E型・A型・D型の4候補を返す（x20201 / 7x776x / x2323x / xx9(10)(10)(10)）" <|
            \_ ->
                GuitarForm.candidatesFor (chord 11 HalfDim7)
                    |> List.map (\c -> ( c.position, c.form.frets ))
                    |> Expect.equal
                        [ ( GuitarForm.Open, [ Nothing, Just 2, Just 0, Just 2, Just 0, Just 1 ] )
                        , ( GuitarForm.ERoot, [ Just 7, Nothing, Just 7, Just 7, Just 6, Nothing ] )
                        , ( GuitarForm.ARoot, [ Nothing, Just 2, Just 3, Just 2, Just 3, Nothing ] )
                        , ( GuitarForm.DRoot, [ Nothing, Nothing, Just 9, Just 10, Just 10, Just 10 ] )
                        ]
        , test "FM7 の E型フォームは先頭が 1x3210（Standard）" <|
            \_ ->
                case GuitarForm.candidatesFor (chord 5 Maj7) |> List.filter (\c -> c.position == GuitarForm.ERoot) |> List.head of
                    Just cand ->
                        Expect.equal [ Just 1, Nothing, Just 3, Just 2, Just 1, Just 0 ] cand.form.frets

                    Nothing ->
                        Expect.fail "FM7 の E型が引けなかった"
        , test "FM7 の ERoot は Standardと1x3210とCompact（1x221x）の2候補" <|
            \_ ->
                GuitarForm.candidatesFor (chord 5 Maj7)
                    |> List.filter (\c -> c.position == GuitarForm.ERoot)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal
                        [ [ Just 1, Nothing, Just 3, Just 2, Just 1, Just 0 ]
                        , [ Just 1, Nothing, Just 2, Just 2, Just 1, Nothing ]
                        ]
        , test "forChord は FM7 で E型コンパクト形（1x221x）を選ぶ（maxFretが小さいため）" <|
            \_ ->
                case GuitarForm.forChord (chord 5 Maj7) of
                    Just f ->
                        Expect.equal [ Just 1, Nothing, Just 2, Just 2, Just 1, Nothing ] f.frets

                    Nothing ->
                        Expect.fail "FM7 のフォームが引けなかった"
        , test "candidatesFor は Bdim7 で E型・A型・D型の3候補を返す（7x676x / x2313x / xx9(10)9(10)）" <|
            \_ ->
                GuitarForm.candidatesFor (chord 11 Dim7)
                    |> List.map (\c -> ( c.position, c.form.frets ))
                    |> Expect.equal
                        [ ( GuitarForm.ERoot, [ Just 7, Nothing, Just 6, Just 7, Just 6, Nothing ] )
                        , ( GuitarForm.ARoot, [ Nothing, Just 2, Just 3, Just 1, Just 3, Nothing ] )
                        , ( GuitarForm.DRoot, [ Nothing, Nothing, Just 9, Just 10, Just 9, Just 10 ] )
                        ]
        , test "Em7-5 の E型は12Fリフトされフレットが全て非負になる" <|
            \_ ->
                case GuitarForm.candidatesFor (chord 4 HalfDim7) |> List.filter (\c -> c.position == GuitarForm.ERoot) |> List.head of
                    Just cand ->
                        Expect.equal [ Just 12, Nothing, Just 12, Just 12, Just 11, Nothing ] cand.form.frets

                    Nothing ->
                        Expect.fail "Em7-5 の E型が引けなかった"
        , test "candidatesFor はオープンコードがあれば先頭に置く" <|
            \_ ->
                GuitarForm.candidatesFor (chord 0 Maj)
                    |> List.head
                    |> Maybe.map .position
                    |> Expect.equal (Just GuitarForm.Open)
        , test "Csus2 は A型・D型の2候補を返す（x35533 / xx(10)(12)(13)(10)）" <|
            \_ ->
                GuitarForm.candidatesFor (chord 0 Sus2)
                    |> List.map (\c -> ( c.position, c.form.frets ))
                    |> Expect.equal
                        [ ( GuitarForm.ARoot, [ Nothing, Just 3, Just 5, Just 5, Just 3, Just 3 ] )
                        , ( GuitarForm.DRoot, [ Nothing, Nothing, Just 10, Just 12, Just 13, Just 10 ] )
                        ]
        , test "C7 の candidatesFor 先頭が Open（x32310）" <|
            \_ ->
                case GuitarForm.candidatesFor (chord 0 Dom7) |> List.head of
                    Just cand ->
                        Expect.equal ( GuitarForm.Open, [ Nothing, Just 3, Just 2, Just 3, Just 1, Just 0 ] ) ( cand.position, cand.form.frets )

                    Nothing ->
                        Expect.fail "C7 の候補が引けなかった"
        , test "GM7 の candidates に Open/ERoot Compact/DRoot の3件が含まれる" <|
            \_ ->
                let
                    candidates =
                        GuitarForm.candidatesFor (chord 7 Maj7)

                    frets =
                        List.map (\c -> ( c.position, c.variant, c.form.frets )) candidates
                in
                Expect.equal True
                    (List.member ( GuitarForm.Open, GuitarForm.Standard, [ Just 3, Just 2, Just 0, Just 0, Just 0, Just 2 ] ) frets
                        && List.member ( GuitarForm.ERoot, GuitarForm.Compact, [ Just 3, Nothing, Just 4, Just 4, Just 3, Nothing ] ) frets
                        && List.member ( GuitarForm.DRoot, GuitarForm.Standard, [ Nothing, Nothing, Just 5, Just 4, Just 3, Just 2 ] ) frets
                    )
        , test "GM7 の forChord は Open（320002）を選ぶ" <|
            \_ ->
                case GuitarForm.forChord (chord 7 Maj7) of
                    Just f ->
                        Expect.equal [ Just 3, Just 2, Just 0, Just 0, Just 0, Just 2 ] f.frets

                    Nothing ->
                        Expect.fail "GM7 のフォームが引けなかった"
        , test "Cadd9 の forChord が x3203xで、toPitches が9th（D=62）を含む" <|
            \_ ->
                case GuitarForm.forChord (chordExt 0 Maj [ Add9 ]) of
                    Just f ->
                        Expect.equal ( [ Nothing, Just 3, Just 2, Just 0, Just 3, Nothing ], True )
                            ( f.frets, List.member 62 (GuitarForm.toPitches f) )

                    Nothing ->
                        Expect.fail "Cadd9 のフォームが引けなかった"
        , test "Dadd9 の forChord が x54230で、toPitches が9th（E=64）を含む" <|
            \_ ->
                case GuitarForm.forChord (chordExt 2 Maj [ Add9 ]) of
                    Just f ->
                        Expect.equal ( [ Nothing, Just 5, Just 4, Just 2, Just 3, Just 0 ], True )
                            ( f.frets, List.member 64 (GuitarForm.toPitches f) )

                    Nothing ->
                        Expect.fail "Dadd9 のフォームが引けなかった"
        , test "Fadd9 の candidates が ERoot/DRoot の2件" <|
            \_ ->
                GuitarForm.candidatesFor (chordExt 5 Maj [ Add9 ])
                    |> List.map (\c -> ( c.position, c.form.frets ))
                    |> Expect.equal
                        [ ( GuitarForm.ERoot, [ Just 1, Nothing, Just 3, Just 2, Just 1, Just 3 ] )
                        , ( GuitarForm.DRoot, [ Nothing, Nothing, Just 3, Just 2, Just 1, Just 3 ] )
                        ]
        , test "DM7 の DRoot は全非負フレット（12F帯へのリフト）" <|
            \_ ->
                case GuitarForm.candidatesFor (chord 2 Maj7) |> List.filter (\c -> c.position == GuitarForm.DRoot) |> List.head of
                    Just cand ->
                        Expect.equal [ Nothing, Nothing, Just 12, Just 11, Just 10, Just 9 ] cand.form.frets

                    Nothing ->
                        Expect.fail "DM7 の DRoot が引けなかった"
        , test "Nine 拡張は Dom7/Maj7/Min7 で candidatesFor に候補が返る" <|
            \_ ->
                Expect.notEqual [] (GuitarForm.candidatesFor (chordExt 0 Dom7 [ Nine ]))
        , test "candidateSuffix は Standard/Compact で異なる文字列を返す" <|
            \_ ->
                let
                    standardCand =
                        { position = GuitarForm.ERoot, variant = GuitarForm.Standard, form = { frets = [] } }

                    compactCand =
                        { position = GuitarForm.ERoot, variant = GuitarForm.Compact, form = { frets = [] } }
                in
                Expect.notEqual (GuitarForm.candidateSuffix standardCand) (GuitarForm.candidateSuffix compactCand)
        , test "B7 の open が x21202" <|
            \_ ->
                GuitarForm.candidatesFor (chord 11 Dom7)
                    |> List.filter (\c -> c.position == GuitarForm.Open)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal
                        [ [ Nothing, Just 2, Just 1, Just 2, Just 0, Just 2 ] ]
        , test "Amaj7 の open が x02120" <|
            \_ ->
                GuitarForm.candidatesFor (chord 9 Maj7)
                    |> List.filter (\c -> c.position == GuitarForm.Open)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal
                        [ [ Nothing, Just 0, Just 2, Just 1, Just 2, Just 0 ] ]
        , test "Asus2 の open が x02200" <|
            \_ ->
                GuitarForm.candidatesFor (chord 9 Sus2)
                    |> List.filter (\c -> c.position == GuitarForm.Open)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal
                        [ [ Nothing, Just 0, Just 2, Just 2, Just 0, Just 0 ] ]
        , test "G5（E型Power）が 355xxx" <|
            \_ ->
                GuitarForm.candidatesFor (chord 7 Power)
                    |> List.filter (\c -> c.position == GuitarForm.ERoot)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal
                        [ [ Just 3, Just 5, Just 5, Nothing, Nothing, Nothing ] ]
        , test "F#sus4（E型Sus4）が 244422" <|
            \_ ->
                GuitarForm.candidatesFor (chord 6 Sus4)
                    |> List.filter (\c -> c.position == GuitarForm.ERoot)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal
                        [ [ Just 2, Just 4, Just 4, Just 4, Just 2, Just 2 ] ]
        , test "C5（A型Power）が x355xx" <|
            \_ ->
                GuitarForm.candidatesFor (chord 0 Power)
                    |> List.filter (\c -> c.position == GuitarForm.ARoot)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal
                        [ [ Nothing, Just 3, Just 5, Just 5, Nothing, Nothing ] ]
        , test "Bsus2（A型Sus2）が x24422" <|
            \_ ->
                GuitarForm.candidatesFor (chord 11 Sus2)
                    |> List.filter (\c -> c.position == GuitarForm.ARoot)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal
                        [ [ Nothing, Just 2, Just 4, Just 4, Just 2, Just 2 ] ]
        , test "Bsus4（A型Sus4）が x24452" <|
            \_ ->
                GuitarForm.candidatesFor (chord 11 Sus4)
                    |> List.filter (\c -> c.position == GuitarForm.ARoot)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal
                        [ [ Nothing, Just 2, Just 4, Just 4, Just 5, Just 2 ] ]
        , test "Fm7-5（D型HalfDim7）が xx3444" <|
            \_ ->
                GuitarForm.candidatesFor (chord 5 HalfDim7)
                    |> List.filter (\c -> c.position == GuitarForm.DRoot)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal
                        [ [ Nothing, Nothing, Just 3, Just 4, Just 4, Just 4 ] ]
        , test "D#dim7（D型Dim7）が xx1212" <|
            \_ ->
                GuitarForm.candidatesFor (chord 3 Dim7)
                    |> List.filter (\c -> c.position == GuitarForm.DRoot)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal
                        [ [ Nothing, Nothing, Just 1, Just 2, Just 1, Just 2 ] ]
        , test "F（D型Maj）が xx3565" <|
            \_ ->
                GuitarForm.candidatesFor (chord 5 Maj)
                    |> List.filter (\c -> c.position == GuitarForm.DRoot)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal
                        [ [ Nothing, Nothing, Just 3, Just 5, Just 6, Just 5 ] ]
        , test "alterations が空でない C7 は candidatesFor が空リスト" <|
            \_ ->
                Expect.equal []
                    (GuitarForm.candidatesFor
                        { root = 0, quality = Dom7, extensions = [], alterations = [ Flat5 ], bass = Nothing, voicing = Nothing }
                    )
        , test "Dim（トライアド）は Dim7 と同じ候補を返す（代用）" <|
            \_ ->
                Expect.equal
                    (GuitarForm.candidatesFor (chord 0 Dim7))
                    (GuitarForm.candidatesFor (chord 0 Dim))
        , test "G5（E型Power）の toPitches が3rdを含まない（root/5th/octaveのみ）" <|
            \_ ->
                case GuitarForm.candidatesFor (chord 7 Power) |> List.filter (\c -> c.position == GuitarForm.ERoot) |> List.head of
                    Just cand ->
                        Expect.equal [ 43, 50, 55 ] (GuitarForm.toPitches cand.form)

                    Nothing ->
                        Expect.fail "G5 の E型が引けなかった"
        , test "C5（A型Power）の toPitches が3rdを含まない（root/5th/octaveのみ）" <|
            \_ ->
                case GuitarForm.candidatesFor (chord 0 Power) |> List.filter (\c -> c.position == GuitarForm.ARoot) |> List.head of
                    Just cand ->
                        Expect.equal [ 48, 55, 60 ] (GuitarForm.toPitches cand.form)

                    Nothing ->
                        Expect.fail "C5 の A型が引けなかった"

        -- 転回形・ハイブリッドコードの個別フレット確認
        , test "C/E の open が 032010" <|
            \_ ->
                GuitarForm.candidatesFor (chordSlash 0 Maj 4)
                    |> List.filter (\c -> c.position == GuitarForm.Open)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal [ [ Just 0, Just 3, Just 2, Just 0, Just 1, Just 0 ] ]
        , test "G/B の open が x20033" <|
            \_ ->
                GuitarForm.candidatesFor (chordSlash 7 Maj 11)
                    |> List.filter (\c -> c.position == GuitarForm.Open)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal [ [ Nothing, Just 2, Just 0, Just 0, Just 3, Just 3 ] ]
        , test "Dm/F の open が 1x0231" <|
            \_ ->
                GuitarForm.candidatesFor (chordSlash 2 Min 5)
                    |> List.filter (\c -> c.position == GuitarForm.Open)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal [ [ Just 1, Nothing, Just 0, Just 2, Just 3, Just 1 ] ]
        , test "Dm7/G の open が 3x0211" <|
            \_ ->
                GuitarForm.candidatesFor (chordSlash 2 Min7 7)
                    |> List.filter (\c -> c.position == GuitarForm.Open)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal [ [ Just 3, Nothing, Just 0, Just 2, Just 1, Just 1 ] ]
        , test "F/G の open が 3x3211" <|
            \_ ->
                GuitarForm.candidatesFor (chordSlash 5 Maj 7)
                    |> List.filter (\c -> c.position == GuitarForm.Open)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal [ [ Just 3, Nothing, Just 3, Just 2, Just 1, Just 1 ] ]
        , test "D/C の open が x30232" <|
            \_ ->
                GuitarForm.candidatesFor (chordSlash 2 Maj 0)
                    |> List.filter (\c -> c.position == GuitarForm.Open)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal [ [ Nothing, Just 3, Just 0, Just 2, Just 3, Just 2 ] ]
        , test "blackadder 3例（Eaug/F#, G#aug/Bb, Dbaug/Eb）がシェイプ計算結果と一致する" <|
            \_ ->
                let
                    eaugFsharp =
                        GuitarForm.candidatesFor (chordSlash 4 Aug 6)
                            |> List.filter (\c -> c.position == GuitarForm.ERoot)
                            |> List.map (\c -> c.form.frets)

                    gSharpAugBb =
                        GuitarForm.candidatesFor (chordSlash 8 Aug 10)
                            |> List.filter (\c -> c.position == GuitarForm.ARoot)
                            |> List.map (\c -> c.form.frets)

                    dbAugEb =
                        GuitarForm.candidatesFor (chordSlash 1 Aug 3)
                            |> List.filter (\c -> c.position == GuitarForm.DRoot)
                            |> List.map (\c -> c.form.frets)
                in
                Expect.equal
                    ( [ [ Just 2, Nothing, Just 2, Just 1, Just 1, Nothing ] ]
                    , [ [ Nothing, Just 1, Just 2, Just 1, Just 1, Nothing ] ]
                    , [ [ Nothing, Nothing, Just 1, Just 2, Just 2, Just 1 ] ]
                    )
                    ( eaugFsharp, gSharpAugBb, dbAugEb )
        , test "C9 の A型が x32333" <|
            \_ ->
                GuitarForm.candidatesFor (chordExt 0 Dom7 [ Nine ])
                    |> List.filter (\c -> c.position == GuitarForm.ARoot)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal [ [ Nothing, Just 3, Just 2, Just 3, Just 3, Just 3 ] ]
        , test "Cmaj9 の A型が x3243x" <|
            \_ ->
                GuitarForm.candidatesFor (chordExt 0 Maj7 [ Nine ])
                    |> List.filter (\c -> c.position == GuitarForm.ARoot)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal [ [ Nothing, Just 3, Just 2, Just 4, Just 3, Nothing ] ]
        , test "Cm11 の A型が x31311" <|
            \_ ->
                GuitarForm.candidatesFor (chordExt 0 Min7 [ Eleven ])
                    |> List.filter (\c -> c.position == GuitarForm.ARoot)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal [ [ Nothing, Just 3, Just 1, Just 3, Just 1, Just 1 ] ]
        , test "G13 の E型が 3x3450 相当" <|
            \_ ->
                GuitarForm.candidatesFor (chordExt 7 Dom7 [ Thirteen ])
                    |> List.filter (\c -> c.position == GuitarForm.ERoot)
                    |> List.map (\c -> c.form.frets)
                    |> Expect.equal [ [ Just 3, Nothing, Just 3, Just 4, Just 5, Nothing ] ]
        , test "Am7/E の可動/5再利用則（ERoot）がオープン形の運指と一致する" <|
            \_ ->
                let
                    openFrets =
                        GuitarForm.candidatesFor (chordSlash 9 Min7 4)
                            |> List.filter (\c -> c.position == GuitarForm.Open)
                            |> List.map (\c -> c.form.frets)

                    eRootFrets =
                        GuitarForm.candidatesFor (chordSlash 9 Min7 4)
                            |> List.filter (\c -> c.position == GuitarForm.ERoot)
                            |> List.map (\c -> c.form.frets)
                in
                Expect.equal openFrets eRootFrets
        , test "C/C（bass==root）は通常経路に落ちる（candidatesFor が C と同じ）" <|
            \_ ->
                Expect.equal (GuitarForm.candidatesFor (chord 0 Maj)) (GuitarForm.candidatesFor (chordSlash 0 Maj 0))
        , test "C/B（未対応スラッシュ）は rootCandidates にフォールバックする" <|
            \_ ->
                Expect.equal (GuitarForm.candidatesFor (chord 0 Maj)) (GuitarForm.candidatesFor (chordSlash 0 Maj 11))
        , test "forChord は C/E で Open を選ぶ" <|
            \_ ->
                case GuitarForm.forChord (chordSlash 0 Maj 4) of
                    Just f ->
                        Expect.equal [ Just 0, Just 3, Just 2, Just 0, Just 1, Just 0 ] f.frets

                    Nothing ->
                        Expect.fail "C/E のフォームが引けなかった"
         ]
            ++ openSlashTests
            ++ movableSlashTests
        )
