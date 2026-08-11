module ChordTrackTest exposing (suite)

import Data.ChordTrack as ChordTrack
import Data.Key
import Data.Meter
import Data.Section exposing (Section)
import Data.Timeline
import Data.Track exposing (Instrument(..))
import Expect
import Set
import Test exposing (Test, describe, test)


track : String -> ChordTrack.ChordTrack
track text =
    { text = text, instrument = Piano, muted = False, volume = 100 }


{-| 4/4 固定の十分長いセクションで作った timeline。既存テストはこれを通して resolved を呼ぶ。
-}
timeline : Data.Timeline.Timeline
timeline =
    Data.Timeline.fromSections { minBars = 64 }
        [ { id = 1, name = "s", lengthBars = 64, memo = "", key = Data.Key.default, meter = Data.Meter.default } ]


{-| 2小節ずつの 3 セクションを持つ timeline。namedSpans/sectionSummaries のセクション跨ぎテストで使う。
-}
multiSectionTimeline : Data.Timeline.Timeline
multiSectionTimeline =
    Data.Timeline.fromSections { minBars = 6 }
        [ { id = 1, name = "A", lengthBars = 2, memo = "", key = Data.Key.default, meter = Data.Meter.default }
        , { id = 2, name = "B", lengthBars = 2, memo = "", key = Data.Key.default, meter = Data.Meter.default }
        , { id = 3, name = "C", lengthBars = 2, memo = "", key = Data.Key.default, meter = Data.Meter.default }
        ]


resolved : ChordTrack.ChordTrack -> List ChordTrack.ResolvedChord
resolved =
    ChordTrack.resolved timeline


suite : Test
suite =
    Test.concat
        [ resolveSuite
        , tokenSpansSuite
        , tokenKeyAtTicksSuite
        , tokenKeysInTickRangeSuite
        , moveTokensSuite
        , removeTokensSuite
        , updateTokenSuite
        , voicingNameSuite
        ]


resolveSuite : Test
resolveSuite =
    describe "コードトラック展開"
        [ test "% は直前のコードを繰り返す" <|
            \_ ->
                resolved (track "C | %")
                    |> List.map .startTicks
                    |> Expect.equal [ 0, Data.Meter.ticksPerBar Data.Meter.default ]
        , test "= は直前のコードを伸ばす" <|
            \_ ->
                case resolved (track "C | =") of
                    [ ev ] ->
                        Expect.equal (Data.Meter.ticksPerBar Data.Meter.default * 2) ev.durationTicks

                    _ ->
                        Expect.fail "イベント数が想定外"
        , test "_ は休符" <|
            \_ ->
                resolved (track "C | _ | C")
                    |> List.length
                    |> Expect.equal 2
        , test "休符の後の % は休符前のコードを繰り返す" <|
            \_ ->
                resolved (track "C | _ | %")
                    |> List.map .startTicks
                    |> Expect.equal [ 0, Data.Meter.ticksPerBar Data.Meter.default * 2 ]
        , test "改行は無視される（小節区切りは | だけ）" <|
            \_ ->
                resolved (track "C |\nG")
                    |> List.map .startTicks
                    |> Expect.equal [ 0, Data.Meter.ticksPerBar Data.Meter.default ]
        , test "改行をまたぐ小節は空白と同じ扱い" <|
            \_ ->
                resolved (track "C\nG")
                    |> List.map .startTicks
                    |> Expect.equal [ 0, Data.Meter.ticksPerBar Data.Meter.default // 2 ]
        , test "小節内分割と = の組合せ" <|
            \_ ->
                case resolved (track "C =") of
                    [ ev ] ->
                        Expect.equal (Data.Meter.ticksPerBar Data.Meter.default) ev.durationTicks

                    _ ->
                        Expect.fail "イベント数が想定外"
        , test "// 以降は行末までコメントとして無視される" <|
            \_ ->
                resolved (track "C | G  // ここからBメロ\n%")
                    |> List.length
                    |> Expect.equal 3
        , test "コメントの中の | は小節区切りとして数えられない" <|
            \_ ->
                ChordTrack.barCount (track "C | G  // 1|2|3 とか書くかも\n| Am")
                    |> Expect.equal 3
        , test "コメントだけの行は空小節扱い" <|
            \_ ->
                resolved (track "C | // この小節はまだ未定\n| G")
                    |> List.map .startTicks
                    |> Expect.equal [ 0, Data.Meter.ticksPerBar Data.Meter.default * 2 ]
        , test "stripComments は // 以降だけを取り除く" <|
            \_ ->
                ChordTrack.stripComments "C | G  // メモ\nAm"
                    |> Expect.equal "C | G  \nAm"
        , test "transpose はコメントの中身を変えない" <|
            \_ ->
                (ChordTrack.transpose 2 (track "C  // C7 が好き")).text
                    |> Expect.equal "D  // C7 が好き"
        , test "transposeBars は指定範囲の小節だけを移調し、範囲外（前/後）は変わらない" <|
            \_ ->
                (ChordTrack.transposeBars 1 2 2 (track "C | D | E | F")).text
                    |> Expect.equal "C | E | F# | F"
        , test "transposeBars は対象範囲内のコメントを保ったまま移調する" <|
            \_ ->
                (ChordTrack.transposeBars 0 1 2 (track "C // note\n| D")).text
                    |> Expect.equal "D // note\n| D"
        , test "namedSpans は % と = を解決したコード名を返す" <|
            \_ ->
                ChordTrack.namedSpans timeline (track "C | G7 | % | =")
                    |> List.map .name
                    |> Expect.equal [ "C", "G7", "G7" ]
        , test "namedSpans は = で伸ばされたスパンを 1 つにまとめる" <|
            \_ ->
                ChordTrack.namedSpans timeline (track "C | G7 | % | =")
                    |> List.length
                    |> Expect.equal 3
        , test "namedSpans は単一セクションなら全て sectionIndex 0" <|
            \_ ->
                ChordTrack.namedSpans timeline (track "C | G | Am | F")
                    |> List.map .sectionIndex
                    |> Expect.equal [ Just 0, Just 0, Just 0, Just 0 ]
        , test "namedSpans はセクションをまたぐと sectionIndex が切り替わる" <|
            \_ ->
                ChordTrack.namedSpans multiSectionTimeline (track "C | C | G | Am")
                    |> List.map .sectionIndex
                    |> Expect.equal [ Just 0, Just 0, Just 1, Just 1 ]
        ]


barLen : Int
barLen =
    Data.Meter.ticksPerBar Data.Meter.default


tokenSpansSuite : Test
tokenSpansSuite =
    describe "Data.ChordTrack.tokenSpans"
        [ test "キーと tick 分割が cells と一致する" <|
            \_ ->
                ChordTrack.tokenSpans timeline (track "C G | Am")
                    |> List.map (\s -> { key = s.key, token = s.token, startTicks = s.startTicks, durationTicks = s.durationTicks })
                    |> Expect.equal
                        [ { key = ( 0, 0 ), token = "C", startTicks = 0, durationTicks = barLen // 2 }
                        , { key = ( 0, 1 ), token = "G", startTicks = barLen // 2, durationTicks = barLen // 2 }
                        , { key = ( 1, 0 ), token = "Am", startTicks = barLen, durationTicks = barLen }
                        ]
        , test "% の resolvedName は直前のコード名" <|
            \_ ->
                ChordTrack.tokenSpans timeline (track "C | %")
                    |> List.filterMap (\s -> Maybe.map (\n -> ( s.key, n )) s.resolvedName)
                    |> Expect.equal [ ( ( 0, 0 ), "C" ), ( ( 1, 0 ), "C" ) ]
        , test "= の resolvedName も直前のコード名" <|
            \_ ->
                ChordTrack.tokenSpans timeline (track "C | =")
                    |> List.filterMap (\s -> Maybe.map (\n -> ( s.key, n )) s.resolvedName)
                    |> Expect.equal [ ( ( 0, 0 ), "C" ), ( ( 1, 0 ), "C" ) ]
        , test "_ とパース不能トークンの resolvedName は Nothing" <|
            \_ ->
                ChordTrack.tokenSpans timeline (track "C | _ | XYZ123")
                    |> List.map (\s -> s.resolvedName)
                    |> Expect.equal [ Just "C", Nothing, Nothing ]
        ]


tokenKeyAtTicksSuite : Test
tokenKeyAtTicksSuite =
    describe "Data.ChordTrack.tokenKeyAtTicks"
        [ test "先頭トークンに一致する ticks" <|
            \_ ->
                ChordTrack.tokenKeyAtTicks timeline 0 (track "C G | Am")
                    |> Expect.equal (Just ( 0, 0 ))
        , test "途中トークンに一致する ticks" <|
            \_ ->
                ChordTrack.tokenKeyAtTicks timeline barLen (track "C G | Am")
                    |> Expect.equal (Just ( 1, 0 ))
        , test "どのトークンにも一致しない ticks" <|
            \_ ->
                ChordTrack.tokenKeyAtTicks timeline (barLen * 10) (track "C G | Am")
                    |> Expect.equal Nothing
        ]


tokenKeysInTickRangeSuite : Test
tokenKeysInTickRangeSuite =
    describe "Data.ChordTrack.tokenKeysInTickRange"
        [ test "半開区間で先頭小節のみ拾う" <|
            \_ ->
                ChordTrack.tokenKeysInTickRange timeline 0 barLen (track "C | G | Am")
                    |> Expect.equal (Set.fromList [ ( 0, 0 ) ])
        , test "次の区間は次の小節のみ" <|
            \_ ->
                ChordTrack.tokenKeysInTickRange timeline barLen (barLen * 2) (track "C | G | Am")
                    |> Expect.equal (Set.fromList [ ( 1, 0 ) ])
        , test "幅を広げれば複数拾う" <|
            \_ ->
                ChordTrack.tokenKeysInTickRange timeline 0 (barLen * 2) (track "C | G | Am")
                    |> Expect.equal (Set.fromList [ ( 0, 0 ), ( 1, 0 ) ])
        ]


moveTokensSuite : Test
moveTokensSuite =
    describe "Data.ChordTrack.moveTokens"
        [ test "空小節への移動" <|
            \_ ->
                let
                    result =
                        ChordTrack.moveTokens timeline 1 (Set.fromList [ ( 0, 0 ) ]) (track "C | ")
                in
                ( ChordTrack.cells timeline result.track |> List.map (\c -> List.map .token c.chords), result.movedKeys )
                    |> Expect.equal ( [ [], [ "C" ] ], Set.fromList [ ( 1, 0 ) ] )
        , test "既存セルへの合流は末尾追記" <|
            \_ ->
                let
                    result =
                        ChordTrack.moveTokens timeline 1 (Set.fromList [ ( 0, 0 ) ]) (track "C | G")
                in
                ChordTrack.cells timeline result.track
                    |> List.map (\c -> List.map .token c.chords)
                    |> Expect.equal [ [], [ "G", "C" ] ]
        , test "% は展開して移動する" <|
            \_ ->
                let
                    result =
                        ChordTrack.moveTokens timeline 1 (Set.fromList [ ( 1, 0 ) ]) (track "C | % | ")
                in
                ChordTrack.cells timeline result.track
                    |> List.map (\c -> List.map .token c.chords)
                    |> Expect.equal [ [ "C" ], [], [ "C" ] ]
        , test "= も展開して移動する" <|
            \_ ->
                let
                    result =
                        ChordTrack.moveTokens timeline 1 (Set.fromList [ ( 1, 0 ) ]) (track "C | = | ")
                in
                ChordTrack.cells timeline result.track
                    |> List.map (\c -> List.map .token c.chords)
                    |> Expect.equal [ [ "C" ], [], [ "C" ] ]
        , test "deltaBars が 0 ならテキスト不変" <|
            \_ ->
                let
                    original =
                        track "C | G"

                    result =
                        ChordTrack.moveTokens timeline 0 (Set.fromList [ ( 0, 0 ) ]) original
                in
                result.track.text |> Expect.equal original.text
        , test "選択が空なら何もしない" <|
            \_ ->
                let
                    original =
                        track "C | G"

                    result =
                        ChordTrack.moveTokens timeline 2 Set.empty original
                in
                result.track.text |> Expect.equal original.text
        , test "負方向は選択の最小 barIndex が 0 を下回らないようにクランプされる" <|
            \_ ->
                let
                    result =
                        ChordTrack.moveTokens timeline -5 (Set.fromList [ ( 1, 0 ) ]) (track "C | G")
                in
                ChordTrack.cells timeline result.track
                    |> List.map (\c -> List.map .token c.chords)
                    |> Expect.equal [ [ "C", "G" ], [] ]
        , test "末尾を超える移動は空小節でパディングされる" <|
            \_ ->
                let
                    result =
                        ChordTrack.moveTokens timeline 3 (Set.fromList [ ( 0, 0 ) ]) (track "C")
                in
                ChordTrack.cells timeline result.track
                    |> List.map (\c -> List.map .token c.chords)
                    |> Expect.equal [ [], [], [], [ "C" ] ]
        , test "無変更の小節のコメントは温存される" <|
            \_ ->
                let
                    result =
                        ChordTrack.moveTokens timeline 2 (Set.fromList [ ( 0, 0 ) ]) (track "C | G // secret\n | Am")
                in
                String.split "|" result.track.text
                    |> List.drop 1
                    |> List.head
                    |> Maybe.withDefault ""
                    |> String.contains "// secret"
                    |> Expect.equal True
        ]


removeTokensSuite : Test
removeTokensSuite =
    describe "Data.ChordTrack.removeTokens"
        [ test "選択トークンを除去しても小節数（| の数）は不変" <|
            \_ ->
                let
                    result =
                        ChordTrack.removeTokens timeline (Set.fromList [ ( 0, 0 ) ]) (track "C G | Am")
                in
                ( ChordTrack.barCount result, ChordTrack.cells timeline result |> List.map (\c -> List.map .token c.chords) )
                    |> Expect.equal ( 2, [ [ "G" ], [ "Am" ] ] )
        , test "選択が空なら何もしない" <|
            \_ ->
                let
                    original =
                        track "C | G"
                in
                (ChordTrack.removeTokens timeline Set.empty original).text |> Expect.equal original.text
        , test "無変更の小節のコメントは温存される" <|
            \_ ->
                let
                    result =
                        ChordTrack.removeTokens timeline (Set.fromList [ ( 0, 0 ) ]) (track "C | G // secret")
                in
                String.contains "// secret" result.text |> Expect.equal True
        ]


updateTokenSuite : Test
updateTokenSuite =
    describe "Data.ChordTrack.updateToken"
        [ test "対象トークンだけを書き換える" <|
            \_ ->
                let
                    result =
                        ChordTrack.updateToken timeline ( 1, 0 ) (\_ -> "REPLACED") (track "C | G")
                in
                ChordTrack.cells timeline result
                    |> List.map (\c -> List.map .token c.chords)
                    |> Expect.equal [ [ "C" ], [ "REPLACED" ] ]
        , test "小節内の複数トークンでも対象 tokenIndex だけを書き換える" <|
            \_ ->
                let
                    result =
                        ChordTrack.updateToken timeline ( 0, 1 ) (\_ -> "REPLACED") (track "C G | Am")
                in
                ChordTrack.cells timeline result
                    |> List.map (\c -> List.map .token c.chords)
                    |> Expect.equal [ [ "C", "REPLACED" ], [ "Am" ] ]
        , test "無変更の小節のコメントは温存される" <|
            \_ ->
                let
                    result =
                        ChordTrack.updateToken timeline ( 1, 0 ) (\_ -> "REPLACED") (track "C // secret\n| G")
                in
                ( String.contains "// secret" result.text
                , ChordTrack.cells timeline result |> List.map (\c -> List.map .token c.chords)
                )
                    |> Expect.equal ( True, [ [ "C" ], [ "REPLACED" ] ] )
        , test "barIndex が範囲外なら何もしない" <|
            \_ ->
                let
                    original =
                        track "C | G"

                    result =
                        ChordTrack.updateToken timeline ( 5, 0 ) (\_ -> "X") original
                in
                result.text |> Expect.equal original.text
        ]


voicingNameSuite : Test
voicingNameSuite =
    describe "Data.ChordTrack.withVoicingName / withoutVoicingName"
        [ test "withVoicingName は @NAME が無ければ末尾に追加する" <|
            \_ ->
                ChordTrack.withVoicingName "Bm7b5_6弦" "Bm7-5"
                    |> Expect.equal "Bm7-5@Bm7b5_6弦"
        , test "withVoicingName は既存の @NAME を新しい名前に付け替える" <|
            \_ ->
                ChordTrack.withVoicingName "New" "Bm7-5@Old"
                    |> Expect.equal "Bm7-5@New"
        , test "withVoicingName はスラッシュベースを保ったまま付け替える" <|
            \_ ->
                ChordTrack.withVoicingName "New" "C@Old/E"
                    |> Expect.equal "C/E@New"
        , test "withoutVoicingName は @NAME を除去する" <|
            \_ ->
                ChordTrack.withoutVoicingName "Bm7-5@Bm7b5_6弦"
                    |> Expect.equal "Bm7-5"
        , test "withoutVoicingName は @NAME が無ければそのまま返す" <|
            \_ ->
                ChordTrack.withoutVoicingName "Bm7-5"
                    |> Expect.equal "Bm7-5"
        , test "withoutVoicingName はスラッシュベースを保つ" <|
            \_ ->
                ChordTrack.withoutVoicingName "C@Old/E"
                    |> Expect.equal "C/E"
        ]
