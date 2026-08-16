module Data.ChordTrack exposing
    ( ChordCell
    , ChordSpan
    , ChordTrack
    , ResolvedChord
    , TokenKey
    , TokenKind(..)
    , TokenSpan
    , barCount
    , cells
    , empty
    , moveTokens
    , namedSpans
    , removeTokens
    , resolved
    , stripComments
    , tokenKeyAtTicks
    , tokenKeysInTickRange
    , tokenSpans
    , toPlainText
    , trackId
    , transpose
    , transposeBars
    , updateToken
    , withVoicingName
    , withoutVoicingName
    , wrapBarLines
    )

import Data.Chord exposing (Chord)
import Data.Chord.Format as ChordFormat
import Data.Chord.Parser as ChordParser
import Data.Meter
import Data.Timeline exposing (Timeline)
import Data.Track exposing (Instrument(..))
import Dict exposing (Dict)
import Set exposing (Set)


type alias ChordTrack =
    { text : String
    , instrument : Instrument
    , muted : Bool
    , volume : Int
    , rhythm : Maybe String
    }


{-| コードトラックを「特別なトラック」として扱う場所（トラック一覧・ゴースト表示など）で使う
擬似的な Track ID。通常トラックの ID は 0 以上なので衝突しない。
-}
trackId : Int
trackId =
    -1


empty : ChordTrack
empty =
    { text = ""
    , instrument = Piano
    , muted = False
    , volume = 100
    , rhythm = Nothing
    }


type TokenKind
    = TChord Chord
    | TRepeat
    | TRest
    | THold


type alias ChordCell =
    { barIndex : Int
    , startTicks : Int
    , lengthTicks : Int
    , chords : List { token : String, result : Result String TokenKind }
    }


parseToken : String -> Result String TokenKind
parseToken tok =
    case tok of
        "%" ->
            Ok TRepeat

        "_" ->
            Ok TRest

        "=" ->
            Ok THold

        _ ->
            ChordParser.parse tok |> Result.map TChord


type TextChunk
    = CodeChunk String
    | CommentChunk String


{-| `//` から行末までをコメントとして切り出す。全チャンクを連結すると元のテキストに一致するので、
改行位置やコメントの中身を完全に保持できる（移調のテキスト置換で使う）。
-}
splitComments : String -> List TextChunk
splitComments text =
    String.split "\n" text
        |> List.map splitLineComment
        |> List.intersperse [ CodeChunk "\n" ]
        |> List.concat


splitLineComment : String -> List TextChunk
splitLineComment line =
    case String.indexes "//" line of
        idx :: _ ->
            [ CodeChunk (String.left idx line), CommentChunk (String.dropLeft idx line) ]

        [] ->
            [ CodeChunk line ]


{-| `//` 以降（行末まで）を取り除く。小節・コードのパースはこの結果に対して行う。
-}
stripComments : String -> String
stripComments text =
    splitComments text
        |> List.map
            (\chunk ->
                case chunk of
                    CodeChunk s ->
                        s

                    CommentChunk _ ->
                        ""
            )
        |> String.concat


{-| テキスト中の小節区切りの数。ticks は不要な場面（曲全体の小節表の長さを決める際など）で使う。
-}
barCount : ChordTrack -> Int
barCount track =
    track.text
        |> stripComments
        |> String.replace "\n" " "
        |> String.split "|"
        |> List.length


{-| 小節区切りは | だけ。改行は空白と同じ扱いで、見た目の整形のために自由に使える。
`//` から行末まではコメントとして無視される。各小節の拍子は Timeline から引く（セクションごとに拍子が違う場合に対応）。
-}
cells : Timeline -> ChordTrack -> List ChordCell
cells timeline track =
    track.text
        |> stripComments
        |> String.replace "\n" " "
        |> String.split "|"
        |> List.indexedMap
            (\i cell ->
                let
                    bar =
                        Data.Timeline.barAt i timeline

                    startTicks =
                        bar |> Maybe.map .startTicks |> Maybe.withDefault (i * Data.Meter.ticksPerBar Data.Meter.default)

                    lengthTicks =
                        bar |> Maybe.map .lengthTicks |> Maybe.withDefault (Data.Meter.ticksPerBar Data.Meter.default)
                in
                { barIndex = i
                , startTicks = startTicks
                , lengthTicks = lengthTicks
                , chords =
                    String.words cell
                        |> List.filter (\w -> w /= "")
                        |> List.map (\tok -> { token = tok, result = parseToken tok })
                }
            )


type alias ResolvedChord =
    { startTicks : Int
    , durationTicks : Int
    , chord : Chord
    }


type alias ResolveState =
    { lastChord : Maybe Chord
    , eventsRev : List ResolvedChord
    }


{-| セル列を実際に鳴るコードイベント列に展開する。
% = 直前のコードをもう一度鳴らす、\_ = 休符、= = 直前のコードを伸ばす。
-}
resolved : Timeline -> ChordTrack -> List ResolvedChord
resolved timeline track =
    cells timeline track
        |> List.foldl resolveCell { lastChord = Nothing, eventsRev = [] }
        |> .eventsRev
        |> List.reverse


{-| ピアノロール上のコード帯など、名前付きで表示する際の単位。再生中判定（active）は
ここには持たせず、呼び出し側が playheadTicks と startTicks/durationTicks を比べて求める。
-}
type alias ChordSpan =
    { name : String
    , startTicks : Int
    , durationTicks : Int
    , sectionIndex : Maybe Int
    }


{-| `resolved` を、表示用のコード名と所属セクション付きのスパン列に変換する。
-}
namedSpans : Timeline -> ChordTrack -> List ChordSpan
namedSpans timeline track =
    resolved timeline track
        |> List.map
            (\rc ->
                { name = ChordFormat.format { preferFlat = False } rc.chord
                , startTicks = rc.startTicks
                , durationTicks = rc.durationTicks
                , sectionIndex = Data.Timeline.sectionIndexAt rc.startTicks timeline
                }
            )


resolveCell : ChordCell -> ResolveState -> ResolveState
resolveCell cell state =
    let
        n =
            List.length cell.chords
    in
    if n == 0 then
        state

    else
        let
            dur =
                cell.lengthTicks // n
        in
        cell.chords
            |> List.indexedMap Tuple.pair
            |> List.foldl
                (\( j, c ) st ->
                    let
                        start =
                            cell.startTicks + j * dur

                        attack chord =
                            { st
                                | lastChord = Just chord
                                , eventsRev =
                                    { startTicks = start, durationTicks = dur, chord = chord }
                                        :: st.eventsRev
                            }
                    in
                    case c.result of
                        Ok (TChord chord) ->
                            attack chord

                        Ok TRepeat ->
                            case st.lastChord of
                                Just chord ->
                                    attack chord

                                Nothing ->
                                    st

                        Ok THold ->
                            case st.eventsRev of
                                ev :: rest ->
                                    if ev.startTicks + ev.durationTicks == start then
                                        { st | eventsRev = { ev | durationTicks = ev.durationTicks + dur } :: rest }

                                    else
                                        st

                                [] ->
                                    st

                        Ok TRest ->
                            st

                        Err _ ->
                            st
                )
                state


type alias TokenKey =
    ( Int, Int )


{-| トークン単位の表示・選択情報。key は (barIndex, その小節内の tokenIndex)。resolvedName は
% や = が実際に何のコードを指しているか（ドラッグ移動時の展開に使う）。TRest/Err は Nothing。
-}
type alias TokenSpan =
    { key : TokenKey
    , token : String
    , result : Result String TokenKind
    , resolvedName : Maybe String
    , startTicks : Int
    , durationTicks : Int
    , sectionIndex : Maybe Int
    }


type alias TokenSpanState =
    { lastChord : Maybe Chord
    , spansRev : List TokenSpan
    }


{-| 全トークンをジオメトリ付きで列挙する。均等分割式（lengthTicks // n）は resolveCell と同じなので、
タイミングは resolved/namedSpans と一致する。
-}
tokenSpans : Timeline -> ChordTrack -> List TokenSpan
tokenSpans timeline track =
    cells timeline track
        |> List.foldl (tokenSpansForCell timeline) { lastChord = Nothing, spansRev = [] }
        |> .spansRev
        |> List.reverse


tokenSpansForCell : Timeline -> ChordCell -> TokenSpanState -> TokenSpanState
tokenSpansForCell timeline cell state =
    let
        n =
            List.length cell.chords
    in
    if n == 0 then
        state

    else
        let
            dur =
                cell.lengthTicks // n
        in
        cell.chords
            |> List.indexedMap Tuple.pair
            |> List.foldl
                (\( j, c ) st ->
                    let
                        start =
                            cell.startTicks + j * dur

                        ( newLastChord, resolvedChord ) =
                            case c.result of
                                Ok (TChord chord) ->
                                    ( Just chord, Just chord )

                                Ok TRepeat ->
                                    ( st.lastChord, st.lastChord )

                                Ok THold ->
                                    ( st.lastChord, st.lastChord )

                                Ok TRest ->
                                    ( st.lastChord, Nothing )

                                Err _ ->
                                    ( st.lastChord, Nothing )

                        span =
                            { key = ( cell.barIndex, j )
                            , token = c.token
                            , result = c.result
                            , resolvedName = Maybe.map (ChordFormat.format { preferFlat = False }) resolvedChord
                            , startTicks = start
                            , durationTicks = dur
                            , sectionIndex = Data.Timeline.sectionIndexAt start timeline
                            }
                    in
                    { lastChord = newLastChord, spansRev = span :: st.spansRev }
                )
                state


{-| tick 範囲 [t0, t1) に開始位置が入るトークンのキー集合。矩形選択・セクション内選択で使う。
ピアノロールの Cmd+Shift+A 選択（n.start >= startTicks && n.start < endTicks）と同じ半開区間。
-}
tokenKeysInTickRange : Timeline -> Int -> Int -> ChordTrack -> Set TokenKey
tokenKeysInTickRange timeline t0 t1 track =
    tokenSpans timeline track
        |> List.filter (\s -> s.startTicks >= t0 && s.startTicks < t1)
        |> List.map .key
        |> Set.fromList


{-| `startTicks` が一致するトークンのキーを逆引きする。`tokenSpans` と完全に同じ均等分割式でタイミングを求めるので、
`ChordSpan.startTicks`（`namedSpans` 由来）は必ずいずれかの `TokenSpan.startTicks` と厳密一致する。ChordStrip の
ダブルクリックで tick 位置から TokenKey を逆引きして FormPicker を開く際に使う。一致がなければ Nothing。
-}
tokenKeyAtTicks : Timeline -> Int -> ChordTrack -> Maybe TokenKey
tokenKeyAtTicks timeline ticks track =
    tokenSpans timeline track
        |> List.filter (\s -> s.startTicks == ticks)
        |> List.head
        |> Maybe.map .key


{-| 選択トークンを deltaBars 小節移動してテキストを再構成する。移動先セルへは末尾追記（上書き・押し出しなし）。
% と = は移動前の resolvedName（実コード名）に展開して移動する（参照先が変わる事故を防ぐため）。
_ と Err はリテラルのまま。変更した小節だけ正規化再構成し、無変更の小節は生チャンク（コメント含む）を温存する。
deltaBars が 0 または選択が空なら何もしない。
-}
moveTokens : Timeline -> Int -> Set TokenKey -> ChordTrack -> { track : ChordTrack, movedKeys : Set TokenKey }
moveTokens timeline deltaBarsRaw selectedKeys track =
    if deltaBarsRaw == 0 || Set.isEmpty selectedKeys then
        { track = track, movedKeys = selectedKeys }

    else
        let
            cellsList =
                cells timeline track

            movedText span =
                case span.result of
                    Ok TRepeat ->
                        Maybe.withDefault span.token span.resolvedName

                    Ok THold ->
                        Maybe.withDefault span.token span.resolvedName

                    _ ->
                        span.token

            spanLookup =
                tokenSpans timeline track
                    |> List.filter (\s -> Set.member s.key selectedKeys)
                    |> List.map (\s -> ( s.key, movedText s ))
                    |> Dict.fromList

            minBar =
                selectedKeys |> Set.toList |> List.map Tuple.first |> List.minimum |> Maybe.withDefault 0

            deltaBars =
                Basics.max deltaBarsRaw (negate minBar)

            rawChunks =
                String.split "|" track.text

            barCountRaw =
                List.length rawChunks

            maxDestBar =
                selectedKeys
                    |> Set.toList
                    |> List.map (\( b, _ ) -> b + deltaBars)
                    |> List.maximum
                    |> Maybe.withDefault 0

            totalBars =
                Basics.max barCountRaw (maxDestBar + 1)

            paddedChunks =
                rawChunks ++ List.repeat (totalBars - barCountRaw) ""

            barTokens b =
                cellsList
                    |> List.filter (\c -> c.barIndex == b)
                    |> List.head
                    |> Maybe.map (\c -> List.indexedMap Tuple.pair c.chords)
                    |> Maybe.withDefault []

            keptTokens b =
                barTokens b
                    |> List.filter (\( j, _ ) -> not (Set.member ( b, j ) selectedKeys))
                    |> List.map (\( _, c ) -> c.token)

            selectedTokensOf b =
                barTokens b
                    |> List.filterMap
                        (\( j, _ ) ->
                            if Set.member ( b, j ) selectedKeys then
                                Dict.get ( b, j ) spanLookup |> Maybe.map (\txt -> ( j, txt ))

                            else
                                Nothing
                        )

            initFinal =
                List.range 0 (totalBars - 1) |> List.map (\b -> ( b, keptTokens b )) |> Dict.fromList

            ( finalTokensDict, keyMapList, touchedBars ) =
                List.range 0 (barCountRaw - 1)
                    |> List.foldl
                        (\b ( finalAcc, mapAcc, touchedAcc ) ->
                            let
                                moving =
                                    selectedTokensOf b
                            in
                            if List.isEmpty moving then
                                ( finalAcc, mapAcc, touchedAcc )

                            else
                                let
                                    destBar =
                                        b + deltaBars

                                    currentDestTokens =
                                        Dict.get destBar finalAcc |> Maybe.withDefault []

                                    baseIndex =
                                        List.length currentDestTokens

                                    appended =
                                        List.indexedMap (\i ( srcIdx, txt ) -> ( srcIdx, txt, baseIndex + i )) moving

                                    newTokens =
                                        List.map (\( _, txt, _ ) -> txt) appended

                                    newMapEntries =
                                        List.map (\( srcIdx, _, newIdx ) -> ( ( b, srcIdx ), ( destBar, newIdx ) )) appended
                                in
                                ( Dict.insert destBar (currentDestTokens ++ newTokens) finalAcc
                                , mapAcc ++ newMapEntries
                                , touchedAcc |> Set.insert b |> Set.insert destBar
                                )
                        )
                        ( initFinal, [], Set.empty )

            newChunks =
                List.range 0 (totalBars - 1)
                    |> List.map
                        (\b ->
                            if Set.member b touchedBars then
                                let
                                    toks =
                                        Dict.get b finalTokensDict |> Maybe.withDefault []
                                in
                                if List.isEmpty toks then
                                    ""

                                else
                                    " " ++ String.join " " toks ++ " "

                            else
                                paddedChunks |> List.drop b |> List.head |> Maybe.withDefault ""
                        )

            newText =
                String.join "|" newChunks

            movedKeys =
                keyMapList |> List.map Tuple.second |> Set.fromList
        in
        { track = { track | text = newText }, movedKeys = movedKeys }


{-| 選択トークンを削除する。| の数（小節構造）は絶対に変えない。変更した小節だけ正規化再構成し、
無変更の小節は生チャンク（コメント含む）を温存する。
-}
removeTokens : Timeline -> Set TokenKey -> ChordTrack -> ChordTrack
removeTokens timeline selectedKeys track =
    if Set.isEmpty selectedKeys then
        track

    else
        let
            cellsList =
                cells timeline track

            rawChunks =
                String.split "|" track.text

            barCountRaw =
                List.length rawChunks

            touchedBars =
                selectedKeys |> Set.toList |> List.map Tuple.first |> Set.fromList

            keptTokens b =
                cellsList
                    |> List.filter (\c -> c.barIndex == b)
                    |> List.head
                    |> Maybe.map
                        (\c ->
                            c.chords
                                |> List.indexedMap Tuple.pair
                                |> List.filter (\( j, _ ) -> not (Set.member ( b, j ) selectedKeys))
                                |> List.map (\( _, tk ) -> tk.token)
                        )
                    |> Maybe.withDefault []

            newChunks =
                List.range 0 (barCountRaw - 1)
                    |> List.map
                        (\b ->
                            if Set.member b touchedBars then
                                let
                                    toks =
                                        keptTokens b
                                in
                                if List.isEmpty toks then
                                    ""

                                else
                                    " " ++ String.join " " toks ++ " "

                            else
                                rawChunks |> List.drop b |> List.head |> Maybe.withDefault ""
                        )
        in
        { track | text = String.join "|" newChunks }


{-| コード進行テキストを丸ごと移調する。`%`・`_`・`=`・改行・空白・`|` はテキストの部分置換で
そのまま保持し、パースできるコードトークンだけを root/bass を +semitones した上で書き直す。
パースできないトークンは無変換のまま残す。`//` コメントの中身は移調対象から除外する。
-}
transpose : Int -> ChordTrack -> ChordTrack
transpose semitones track =
    { track | text = transposeText semitones track.text }


{-| 指定小節範囲（0-based、fromBar から count 小節）だけをまとめて移調する。範囲外の小節はそのまま。
セクションごとの移調で使う。`Data.Project.insertChordBars`/`removeChordBars` と同じ「`|` で split して
一部だけ変えて join」パターン。
-}
transposeBars : Int -> Int -> Int -> ChordTrack -> ChordTrack
transposeBars fromBar count semitones track =
    let
        bars =
            String.split "|" track.text

        before =
            List.take fromBar bars

        target =
            List.take count (List.drop fromBar bars)

        after =
            List.drop (fromBar + count) bars
    in
    { track | text = String.join "|" (before ++ List.map (transposeText semitones) target ++ after) }


type alias TextSegment =
    { content : String
    , isWord : Bool
    }


isDelim : Char -> Bool
isDelim c =
    c == ' ' || c == '\n' || c == '\t' || c == '|'


{-| テキストを「区切り文字の連続」と「単語（区切り文字以外）の連続」に分割する。
全セグメントを連結すると元のテキストに一致するので、区切り文字側は完全に保持できる。
-}
tokenize : String -> List TextSegment
tokenize text =
    String.toList text
        |> List.foldl
            (\c segs ->
                let
                    wordChar =
                        not (isDelim c)
                in
                case segs of
                    seg :: rest ->
                        if seg.isWord == wordChar then
                            { seg | content = seg.content ++ String.fromChar c } :: rest

                        else
                            { content = String.fromChar c, isWord = wordChar } :: seg :: rest

                    [] ->
                        [ { content = String.fromChar c, isWord = wordChar } ]
            )
            []
        |> List.reverse


transposeText : Int -> String -> String
transposeText semitones text =
    splitComments text
        |> List.map
            (\chunk ->
                case chunk of
                    CodeChunk s ->
                        tokenize s
                            |> List.map
                                (\seg ->
                                    if seg.isWord then
                                        transposeWord semitones seg.content

                                    else
                                        seg.content
                                )
                            |> String.concat

                    CommentChunk s ->
                        s
            )
        |> String.concat


transposeWord : Int -> String -> String
transposeWord semitones word =
    case word of
        "%" ->
            word

        "_" ->
            word

        "=" ->
            word

        _ ->
            case ChordParser.parse word of
                Ok chord ->
                    ChordFormat.format { preferFlat = False } (transposeChord semitones chord)

                Err _ ->
                    word


transposeChord : Int -> Chord -> Chord
transposeChord semitones chord =
    { chord
        | root = modBy 12 (chord.root + semitones)
        , bass = Maybe.map (\b -> modBy 12 (b + semitones)) chord.bass
    }


{-| ボイシング指定（`@NAME`）を落としたプレーンなテキストを返す。第三者に渡すコード譜のコピー用。
`transposeText` と同じ構造で、トークン単位にパース→`formatPlain`で書き戻すだけが違う。
パースできないトークン・`%`/`_`/`=`・コメント・改行はそのまま残る。
-}
toPlainText : ChordTrack -> String
toPlainText track =
    splitComments track.text
        |> List.map
            (\chunk ->
                case chunk of
                    CodeChunk s ->
                        tokenize s
                            |> List.map
                                (\seg ->
                                    if seg.isWord then
                                        plainWord seg.content

                                    else
                                        seg.content
                                )
                            |> String.concat

                    CommentChunk s ->
                        s
            )
        |> String.concat


{-| `toPlainText` の出力を `barsPerLine` 小節ごとに改行して整形する（コード譜コピー用）。行のコード部の
小節数が `barsPerLine` 以下ならその行は完全に無変更で返す（空白・区切りも含めて元のまま）。それを超える行の
み、`|` 区切りの小節を `barsPerLine` 個ずつのチャンクに分け直し、チャンクごとに改行する。コメント（`//`
以降）は再整形せず、行末に1つ空白を挟んで付け直す。

再整形されたチャンクの先頭小節が空文字列の場合は行頭に `"| "` を前置する。`Data.ChordSheet.parse` の
`barsOfContent` は行頭・行末の空チャンクをそれぞれ1つ落とす仕様（`toSheetText.barsLine` と同じ規約）なので、
これを省くと再パース時に空小節が1つ失われる。4小節超の行のみ小節内の空白が正規化される点に注意。
-}
wrapBarLines : Int -> String -> String
wrapBarLines barsPerLine text =
    String.split "\n" text
        |> List.map (wrapLine barsPerLine)
        |> String.join "\n"


wrapLine : Int -> String -> String
wrapLine barsPerLine line =
    case String.indexes "//" line of
        idx :: _ ->
            let
                code =
                    String.left idx line

                comment =
                    String.dropLeft idx line

                wrapped =
                    wrapCode barsPerLine code
            in
            if wrapped == code then
                line

            else
                wrapped ++ " " ++ comment

        [] ->
            wrapCode barsPerLine line


wrapCode : Int -> String -> String
wrapCode barsPerLine code =
    let
        bars =
            barsOfLineContent code
    in
    if List.length bars <= barsPerLine then
        code

    else
        chunksOf barsPerLine bars
            |> List.map wrappedBarsLine
            |> String.join "\n"


{-| `Data.ChordSheet.barsOfContent` と同じ規約（行頭・行末の空チャンクを1つずつ落とす、各小節をtrim）で
`|` 区切りの小節リストを取り出す。
-}
barsOfLineContent : String -> List String
barsOfLineContent content =
    let
        dropFirstIfBlank chunks =
            case chunks of
                first :: rest ->
                    if first == "" then
                        rest

                    else
                        chunks

                [] ->
                    []

        dropLastIfBlank chunks =
            chunks |> List.reverse |> dropFirstIfBlank |> List.reverse
    in
    String.split "|" content
        |> List.map String.trim
        |> dropFirstIfBlank
        |> dropLastIfBlank


chunksOf : Int -> List a -> List (List a)
chunksOf n xs =
    case xs of
        [] ->
            []

        _ ->
            List.take n xs :: chunksOf n (List.drop n xs)


{-| `Data.ChordSheet.toSheetText` の `barsLine` と同じ規約（先頭小節が空なら `"| "` を前置、行末は常に
`" |"`）で小節リストを1行のテキストに戻す。
-}
wrappedBarsLine : List String -> String
wrappedBarsLine bars =
    let
        leading =
            if (List.head bars |> Maybe.withDefault "") == "" then
                "| "

            else
                ""
    in
    leading ++ String.join " | " bars ++ " |"


plainWord : String -> String
plainWord word =
    case word of
        "%" ->
            word

        "_" ->
            word

        "=" ->
            word

        _ ->
            case ChordParser.parse word of
                Ok chord ->
                    ChordFormat.formatPlain { preferFlat = False } chord

                Err _ ->
                    word


{-| 対象 1 トークンのテキストだけを f で書き換える。`removeTokens` と同じ「`|` で split → 対象小節だけ再構成、
他の小節は生チャンク温存」パターンを踏襲する。変更した小節のコメントは消える（`moveTokens`/`removeTokens`
と同一仕様）。key の barIndex が範囲外なら何もしない。
-}
updateToken : Timeline -> TokenKey -> (String -> String) -> ChordTrack -> ChordTrack
updateToken timeline ( barIdx, tokenIdx ) f track =
    let
        cellsList =
            cells timeline track

        rawChunks =
            String.split "|" track.text

        barCountRaw =
            List.length rawChunks

        newTokensForBar b =
            cellsList
                |> List.filter (\c -> c.barIndex == b)
                |> List.head
                |> Maybe.map
                    (\c ->
                        c.chords
                            |> List.indexedMap Tuple.pair
                            |> List.map
                                (\( j, tk ) ->
                                    if j == tokenIdx then
                                        f tk.token

                                    else
                                        tk.token
                                )
                    )
                |> Maybe.withDefault []
    in
    if barIdx < 0 || barIdx >= barCountRaw then
        track

    else
        let
            newChunks =
                List.range 0 (barCountRaw - 1)
                    |> List.map
                        (\b ->
                            if b == barIdx then
                                let
                                    toks =
                                        newTokensForBar b
                                in
                                if List.isEmpty toks then
                                    ""

                                else
                                    " " ++ String.join " " toks ++ " "

                            else
                                rawChunks |> List.drop b |> List.head |> Maybe.withDefault ""
                        )
        in
        { track | text = String.join "|" newChunks }


{-| 既存の `@NAME` 部分を除去し、末尾に `@name` を追記する。parse→format だとクオリティが正規化されて表記が壊れる
（例: FM7 が Fmaj7 になる）ため、テキスト操作だけで行う。スラッシュベース（`/BASS`）がある場合は
`withoutVoicingName` で保ったまま末尾に `@name` を付ける（`MAIN@OLD/BASS` → `MAIN/BASS@name`）。
-}
withVoicingName : String -> String -> String
withVoicingName name token =
    withoutVoicingName token ++ "@" ++ name


{-| `@NAME`（ボイシング指定）を除去する。`/BASS` が後ろにあっても保つ。`@` がなければそのまま返す。
-}
withoutVoicingName : String -> String
withoutVoicingName token =
    case String.indexes "@" token of
        [ atIdx ] ->
            let
                afterAt =
                    String.dropLeft (atIdx + 1) token
            in
            case String.indexes "/" afterAt of
                slashIdx :: _ ->
                    String.left atIdx token ++ String.dropLeft slashIdx afterAt

                [] ->
                    String.left atIdx token

        _ ->
            token
