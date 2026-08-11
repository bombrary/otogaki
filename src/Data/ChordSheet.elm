module Data.ChordSheet exposing
    ( Block
    , ParseError
    , applyToProjectPreservingIds
    , parse
    , toChordText
    , toSections
    , toSheetText
    )

{-| コード譜テキスト（空行区切り＋任意タイトル行＋`|` 小節区切り）をパースして、
Section 列とコードトラックのテキストに変換する。

入力フォーマットの例:

    intro
    FM7 | G7/F | Em7/F | Am/F |
    FM7 | G7/F | Em7/F | Am/F |

    A
    Fmaj7 | G7/F | Em7 | Am7 |

パース仕様はこのモジュールの doc コメントが情報源。詳細は各関数を参照。
-}

import Data.ChordTrack exposing (ChordTrack)
import Data.Key exposing (Key)
import Data.Meter exposing (Meter)
import Data.Project exposing (Project)
import Data.Section exposing (Section)


type alias Block =
    { title : Maybe String
    , bars : List String
    , startLine : Int
    }


type alias ParseError =
    { line : Int, message : String }


type LineKind
    = LineBar String
    | LineText String


type alias ClassifiedLine =
    { lineNo : Int, kind : LineKind }


type Line
    = Separator
    | Ignored
    | Content ClassifiedLine


normalizeBarChars : String -> String
normalizeBarChars =
    String.replace "┃" "|"
        >> String.replace "│" "|"
        >> String.replace "｜" "|"


stripLineComment : String -> String
stripLineComment line =
    case String.indexes "//" line of
        idx :: _ ->
            String.left idx line

        [] ->
            line


lineOf : Int -> String -> Line
lineOf lineNo rawLine =
    if String.trim rawLine == "" then
        Separator

    else
        let
            content =
                stripLineComment rawLine |> String.trim |> normalizeBarChars
        in
        if content == "" then
            Ignored

        else if String.contains "|" content then
            Content { lineNo = lineNo, kind = LineBar content }

        else
            Content { lineNo = lineNo, kind = LineText content }


groupBlocks : List Line -> List (List ClassifiedLine)
groupBlocks lines =
    let
        step line ( current, acc ) =
            case line of
                Separator ->
                    if List.isEmpty current then
                        ( [], acc )

                    else
                        ( [], acc ++ [ List.reverse current ] )

                Ignored ->
                    ( current, acc )

                Content cl ->
                    ( cl :: current, acc )

        ( lastCurrent, doneBlocks ) =
            List.foldl step ( [], [] ) lines
    in
    if List.isEmpty lastCurrent then
        doneBlocks

    else
        doneBlocks ++ [ List.reverse lastCurrent ]


findNonBar : List ClassifiedLine -> Maybe ClassifiedLine
findNonBar lines =
    lines
        |> List.filter
            (\l ->
                case l.kind of
                    LineText _ ->
                        True

                    LineBar _ ->
                        False
            )
        |> List.head


barsOfContent : String -> List String
barsOfContent content =
    let
        dropFirstIfBlank chunks =
            case chunks of
                first :: rest ->
                    if String.trim first == "" then
                        rest

                    else
                        chunks

                [] ->
                    []

        dropLastIfBlank chunks =
            chunks |> List.reverse |> dropFirstIfBlank |> List.reverse
    in
    String.split "|" content
        |> dropFirstIfBlank
        |> dropLastIfBlank
        |> List.map String.trim


barsOfLine : ClassifiedLine -> List String
barsOfLine l =
    case l.kind of
        LineBar content ->
            barsOfContent content

        LineText _ ->
            []


buildBlock : List ClassifiedLine -> Result ParseError Block
buildBlock lines =
    case lines of
        [] ->
            Err { line = 0, message = "空のブロックです" }

        first :: rest ->
            case findNonBar rest of
                Just badLine ->
                    Err { line = badLine.lineNo, message = "小節区切り | のない行です" }

                Nothing ->
                    let
                        ( title, ownBars ) =
                            case first.kind of
                                LineText t ->
                                    ( Just t, [] )

                                LineBar b ->
                                    ( Nothing, barsOfContent b )

                        bars =
                            ownBars ++ (rest |> List.concatMap barsOfLine)
                    in
                    if List.isEmpty bars then
                        Err { line = first.lineNo, message = "小節（| 区切り）を含まないブロックです" }

                    else
                        Ok { title = title, bars = bars, startLine = first.lineNo }


combineResults : List (Result e a) -> Result e (List a)
combineResults results =
    List.foldl
        (\r acc -> Result.andThen (\accList -> Result.map (\v -> accList ++ [ v ]) r) acc)
        (Ok [])
        results


parse : String -> Result ParseError (List Block)
parse raw =
    let
        normalizedNewlines =
            raw
                |> String.replace "\u{000D}\n" "\n"
                |> String.replace "\u{000D}" "\n"

        lines =
            String.split "\n" normalizedNewlines
                |> List.indexedMap (\i l -> lineOf (i + 1) l)

        blocks =
            groupBlocks lines
    in
    if List.isEmpty blocks then
        Err { line = 1, message = "コード譜が空です" }

    else
        blocks |> List.map buildBlock |> combineResults


toSections : { firstId : Int, key : Key, meter : Meter } -> List Block -> List Section
toSections cfg blocks =
    List.indexedMap
        (\i b ->
            { id = cfg.firstId + i
            , name = Maybe.withDefault ("セクション " ++ String.fromInt (i + 1)) b.title
            , lengthBars = List.length b.bars
            , memo = ""
            , key = cfg.key
            , meter = cfg.meter
            }
        )
        blocks


toChordText : List Block -> String
toChordText blocks =
    blocks
        |> List.map (\b -> String.join " | " b.bars)
        |> String.join " |\n"


{-| 現在のセクション列とコードトラックから、`parse` が受け付けるテキストを組み立てる（`toChordText` の逆方向）。

小節は生の文字列をそのまま使う（コメントや @NAME も保存される）。各セクションは名前の行と、
小節を縦棒で並べた1行（行末に常に縦棒を付ける）の組で構成する。行頭の縦棒は先頭小節が空のときだけ
付ける（parse 側が行頭・行末の空チャンクをそれぞれ1つ落とす仕様に合わせ、空小節を往復で失わないようにするため）。

既知の制限: 小節内コメントは同じ行の後続小節ごと消えうる。セクション合計を超えるオーバーフロー小節は
出力に含まれず、書き戻すと失われる。
-}
toSheetText : List Section -> ChordTrack -> String
toSheetText sections track =
    let
        rawBars =
            String.split "|" track.text |> List.map String.trim

        totalBars =
            List.foldl (\s acc -> acc + s.lengthBars) 0 sections

        paddedBars =
            rawBars ++ List.repeat (Basics.max 0 (totalBars - List.length rawBars)) ""

        barsLine bars =
            let
                leading =
                    if (List.head bars |> Maybe.withDefault "") == "" then
                        "| "

                    else
                        ""
            in
            leading ++ String.join " | " bars ++ " |"

        step s ( offset, acc ) =
            let
                bars =
                    paddedBars |> List.drop offset |> List.take s.lengthBars
            in
            ( offset + s.lengthBars, acc ++ [ s.name ++ "\n" ++ barsLine bars ] )
    in
    List.foldl step ( 0, [] ) sections
        |> Tuple.second
        |> String.join "\n\n"


{-| ブロック列をプロジェクトへ反映する。位置ベースで既存セクションの id / key / meter / memo を引き継いで
ID を安定させる（テキスト入力のたびに呼ばれるため、毎回振り直すと選択状態が崩れる）。

インデックス i のブロックは、既存 sections の同じ位置にセクションがあれば id/key/meter/memo を引き継ぎ、
name と lengthBars だけ上書きする（name は title があればそれ、なければ自動命名）。既存セクション数を超える
ブロックには nextId から新規 ID を振る。ブロック数が既存より少なければ末尾セクションは消える。
-}
applyToProjectPreservingIds : List Block -> Project -> Project
applyToProjectPreservingIds blocks project =
    let
        existingCount =
            List.length project.sections

        sectionAt i =
            project.sections |> List.drop i |> List.head

        toSection i b =
            let
                name =
                    Maybe.withDefault ("セクション " ++ String.fromInt (i + 1)) b.title

                lengthBars =
                    List.length b.bars
            in
            case sectionAt i of
                Just existing ->
                    { existing | name = name, lengthBars = lengthBars }

                Nothing ->
                    { id = project.nextId + (i - existingCount)
                    , name = name
                    , lengthBars = lengthBars
                    , memo = ""
                    , key = Data.Key.default
                    , meter = Data.Meter.default
                    }

        newIdCount =
            Basics.max 0 (List.length blocks - existingCount)

        ct =
            project.chordTrack
    in
    { project
        | sections = List.indexedMap toSection blocks
        , chordTrack = { ct | text = toChordText blocks }
        , nextId = project.nextId + newIdCount
    }
