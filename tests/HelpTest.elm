module HelpTest exposing (suite)

import Data.Help as Help
import Expect
import Set
import Test exposing (Test, describe, test)


{-| `Help.TopicId` はカスタム型なので `comparable` ではなく `Set`/`Dict` のキーにできない。
一意性の検査は `Help.domId`（各コンストラクタを重複なく文字列化する全体関数）を介して行う。
-}
allTopicIds : List Help.TopicId
allTopicIds =
    [ Help.Transport
    , Help.EditKeys
    , Help.SelectKeys
    , Help.VoicingKeys
    , Help.KeyboardPlay
    , Help.PianoRollOps
    , Help.VelocityLane
    , Help.LoopOps
    , Help.SectionOps
    , Help.ChordOps
    , Help.DrumApply
    , Help.DrumPresets
    , Help.TrackOps
    , Help.Modifiers
    , Help.TouchOps
    , Help.FileOps
    , Help.RefAudioOps
    , Help.ScrapOps
    , Help.PaneOps
    , Help.ChordSheet
    , Help.VoicingNotation
    , Help.VoicingEditorOps
    , Help.Degrees
    , Help.Icons
    , Help.Terms
    ]


suite : Test
suite =
    describe "Data.Help"
        [ test "domId は TopicId の全コンストラクタで重複しない" <|
            \_ ->
                let
                    domIds =
                        List.map Help.domId allTopicIds
                in
                Expect.equal (List.length domIds) (Set.size (Set.fromList domIds))
        , test "topics は TopicId 全種を過不足なく 1 つずつ含む" <|
            \_ ->
                Expect.equal
                    (List.sort (List.map Help.domId allTopicIds))
                    (List.sort (List.map (.id >> Help.domId) Help.topics))
        , test "tabs の各タブに 1 つ以上トピックがある" <|
            \_ ->
                Help.tabs
                    |> List.map Help.topicsIn
                    |> List.all (\ts -> not (List.isEmpty ts))
                    |> Expect.equal True
        , test "すべての Topic.title が非空" <|
            \_ ->
                Help.topics
                    |> List.all (\t -> String.trim t.title /= "")
                    |> Expect.equal True
        , test "すべての Line.desc が非空" <|
            \_ ->
                Help.topics
                    |> List.concatMap .lines
                    |> List.all (\l -> String.trim l.desc /= "")
                    |> Expect.equal True
        , test "トピック単体では Line.term が重複しない（トピックをまたぐ重複は、VoicingKeys のように別モードで同じキーを使う意図的なものなので許容する）" <|
            \_ ->
                Help.topics
                    |> List.all
                        (\t ->
                            let
                                terms =
                                    List.map .term t.lines
                            in
                            List.length terms == Set.size (Set.fromList terms)
                        )
                    |> Expect.equal True
        , test "tabOf は topics の tab と一致する" <|
            \_ ->
                Help.topics
                    |> List.all (\t -> Help.tabOf t.id == t.tab)
                    |> Expect.equal True
        , test "文脈ヘルプが参照する代表的な TopicId が find で引ける" <|
            \_ ->
                [ Help.DrumApply, Help.LoopOps, Help.FileOps, Help.ChordSheet, Help.VoicingEditorOps ]
                    |> List.all (\id -> Help.find id /= Nothing)
                    |> Expect.equal True
        ]
