module Data.Voicing exposing (Voicing, anchorPitch, displayRoot, empty, findByName, pitchesFor, removeOffsets, shiftOffsets)

import Set exposing (Set)


{-| offset 0 が置かれる MIDI ピッチ。`Data.Chord.toPitchesWith` のフォールバック経路が
ベース音を置く位置（`36 + ピッチクラス`）と同じに揃えてある。こうすることで、ボイシングは
自分でベース音まで含めて書けば十分になり、自動でベース音を足す必要がなくなる。
-}
anchorPitch : Int
anchorPitch =
    36


type alias Voicing =
    { name : String
    , offsets : List Int -- anchorPitch + root からの半音オフセット。常に -12 以上（転回形・ハイブリッドコードで root より低いベース音を許容）
    , stringPicks : Set ( Int, Int ) -- (offset, 弦インデックス)。押さえた位置の運指メモ。Data.GuitarForm.StringPicks と同型
    }


empty : String -> Voicing
empty name =
    { name = name, offsets = [], stringPicks = Set.empty }


{-| 登録済みボイシングを名前で引く。`Data.Chord.toPitchesWith` と `Data.StrumExpand.soundingPitches` の
両方がこのマッチングロジックを共有する。
-}
findByName : String -> List Voicing -> Maybe Voicing
findByName name voicings =
    List.filter (\v -> v.name == name) voicings |> List.head


{-| root のピッチクラス（0-11）を与えて実際に鳴らす MIDI ピッチ列を得る。
鍵盤表示・「全部鳴らす」・実際のコード再生のすべてがこの関数を通る。
-}
pitchesFor : Int -> Voicing -> List Int
pitchesFor rootPc voicing =
    List.map (\offset -> anchorPitch + modBy 12 rootPc + offset) voicing.offsets


{-| 表示上の root。実際に置かれている最低音。まだ 1 音もないときは基準の rootPitch をそのまま返す。
鍵盤の root 線の描画や、編集を開いたときのスクロール先に使う。offset ↔ 絶対ピッチの変換基準（rootPitch 自体）とは
別のものなので混同しないこと。
-}
displayRoot : Int -> List Int -> Int
displayRoot rootPitch offsets =
    offsets |> List.minimum |> Maybe.map ((+) rootPitch) |> Maybe.withDefault rootPitch


{-| 選択中の offset だけを delta 分シフトする。0〜maxOffset にクランプし、移動先が既存の別 offset
と重複したら後の方（既にリストに入っているもの）を残して片方を捨てる。
ボイシング鍵盤のドラッグ移動（delta = マウス移動量から算出）と矢印キー移動（delta = ±1 / ±12）の
両方がこの関数を通る。
-}
shiftOffsets : Int -> Int -> Set Int -> List Int -> List Int
shiftOffsets delta maxOffset selected offsets =
    offsets
        |> List.map
            (\o ->
                if Set.member o selected then
                    clamp -12 maxOffset (o + delta)

                else
                    o
            )
        |> List.foldl
            (\o acc ->
                if List.member o acc then
                    acc

                else
                    acc ++ [ o ]
            )
            []


{-| 選択中の offset を削除する。
-}
removeOffsets : Set Int -> List Int -> List Int
removeOffsets selected offsets =
    List.filter (\o -> not (Set.member o selected)) offsets
