# 2026-09-14 「📌 再生位置へ」をヘッダーからピアノロール上のフロートに移す

作業者: Sonnet（headless）。計画: Fable。前提: `docs/tasks/2026-09-14-follow-playhead.md` の実装（コミット 8f62c60, 71cc451）が入っている。

## 共通ルール

- 作業ディレクトリは `~/repos/otogaki`。ツールは `nix develop -c <cmd>` で呼ぶ（`elm-test`, `npx vite build`, `elm-format`）。
- 1 コミット。メッセージは `fix(playhead): ...`、本文末尾に `Co-Authored-By: Claude Sonnet <noreply@anthropic.com>`。
- コミット前に `nix develop -c elm-test` と `nix develop -c npx vite build` を通す。触った Elm ファイルは `nix develop -c elm-format --yes <file>`。
- 指定外の変更はしない。最後に合格条件を埋めた短い報告を標準出力に書く（ファイル名と行番号付き）。ブラウザ確認はできないので「未検証」と書く。

## 背景

ブラウザで確認したところ、追従の状態遷移（再生で ON → 操作で OFF → 📌 で復帰）は正しく動いている。ただし「📌 再生位置へ」を `playStopGroup` に入れたため、ボタンが出たり消えたりするたびにヘッダーの flex-wrap が折り返し直され、ツールバー全体が動く。ページ型レイアウト（iPad）では確実に起きる。

設計判断: ボタンをヘッダーから外し、**スクロール面の右下に重ねてフロート**させる（マップアプリの「現在地へ」と同じ）。ヘッダーのレイアウトは一切動かず、スワイプした指の近くに出る。

## やること

### 1. ヘッダーから外す

- `src/Main.elm` の `playStopGroup`（7121 行付近）から、`model.playState == Playing && not model.followPlayhead` で描画している `📌 再生位置へ` ボタンの分岐を削除する（`▶ 再生` `■ 停止` の 2 つだけに戻す）。

### 2. フロートボタンのヘルパーを作る

`src/Main.elm` に次のヘルパーを足す（名前は目安）:

```elm
{-| 追従が外れている間だけ、スクロール面の右下に「📌 再生位置へ」を重ねる。
ラッパーは `pr-col pr-fill` なので、親が `pr-col` のときは中の `.pr-fill`（scrollFrame）が残り高さに収まる契約をそのまま引き継ぐ。
親が `pr-col` でないときは何もしない（`.pr-fill` 単体にスタイルはない）。
-}
withFollowOverlay : Model -> Html Msg -> Html Msg
withFollowOverlay model surface =
    div [ Html.Attributes.class "pr-col pr-fill", style "position" "relative" ]
        [ surface
        , if model.playState == Playing && not model.followPlayhead then
            button
                (Style.baseButton
                    ++ [ onClick ResumedFollowPlayhead
                       , Html.Attributes.title "再生位置までスクロールして追従を再開"
                       , style "position" "absolute"
                       , style "right" "16px"
                       , style "bottom" "16px"
                       , style "z-index" "5"
                       , style "box-shadow" "0 2px 8px rgba(0,0,0,0.35)"
                       ]
                )
                [ text "📌 再生位置へ" ]

          else
            text ""
        ]
```

- `View/Theme.elm` の `.pr-col > .pr-fill { flex: 1 1 auto; min-height: 0 }` / `.pr-col > * { flex: 0 0 auto }`（762-767 行付近）を読んで、上のラッパーが既存の flex チェーンを壊さないことを確認する。scrollFrame は `pr-fill pr-frame` を持つので、ラッパー（pr-col）の中で従来通り残り高さに収まる。
- ボタンの見た目は既存の `Style.baseButton`（.m3-btn）を使う。タッチ向けのパディング拡大は `.touch-ui .m3-btn` 側で既に効くはず。背景が透けていて読みにくければ `style "background" Theme.surfaceContainerHigh` を足す。

### 3. スクロール面をラップする

`pianoRollScrollId` を持つ要素を描画している Main.elm 側の呼び出しをすべて `withFollowOverlay model` で包む:

- 通常トラックの `PianoRoll.view ...`（`pianoRollView` 付近）
- コード進行トラックの `PianoRoll.chordTrackView ...`（`chordTrackMainView` 付近。軽量表示・鍵盤列あり表示の両方がこの 1 箇所を通るはず。読んで確認）
- ドラムの `DrumEditor.view ...`（6922 行付近）

各呼び出し元の親が `pr-col` であるかどうかを読み、ラップしても高さの振る舞いが変わらないことをコード上で確認する（見た目の検証は Fable がやる）。もしある呼び出し元で scrollFrame の高さが `pr-fill` ではなく `max-height` だけで決まっているなら、ラッパーは `pr-col pr-fill` でも害はない（子は `flex: 0 0 auto` で自然高さ）。

### 4. Help

- `src/Data/Help.elm` 463 行の `📌 再生位置へ` の説明に「ピアノロールの右下に表示」と場所を一言足す。

## 合格条件

- [ ] `playStopGroup` に `再生位置へ` が残っていない
- [ ] `withFollowOverlay`（相当）があり、`Playing && not followPlayhead` のときだけ `position: absolute` のボタンを描画する
- [ ] 通常トラック・コード進行トラック・ドラムの 3 箇所がラップされている
- [ ] Help の説明に表示場所がある
- [ ] elm-test / vite build 通過、elm-format 済み、1 コミット
