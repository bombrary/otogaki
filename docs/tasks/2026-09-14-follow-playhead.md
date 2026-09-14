# 2026-09-14 再生追従を GarageBand 方式にする

作業者: Sonnet（headless）。計画: Fable。

## 共通ルール

- 作業ディレクトリは `~/repos/otogaki`。ツールは `nix develop -c <cmd>` で呼ぶ（`elm-test`, `npx vite build`, `elm-format`）。
- 全体で 1 コミット。コミットメッセージは既存の流儀（`feat(scope): 日本語で何を変えたか`）。本文末尾に
  `Co-Authored-By: Claude Sonnet <noreply@anthropic.com>` を付ける。
- コミット前に必ず `nix develop -c elm-test` と `nix develop -c npx vite build` を通す。失敗したままコミットしない。
- 触った Elm ファイルは `nix develop -c elm-format --yes <file>` で整形する。
- 指定外の機能追加・リファクタはしない。分からない点は推測で拡張せず、最小の変更で止める。
- 最後に、合格条件チェックボックスを埋めた短い報告を標準出力に書く（ファイル名と行番号付き）。ブラウザ確認はできないので「未検証」と正直に書く。

## 背景と設計判断

利用者は iPad で使っている。現状の「📌 追従」は永続的な ON/OFF モードで、タッチ向けのページ型レイアウトでは ☰ メニューの中に隠れているため、再生中に切り替えられない。

設計判断: 追従を「モード」ではなく「意図の帰結」にする（GarageBand と同じ）。

- 再生を始めた ＝ 見ていたい → 追従 ON
- 再生中にピアノロールやセクションバーをタップ／スワイプ／ホイールした ＝ 別の場所を見たい → 追従 OFF
- 追従 OFF の間だけ「再生位置へ戻る」ボタンを出す。それ以外のときは何も出さない

これにより永続トグルの 📌 ボタンは不要になる。デスクトップも同じ挙動に統一する（挙動を 2 種類にしない）。

## 現状（読んで確認すること。行番号は目安）

- `src/Main.elm`
  - `Model.followPlayhead : Bool`（204 行）、初期値 `True`（599 行）
  - `ToggledFollowPlayhead` Msg（483 行）とハンドラ（5991 行）: 単純な反転
  - `GotAudio (Playhead ticks)`（3573 行）: `followPlayhead` が真なら `GotPianoRollViewport` / `GotSectionBarViewport` で可視範囲外のときだけ `setViewportOf` する（6021 行〜）。横方向のみ。
  - `revealPlayheadCmd`（2371 行）: 手動シーク時に追従フラグと無関係にプレイヘッドを見せる。これは変えない。
  - `startPlay`（2018 行）: すべての再生開始経路（`ClickedPlay`、Space、ループ設定変更時の再スタート等）が最終的にここを通る。`playWithLoop`（2699 行）はその手前。
  - `followGroup`（7200 行）: 📌 ボタン。`loopGroup = div groupStyle [ loopOnlyGroup, followGroup ]`（7212 行）と、ページ型レイアウトの ☰ メニュー内（7365 行）で使われている。
  - `playStopGroup`（7109 行）: ▶ 再生 / ■ 停止。全レイアウトで常時表示。
  - 3503 行付近に `"follow" ->` という分岐がある。キーボードショートカットかコマンド名の対応表と思われる。何に使われているか読んで、下の方針に合わせる。
  - `ScrolledPianoRoll`（6121 行）は DOM の `scroll` イベント。**プログラムからの `setViewportOf` でも発火するので、利用者操作の検出には使えない**。
- `src/View/PianoRoll.elm`
  - `scrollFrame`（413 行）: `FrameOpts` を受けて `overflow: auto` の枠を作る。ピアノロール本体とコード進行トラックの鍵盤列あり表示で共有。`Html.Events.on "scroll"` は 439 行。
  - ノートセル等は `pointerdown` を `stopPropagationOn` している箇所がある（993, 1236 行付近）。`touchstart` と `wheel` は別イベントなので枠まで届く。
- `src/View/SectionBar.elm`
  - `sectionBarScrollId` の div（344 行）: `overflow-x: auto`。追従スクロールの対象。
- `src/Help.elm` / `README.md`: 「追従」「📌」の説明があれば挙動に合わせて直す。

## やること

### 1. 追従フラグの意味を変える

- `followPlayhead` は「利用者の設定」ではなく「今、追従中か」を表す一時状態にする。フィールド名はそのままでよい（コメントを直す）。
- `startPlay` の返す Model で `followPlayhead = True` にする。ここ 1 箇所で全経路をカバーする。
- `ToggledFollowPlayhead` は削除し、代わりに次の 2 つの Msg を追加する:
  - `TouchedScrollSurface` : 利用者がスクロール面に触れた。`model.playState == Playing` のときだけ `followPlayhead = False` にする。停止中は何もしない（Model を変えない）。
  - `ResumedFollowPlayhead` : `followPlayhead = True` にして `revealPlayheadCmd model.playheadTicks` を発行する。
- 3503 行付近の `"follow"` 分岐が `ToggledFollowPlayhead` を指していたら、`ResumedFollowPlayhead` に差し替える（ショートカットなら「再生位置へ戻る」の意味になる）。ヘルプに載っていれば文言も直す。

### 2. 利用者操作の検出

- `PianoRoll.FrameOpts` に `touched : msg` を追加し、`scrollFrame` の外側 div（`HA.id f.scrollId` の要素）に次を付ける:
  - `Html.Events.on "touchstart" (Decode.succeed f.touched)`
  - `Html.Events.on "wheel" (Decode.succeed f.touched)`
  - `Html.Events.on "pointerdown" (Decode.succeed f.touched)`
  - いずれも `preventDefault` / `stopPropagation` しない（既存のズーム・ドラッグ・スクロールを邪魔しない）。
- `scrollFrame` の呼び出し元（`PianoRoll.view` と、コード進行トラック側の呼び出し）すべてで `touched` を渡す。`PianoRoll.Config` に `touched : msg` を追加して Main.elm から `TouchedScrollSurface` を渡す。
- `SectionBar` の `sectionBarScrollId` の div にも同じ 3 イベントを付け、`SectionBar` の config 経由で `TouchedScrollSurface` を渡す。
- 既知の割り切り（そのままでよい、直さない）:
  - 縦スワイプでも追従が切れる（GarageBand と同じ）。
  - ホイールズーム（ルーラー上の wheel）でも追従が切れる。
  - マウスでノートを押した場合は `pointerdown` が `stopPropagation` されて枠に届かないため追従は切れない。タッチでは `touchstart` が届くので切れる。

### 3. 「再生位置へ戻る」ボタン

- `followGroup` と `loopGroup` を削除し、`loopGroup` を使っていた箇所は `loopOnlyGroup` に置き換える。☰ メニュー内の `followGroup` も消す。
- `playStopGroup` の `■ 停止` の右に、**`model.playState == Playing && not model.followPlayhead` のときだけ** ボタンを 1 つ描画する（それ以外は `text ""`）:
  - ラベル `📌 再生位置へ`、`onClick ResumedFollowPlayhead`
  - `title` は「再生位置までスクロールして追従を再開」
  - 見た目は `Style.baseButton`（トグル表示は不要。押したら消えるので）
- `playStopGroup` は全レイアウトで共有されているので、これだけでデスクトップ・コンパクト・ページ型すべてに出る。

### 4. ドキュメント

- `README.md` と `src/Help.elm` に「📌 追従」トグルの説明があれば、新しい挙動（再生で自動追従、スワイプで解除、📌 再生位置へ で復帰）に書き換える。

## 合格条件

- [ ] `ToggledFollowPlayhead` が残っていない（`grep` で 0 件）
- [ ] `followGroup` / `loopGroup` が残っていない（`grep` で 0 件）
- [ ] `startPlay` で `followPlayhead = True` になる
- [ ] `TouchedScrollSurface` は `playState == Playing` のときだけ `followPlayhead` を `False` にする
- [ ] `ResumedFollowPlayhead` は `followPlayhead = True` と `revealPlayheadCmd` の両方を行う
- [ ] `scrollFrame` の外側 div と `sectionBarScrollId` の div に `touchstart` / `wheel` / `pointerdown` が付いている
- [ ] `📌 再生位置へ` は `Playing && not followPlayhead` のときだけ `playStopGroup` に描画される
- [ ] README / Help の追従の説明が新しい挙動になっている
- [ ] elm-test / vite build 通過、elm-format 済み
- [ ] 1 コミット、Co-Authored-By 付き
