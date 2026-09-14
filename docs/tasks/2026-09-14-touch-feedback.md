# 2026-09-14 タッチ利用者フィードバック対応（5 件）

作業者: Sonnet（headless）。計画: Fable。

## 共通ルール

- 作業ディレクトリは `~/repos/otogaki`。ツールは `nix develop -c <cmd>` で呼ぶ（`elm-test`, `npx vite build`, `elm-format`）。
- **M ごとに 1 コミット**。コミットメッセージは既存の流儀（`fix(scope): 日本語で何を直したか`）。本文末尾に
  `Co-Authored-By: Claude Sonnet <noreply@anthropic.com>` を付ける。
- コミット前に必ず `nix develop -c elm-test` と `nix develop -c npx vite build` を通す。失敗したまま次へ進まない。
- 触った Elm ファイルは `nix develop -c elm-format --yes <file>` で整形する。
- 既存の挙動を壊さない。特に「タップで選択」「shift クリックで複数選択」「矩形選択」「ダブルクリック」「Undo」は現状維持。
- 指定外の機能追加・リファクタはしない。分からない点は推測で拡張せず、最小の変更で止める。
- README.md の該当箇所（「できること」「ピアノロール」「セクション構成」など）に記述があれば、挙動変更に合わせて 1 行直す。
- 最後に、各 M の合格条件チェックボックスを埋めた短い報告を標準出力に書く（ファイル名と行番号付き）。

背景: 利用者は iPad（タッチ）で使っている。誤操作が多いという指摘への対応。

---

## M1. 「先頭へ戻す」ボタンを常時表示にする（ページ型レイアウト）

### 現状
- `src/Main.elm` の `seekGroup`（`SeekTo 0` の `⏮`、`SeekPrevSection` `⏪`、`SeekNextSection` `⏩`）は、`isPageLayout model` のとき `headerMenuOpen` が真の間だけ描画される（7362-7397 行あたり）。常時表示は `playStopGroup`（▶ / ■）と `positionGroup` のみ。
- デスクトップ幅では `transportGroup = [ seekGroup, playStopGroup, metronomeGroup ]` が常時表示なので問題ない。

### やること
- ページ型レイアウトのヘッダーで、`⏮`（`SeekTo 0`）ボタンを `playStopGroup` の **左隣** に常時表示する。`▶` `■` と同じ見た目・同じサイズ（タップしやすいこと）。
- `⏪` `⏩` はアコーディオン内のままでよい（`seekGroup` から `⏮` を抜いた形で残す）。
- デスクトップ幅・コンパクトヘッダーの見た目は変えない（`⏮` が二重に出ないこと）。

### 合格条件
- [ ] 幅 1200px 未満（またはタッチ判定）のレイアウトで、アコーディオンを閉じたまま `⏮` が `▶` の左に見える
- [ ] `⏮` を押すと playhead が 0 に戻る（既存 `SeekTo 0` 経路）
- [ ] デスクトップ幅で `⏮` は 1 つだけ
- [ ] elm-test / vite build 通過

---

## M2. コードトークンのドラッグ移動を廃止する

### 現状
- ブロック表示 `src/View/ChordBlocks.elm` とライン表示 `src/View/ChordLane.elm` の両方で、トークンの `pointerdown` → `PressedChordToken` → 閾値超えで `pendingChordDrag` が `chordDrag` に昇格 → `DraggedTo` / `DraggedOverChordBar` で `applyChordDragDelta`（→ `Data.ChordTrack.moveTokens`）を呼び、小節を入れ替える。
- `PressedChordToken`（`src/Main.elm` 4052 行あたり）は **選択のトグルも兼ねている**（shift で追加選択、通常クリックで単独選択）。
- `DraggedTo` / `ReleasedDrag` はノート・ドラム・トラック並び替えなど他機能と共有の汎用 Msg なので消さない。

### やること
- コードトークンの **ドラッグによる移動だけ** を取り除く。選択（タップ / shift / 矩形 `chordRubberBand`）、ダブルクリック、クリック時のプレビュー再生など他の挙動は現状維持。
- 消す対象（目安。実際の依存を読んで判断）:
  - `ChordDrag` 型、`Model.pendingChordDrag`、`Model.chordDrag`、`DraggedOverChordBar` Msg とそのハンドラ
  - `chordDragMove`、`applyChordDragDelta`、`legacyDraggedToTop` 内の `pendingChordDrag` 昇格分岐、`releasedDragMain` 内の chordDrag 分岐、`exceedsDragThreshold` の chord 用途
  - `ChordBlocks.elm` の `draggedOverBar`（セル `pointerenter`）、グリッドの `pointermove/pointerup/pointercancel/pointerleave` のうちドラッグのためだけに存在するもの
  - `ChordLane.elm` のトークン `pointermove` / `pointerup`（`tokenMoveDecoder` 等）のうちドラッグのためだけに存在するもの
  - ドラッグ中の見た目（透明度・カーソル `grab` など）
- `Data.ChordTrack.moveTokens` と `tests/ChordTrackTest.elm` の `moveTokensSuite` は **残す**（純粋ロジック。UI から呼ばれなくなるだけ）。
- 注意: `PressedChordToken` の「押した時に選択、離した時に何もしない／解除」といった細かい選択セマンティクスを変えないこと。ドラッグ用の状態を消す際に、離した時の分岐がどう変わるか必ず読んで確認する。
- 「? ヘルプ」内やREADMEにコードブロックのドラッグ移動の説明があれば削除する。

### 合格条件
- [ ] ブロック表示・ライン表示ともに、トークンを押して横に動かしても小節が入れ替わらない
- [ ] タップで選択・shift で複数選択・矩形選択・ダブルクリックは従来どおり
- [ ] `pendingChordDrag` / `chordDrag` / `DraggedOverChordBar` が残っていない（`grep` で 0 件）
- [ ] elm-test / vite build 通過

---

## M3. セクション俯瞰バーで playhead が追従して見える

### 現状
- `src/View/SectionBar.elm` の `regionRulerView`（16px のルーラー帯、`Svg.svg` 高さ `regionRulerHeight`）にだけ playhead 線がある（505-513 行あたり）。下のブロック行には無く、再生中ブロックが `box-shadow` で光るだけ。
- ルーラー・波形・ChordStrip・ブロック行は同じ `sectionBarScrollId` の `overflow-x: auto` コンテナ内で縦に並び、x 座標系は同じ（`ticksToPx` = `Data.Timeline.ticksToFractionalBar ticks timeline * pxPerBar`）。
- PianoRoll には `model.followPlayhead` が真のとき `Playhead ticks` 受信ごとに `Browser.Dom.getViewportOf pianoRollScrollId` → `GotPianoRollViewport` で可視範囲外なら `setViewportOf` する追従スクロールがある（`src/Main.elm` 3642-3649, 6078-6098 行あたり）。SectionBar にはこれが無い（`sectionBarScrollId` を触るのはホイールズーム補正だけ）。

### やること
1. **全高の playhead 線**: `SectionBar.view` で、ルーラーからブロック行までを `position: relative` の div で包み、その中に `position: absolute; top: 0; bottom: 0; width: 2px; pointer-events: none` の縦線を 1 本重ねる。`left` は既存 `rulerData.ticksToPx playheadTicks`。色はルーラーの playhead 線と同じ。ルーラー内の既存の線は残してもよいが、二重に太く見えないように調整する（同じ色・同じ x なら重なって見えるので許容）。
2. **追従スクロール**: `Playhead ticks` ハンドラで `model.followPlayhead` が真なら、PianoRoll 用と並べて `Browser.Dom.getViewportOf SectionBar.sectionBarScrollId` も発行し、新 Msg `GotSectionBarViewport ticks result` で可視範囲外なら `setViewportOf` する（`GotPianoRollViewport` と同じ判定・同じ左余白 40px）。要素が DOM に無いとき（別ページ表示中）は `Task.attempt` のエラーを黙って無視する。
3. 手動シーク時の `revealPlayheadCmd` にも同様に SectionBar 側の reveal を足す（PianoRoll と同じ条件）。

### 合格条件
- [ ] 再生中、セクションバーのブロック行を縦に貫く線が playhead 位置を動く
- [ ] ズーム（`sectionBarZoom`）を変えても線の x がルーラーの目盛りとずれない
- [ ] 曲が長くバーが横スクロールする状態で再生すると、`📌 追従` ON のとき線が画面外に出る前にバーがスクロールする。OFF のときはスクロールしない
- [ ] 追従 ON でも、SectionBar が表示されていないページ（編集タブなど）でエラーやコンソール警告が出ない
- [ ] elm-test / vite build 通過

---

## M4. セクション並び替えを「長押しで有効化」にする

### 現状
- ブロック本体の `pointerdown`（`src/View/SectionBar.elm` 694 行あたり）→ `PressedSectionBlock sectionId clientX` → 即座に `selectedSectionId` と `sectionMoveDrag` をセット。閾値も長押しも無く、押した瞬間からドラッグで入れ替わる。
- `DraggedTo` の `sectionMoveDrag` 分岐（`src/Main.elm` 1634-1658 行あたり）で累積 dx が隣接幅の半分を超えるたび `SectionBar.sectionDragTargetIndex` → `Data.Project.moveSectionToIndex`。
- `ReleasedDrag` → `releaseSectionMoveDrag`（2397-2412 行あたり）: 動かしていなければ「押す前から選択済みなら解除」のトグル。
- 既存の長押し機構: `armLongPress : LongPressTarget -> Model -> (Model, Cmd Msg)`（804-812 行）。`Process.sleep 500` → `LongPressFired token`、`model.longPress` のトークン一致で `promoteLongPress`。ポインタが動いたら disarm（`legacyDraggedToTop` 1790-1803 行あたり）。`LongPressTarget` には `LongPressSectionLoop` 等の前例がある。
- ブロックには `touch-action: pan-x` が付いていて、タッチの横スワイプはブラウザのスクロールに譲っている（689-693 行のコメント）。`user-select` / `-webkit-touch-callout` の指定は無い。

### 目標の操作
- **タップ**: 従来どおり選択（トグル）。並び替えは起きない。
- **長押し**（`sectionMoveLongPressMs = 800` を `Main.elm` に名前付き定数として置く。後で調整しやすく）: 指を動かさずに押し続けると、時間経過でそのブロックが「移動可能（armed）」状態になり **色が変わる**（背景をアクセント色に、または太い枠。`Palette` の既存色を使う）。
- armed になった後:
  - そのまま指を離さず横に動かせば並び替え（マウス／ペンではこれが自然に動く）。
  - 指を離しても armed は解除しない（タッチではブラウザのスクロールに取られて同一ジェスチャで動かせないことがあるため）。armed なブロックには `touch-action: none` を描画時に付け、**次の**ドラッグで動かせるようにする。
  - armed の解除: 並び替えを 1 回完了したとき、他の場所を `pointerdown` したとき、Escape。
- 長押し中（タイマー待ち）にポインタが閾値以上動いたら disarm し、通常のスクロール／タップとして扱う（誤操作防止の本丸）。

### 実装の指針
- `LongPressTarget` に `LongPressSectionMove { sectionId : Int, clientX : Float }` を追加し、`PressedSectionBlock` では `sectionMoveDrag` を即セットせず `armLongPress` する。`armLongPress` の 500ms 固定を、引数または別関数で `sectionMoveLongPressMs` を使えるようにする（他の呼び出し元は 500 のまま）。
- `promoteLongPress` の新分岐で `sectionMoveDrag = Just {...}` と `armedSectionId = Just sectionId`（Model に追加）をセット。
- `blockView` に armed 状態を渡し、色と `touch-action` を切り替える。ブロックに `user-select: none` と `-webkit-touch-callout: none` を付ける（長押しで iOS のコールアウト／テキスト選択が出ないように）。
- 既存の `sectionResizeDrag`（右端ハンドル）や `LongPressSectionLoop`（ルーラー）と衝突しないこと。
- タップ選択のセマンティクス（`wasSelected` によるトグル解除）は維持する。

### 合格条件
- [ ] ブロックを押してすぐ横に動かしても並び替わらない（スクロール or 何も起きない）
- [ ] 800ms 押し続けると色が変わる。動かさずに離しても色は残り、次のドラッグで並び替えできる
- [ ] 色が変わった後にそのまま横に動かすと並び替わる（マウスで確認）
- [ ] 並び替え完了 / 他所タップ / Escape で色が戻る
- [ ] 長押し中に動かすと disarm され、色は変わらない
- [ ] タップでの選択トグルは従来どおり
- [ ] `tests/SectionBarTest.elm` の既存テスト通過。`sectionDragTargetIndex` は変更しない
- [ ] elm-test / vite build 通過

---

## M5. キック・スネアが聞こえない（TR-808 のサンプル選択）

### 原因（調査済み）
- `js/audio.js` の `resolveDrumSampleForPlayer`（155-177 行あたり）は `DRUM_ROLES`（122-144 行）の候補語をグループ名（`player.sampleNames` = deprecated な group 名一覧）に部分一致させ、`player.start({ note: "kick" })` のように **グループ名** で鳴らしている。
- smplr の `DrumMachine.start` はグループ名を「そのグループで最初に現れたサンプル」に解決する（`node_modules/smplr/dist/index.mjs` 641-665, 712-729 行）。
- TR-808 の `dm.json`（https://smpldsnds.github.io/drum-machines/TR-808/dm.json、確認済み）では
  - `kick`: `bd0000, bd0010, bd0025, bd0050, bd0075, bd1000, ... bd7575`（25 種。数字は Tone/Decay ノブ）
  - `snare`: `sd0000 ... sd7575`（25 種）
  - `hihat-close`: `ch`（1 種）
  - `hihat-open`: `oh00, oh10, oh25, oh50, oh75`、`cymbal`: `cy0000...cy7575`、トム類: `*00, *10, *25, *50, *75`
  なので kick は `bd0000`（トーン 0・ディケイ 0 ＝ 最も短く低くこもった音）、snare は `sd0000` が鳴る。タブレットのスピーカーではほぼ聞こえない。ハイハットは 1 種しかないので普通に鳴る。
- `drumSampleNamesFor`（148-153 行）は `Array.isArray(player.sampleNames)` が空配列でも真になるため `getSampleNames()`（フル名一覧）に到達しない。

### やること
1. `resolveDrumSampleForPlayer` を「グループ名ではなくフルサンプル名」を返すように直す。手順:
   - `player.getGroupNames()` があればそれで役割語 → グループを決める（無ければ従来の `sampleNames` にフォールバック）。
   - 決まったグループの `player.getSampleNamesForGroup(group)`（無ければ `getSampleNames()` を `group + "/"` 前置きで絞る）からバリエーションを 1 つ選ぶ。
   - 選び方: `DRUM_PREFERRED_VARIANT` 定数（例: `{ kick: "bd5050", snare: "sd5050", cymbal: "cy5050", "hihat-open": "oh50" }`）に一致する名前があればそれ。無ければ名前でソートした **中央** の要素。バリエーションが 1 つならそれ。
   - 返す値は `start({ note })` にそのまま渡せる形（`"kick/bd5050"` のようなフル名。smplr が受け付ける形式を `index.mjs` の `start` 実装で確認すること）。
   - キャッシュ（pitch → サンプル名）は従来どおり。
2. 4 箇所ある呼び出し（通常再生 313-319、オフライン書き出し 439-444、プレビュー 719-722、断片棚 753-756 行あたり）が全部この関数を通ることを確認する。重複ロジックがあれば 1 つの helper にまとめてよい（動作は変えない）。
3. `loadInstrument` 完了時のログ `[audio] ドラムサンプル一覧:` に、解決結果（36 → `kick/bd5050` など主要 pitch の対応表）も出す。
4. 開発時の検証用に、`import.meta.env.DEV` のときだけ `window.__otogakiAudio = { players, resolveDrumSample }` を公開する（本番ビルドには入れない）。
5. 選んだバリエーションは音を聴いて最終判断する必要があるので、`DRUM_PREFERRED_VARIANT` は 1 箇所で差し替えられる形にし、コメントに「タブレットのスピーカーで聞こえるかで調整する」旨を書く。

### 合格条件
- [ ] dev サーバで再生し、コンソールに 36/38/42 の解決結果が `kick/bd5050` `snare/sd5050` `hihat-close/ch`（または同等）と出る
- [ ] `[audio] ピッチに対応するドラムサンプルがない` の警告が 8 ビートで出ない
- [ ] WAV 書き出し・プレビュー・断片棚のドラム再生経路がすべて同じ解決関数を使う
- [ ] `npx vite build` 通過。本番バンドルに `__otogakiAudio` が含まれない
- [ ] （耳での確認は利用者が行う。報告に「聴感は未確認」と明記すること）
