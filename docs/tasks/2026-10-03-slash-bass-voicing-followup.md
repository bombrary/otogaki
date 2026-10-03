# 2026-10-03 スラッシュベース修正の追加入れ（コミット 7f5f363 への続き）

作業者: Sonnet（headless）。計画: Fable。前提: コミット 7f5f363 が入っている。ブラウザで確かめたら 2 点が期待と違った。

## 共通ルール

- 作業ディレクトリは `~/repos/otogaki`。ツールは `nix develop -c <cmd>`（`elm-test`, `npx vite build`, `elm-format`）。
- **コマンドは必ずフォアグラウンドで実行する（run_in_background を使わない）。** このセッションは headless で、ターンが終わると同時に終了する。elm-test と vite build は Bash の timeout を 600000 にして同期的に待つ。最後の報告を書くまでターンを終えない。
- 1 コミット。メッセージは `fix(voicing): 日本語で何を直したか`、本文末尾に `Co-Authored-By: Claude Sonnet <noreply@anthropic.com>`。docs/tasks 配下のブリーフはコミットに含めない。
- コミット前に `nix develop -c elm-test` と `nix develop -c npx vite build` を通す。触った Elm ファイルは `nix develop -c elm-format --yes <file>`。
- 指定外の変更はしない。最後に標準出力へ短い報告（変更箇所のファイル名:行番号、テスト件数、コミットハッシュ、ブラウザ未検証である旨）。

## 1. `withBassLowest` の規則を直す（計画側の前提が誤っていた）

### 事実
`Data.VoicingPreset.offsetsFor` が返すプリセットは「offset 0 の低いルート（ベース位置）＋ 12 以上の上の和音」という形（Closed は `0 :: List.map ((+) 12) intervals`。Maj なら `[0, 12, 16, 19]`）。計画は `[0, 4, 7]` を想定していたので、今の `withBassLowest` は `G#/C` のクローズドを `[16, 19, 24, 24]` にしてしまう（1 オクターブ高く、重複もある）。

### 正しい規則
スラッシュコードでは、**ベース位置の低いルート（offset 0）をスラッシュのベースに差し替え、上の和音はそのまま残す。**

`withBassLowest : Maybe Int -> List Int -> List Int`（引数は `GuitarForm.bassInterval chord` の結果 = ルートからの半音 1〜11、と offsets）:

- `Nothing` → offsets をそのまま返す。
- `Just interval` →
  1. `others` = offsets から offset 0 を除いたもの（0 が無ければ offsets のまま）。
  2. `others` が空 → `[ interval ]`。
  3. `others` の最低音の音名（`modBy 12`）が `interval` と同じ → `others` をそのまま（ベースは既に最低音にある）。
  4. `others` の最低音 > `interval` → `interval :: others`。
  5. それ以外 → `(interval - 12) :: others`（ルートより下に置く。範囲 -11〜-1）。
  6. 結果は昇順に並べ、重複を除く。

### 期待値（テストにする。前回書いた `withBassLowest` のテストは誤った前提なので書き換える）
- `Nothing`, `[0,12,16,19]` → `[0,12,16,19]`
- `Just 4`, `[0,12,16,19]`（G#/C クローズド）→ `[4,12,16,19]`
- `Just 4`, `[0,4,12,19]`（最低音が既にベースの音名）→ `[4,12,19]`
- `Just 2`, `[0,12,16,19]`（F/G のようなハイブリッド）→ `[2,12,16,19]`
- `Just 7`, `[0,12,16,19]` → `[7,12,16,19]`
- `Just 5`, `[0,3,7]`（ルートより下に置くケース）→ `[-7,3,7]`
- 可能なら `offsetsFor` の実際の出力（Maj の Closed / Drop2 / Wide）に `Just 4` を掛けて、結果の最低音の音名が 4 で、重複が無いことも確かめる。

`Main.elm` の呼び出し側（`registerShapeForToken`、`autoRegisterAndOpenEditTab`）は変えなくてよい。

## 2. ボイシング鍵盤の度数ラベルを本当のルート基準にする

### 事実
`src/View/ChordEditor.elm:333` で `displayRootPitch = Data.Voicing.displayRoot rootPitch voicing.offsets`（= 実際に置かれている最低音）を `VoicingKeyboard.view` に渡している。鍵盤の行に出る度数ラベル（R, b3, #5 など）がこの最低音基準で計算されているらしく、`G#/C` で C を最低音にすると C の行が「R」と表示される。指板（`Fretboard.view`）とホバー表示（`VoicingKeyboard.elm:281` の `modBy 12 (pitch - rootPitch)`）は本当のルート（`rootPitch`）基準なので、鍵盤のラベルだけが食い違っている。

### やること
- `src/View/VoicingKeyboard.elm` を読み、行に出る度数ラベルの計算基準を特定する。`displayRootPitch` 基準なら `rootPitch`（本当のルート）基準に変える。これで `G#/C` の C は「3」、G# は「R」と出る。
- `displayRootPitch` の他の用途（root 行の背景色 `isRoot`、編集を開いたときのスクロール先）は**変えない**。
- ラベルが最初から `rootPitch` 基準だった場合は何も変えず、表示が「R」になった理由を調べて報告する。
- `displayRootPitch` 基準にした理由がコメントやテストに明記されていたら、変更せずに引用して報告する。
