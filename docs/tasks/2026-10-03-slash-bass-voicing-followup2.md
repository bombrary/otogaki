# 2026-10-03 スラッシュベース修正の追加入れ 2（コミット 06d1aca への続き）

作業者: Sonnet（headless）。計画: Fable。前提: コミット 7f5f363 と 06d1aca が入っている。

## 共通ルール

- 作業ディレクトリは `~/repos/otogaki`。ツールは `nix develop -c <cmd>`（`elm-test`, `npx vite build`, `elm-format`）。
- **コマンドは必ずフォアグラウンドで実行する（run_in_background を使わない）。** このセッションは headless で、ターンが終わると同時に終了する。elm-test と vite build は Bash の timeout を 600000 にして同期的に待つ。最後の報告を書くまでターンを終えない。
- 1 コミット。メッセージは `fix(voicing): 日本語で何を直したか`、本文末尾に `Co-Authored-By: Claude Sonnet <noreply@anthropic.com>`。docs/tasks 配下のブリーフはコミットに含めない。
- コミット前に `nix develop -c elm-test` と `nix develop -c npx vite build` を通す。触った Elm ファイルは `nix develop -c elm-format --yes <file>`。
- 指定外の変更はしない。最後に標準出力へ短い報告（変更箇所のファイル名:行番号、テスト件数、コミットハッシュ、ブラウザ未検証である旨）。

## ブラウザで確認できたこと（変えない）

`G#/C` のトークンから「候補から選ぶ → クローズド」で登録すると、offsets は `[4, 12, 16, 19]`、試聴キーは G#、プレビューのベースは C3 になる。ここは期待通り。

## 直すこと（どちらも「フォームピッカーがスラッシュコードのトークンを開いているのに、ベースを考慮しない」食い違い）

### 1. 候補タブの「ボイシング型」ボタンのラベル

- 現状: `G#/C` を開くと、クローズドのボタンに「G# G# C D#」と出る。これは素のプリセット（`Data.VoicingPreset.offsetsFor`）の音名。実際に登録されるのは `withBassLowest` を掛けた `[4, 12, 16, 19]`（C G# C D#）なので、ラベルと結果が違う。
- やること: ラベルを作っている箇所（`src/View/FormPicker.elm` か、そこへ渡す Main.elm 側）を特定し、登録時と同じ `Data.VoicingPreset.withBassLowest (Data.GuitarForm.bassInterval chord)` を掛けた offsets から音名を出す。`G#/C` のクローズドは「C G# C D#」になる。スラッシュなしのコードのラベルは変わらないこと。
- クリック時に試聴音が鳴る実装なら、その音も同じ offsets から作る。

### 2. 「手で編集」タブの「プリセットを適用」

- 現状: `src/Main.elm` の該当ハンドラ（`offsets = Data.VoicingPreset.offsetsFor quality shape, stringPicks = Set.empty` と書いている箇所。5290 行付近）は素のプリセットで上書きする。`G#/C` のトークンから開いた編集画面でこれを押すと最低音が G#2 に戻り、再生時に C2 が自動で足される状態に戻ってしまう。
- やること: フォームピッカーがトークンを開いている状態（`model.formPicker` などからそのトークンの `Chord` が取れる状態）で「プリセットを適用」が押されたら、`withBassLowest (bassInterval chord)` を掛けた offsets で上書きする。
  - プリセット選択のドロップダウンで選んだ quality がトークンの quality と違っていてもよい（ベースの度数だけトークンから取る）。
  - トークンの文脈が無い（辞書を直接編集している）ときは従来通り素のプリセット。
  - トークンの `Chord` を取り出す既存のヘルパー（`registerShapeForToken` や `openEditTab` 周辺が使っているもの）があれば再利用する。無ければ最小限で足す。
- 文脈からコードが取れない構造だった場合は、無理に作り込まず、何が取れて何が取れないかを報告して止める。

## 合格条件

- [ ] `G#/C` の候補タブで、クローズドのラベルが登録結果（C G# C D#）と一致する
- [ ] スラッシュなしのコードのラベルは変わらない
- [ ] `G#/C` のトークンから開いた編集画面の「プリセットを適用」で、最低音がベースの音名になる
- [ ] 辞書を直接編集しているときの「プリセットを適用」は従来通り
- [ ] elm-test / vite build 通過、elm-format 済み、1 コミット
