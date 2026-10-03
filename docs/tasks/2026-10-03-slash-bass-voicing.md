# 2026-10-03 スラッシュコードのベース音を辞書で動かせるようにする

作業者: Sonnet（headless）。計画: Fable。以下の「共通ルール」と、その後ろの計画本文に従って実装する。

## 共通ルール

- 作業ディレクトリは `~/repos/otogaki`。ツールは `nix develop -c <cmd>` で呼ぶ（`elm-test`, `npx vite build`, `elm-format`）。
- **最初にテストを書く。** 計画本文の「追加するテスト」を先に書き、修正前の状態で `nix develop -c elm-test` を回して、どのテストが落ちるかを記録する（`withBassLowest` のように関数がまだ無いものは、関数の雛形を置いてから落とす）。落ち方が計画の見立てと食い違う場合（たとえば「辞書の最低音がベースなのに C2 が重なる」テストが修正前から通るなど）は、その事実を最後の報告に必ず書く。
- 1 コミット。メッセージは `fix(voicing): 日本語で何を直したか`、本文末尾に `Co-Authored-By: Claude Sonnet <noreply@anthropic.com>`。このブリーフ自体（docs/tasks 配下）はコミットに含めない。
- コミット前に `nix develop -c elm-test` と `nix develop -c npx vite build` を通す。失敗したままコミットしない。触った Elm ファイルは `nix develop -c elm-format --yes <file>`。
- 計画に無い機能追加・リファクタはしない。「やらないこと」に書いたものは作らない。分からない点は推測で広げず、最小の変更で止めて報告する。
- 計画本文の行番号は目安。必ず実際のコードを読んで確かめる。
- 計画本文の「1. 編集画面をコードのルートで開く」では、トークンからボイシング編集に入る導線を自分で列挙し、報告に一覧（ファイル名:行番号）を載せる。
- 最後に標準出力へ短い報告を書く: 修正前に落ちたテスト、変更箇所（ファイル名:行番号）、テスト件数、コミットハッシュ、ブラウザ未検証である旨。

---

# otogaki: スラッシュコードのベース音を辞書で動かせるようにする

## Context

リクの報告: 「`G#/C` のルート音が C2 で固定になる。C3 にしようとボイシング辞書をいじっても適用されない」。操作は「C3 を一番下にした」。

調査で分かった原因は 2 つが重なったもの。

**原因 1（主因）: 編集画面の鍵盤が、編集中のコードの高さで表示されていない。**
`model.voicingPreviewRoot`（試聴キー）は初期値 0（= C）で、代入はドロップダウンを手で変えたとき（`src/Main.elm:5273`）だけ。トークンから編集画面を開いてもコードのルートに切り替わらない。辞書の中身はルートからの相対値（offset）なので、`G#/C` のボイシングを開くと「C のコードなら」の高さで見える。そこで一番下を C3 にすると offset +12 が保存され、実際の `G#/C` では G#3 になる。

**原因 2: スラッシュのベースが辞書の外で、見えない形で足されている。**
`withSlashBass`（`src/Data/StrumExpand.elm:94`）は、辞書の最低音がベースと同じ音名でなければ `anchorPitch(36) + 音名`（`/C` なら C2）を先頭に足す。この音は編集画面に出ない。原因 1 で最低音が G#3 になっているので、C2 が足され続ける。

付随して見つかった不整合:
- `Data.Chord.toPitchesWith` の辞書ありの枝（`src/Data/Chord.elm:181-183`）は最低音を見ずに無条件で C2 を足す。`bestForm` が失敗したとき（7 音以上など）と `ghostNoteGroups`（`Main.elm:738`）でこの枝を通るので、辞書に C を最低音として書いていても C2 が重なる。
- 自動登録（`registerShapeForToken` `Main.elm:3373`、`autoRegisterAndOpenEditTab` `Main.elm:3397`）は `VoicingPreset.offsetsFor quality shape` だけで作り、ベースを無視する。`G#/C` でも最低音は G# になる。
- 鍵盤の空き行クリックは `offset < 0` を弾く（`Main.elm:5034`）。型とドラッグ側（`shiftPicks`、`GuitarForm.elm`）は下限 -12 を許しているので、このガードだけ古い。ルートより低いベースを置けない。

リクが選んだ方針: **ベースは辞書に入れて普通の音として扱う。** 既存の辞書は今の規則のまま鳴らす（音を変えない）。

## 変更内容

### 1. 編集画面をコードのルートで開く（主因の修正）
- トークンからボイシング編集に入る全経路で `voicingPreviewRoot = modBy 12 chord.root` をセットする。対象は `autoRegisterAndOpenEditTab`、`registerShapeForToken`、`registerCandidateForToken`（`Main.elm:3347`）と、トークンから編集タブを開く他の導線（実装時に列挙する）。
- 辞書タブを直接開いた場合は従来通り（最後に選んだ試聴キーのまま）。

### 2. ベース規則を 1 か所にまとめる
- `withSlashBass` を `Data.Chord` に移して公開し、`StrumExpand.soundingPitches` と `Data.Chord.toPitchesWith` の辞書ありの枝の両方から使う。
- 規則は現行の `withSlashBass` のまま: 最低音がベースと同じ音名ならそのまま、違えば従来の位置に足す。既存の辞書の鳴り方は変わらない。変わるのは「辞書の最低音がベースなのに C2 が重なる」ケースだけ。
- 辞書なしのフォールバック（`Chord.elm:188-203`）は触らない。

### 3. 自動登録でベースを最低音にする
- 純粋関数 `withBassLowest : Maybe Int -> List Int -> List Int` を `Data.VoicingPreset` に追加（引数は `GuitarForm.bassInterval chord` の結果と offsets）。
  - ベースなし → そのまま。
  - offsets にベースと同じ音名がある → その最も低いものを軸に、それより低い音を 1 オクターブずつ上げる。例: `G#/C` closed `[0,4,7]` → `[4,7,12]`（C3・D#3・G#3）。
  - 無い（F/G などのハイブリッド）→ `interval - 12` を先頭に足す（範囲 -11〜-1。今の自動ベースと同じ高さになる）。
- `registerShapeForToken` と `autoRegisterAndOpenEditTab` の `offsets` にこれを掛ける。`registerCandidateForToken` はギターフォーム由来でベースが既に最低音なので触らない。

### 4. ルートより低い音を置けるようにする
- `PressedVoicingOffset` のガード（`Main.elm:5034`）を `offset < -12` に緩め、コメントを直す。同じ前提のガードが他の Msg（`Main.elm:5151, 5174, 5231` 付近）にあれば揃える。

### 5. ヘルプ
- `src/Data/Help.elm` の「ボイシングの編集画面」に 2 行: 鍵盤は編集中のコードの高さで表示されること、スラッシュコードは一番下の音がベースになること（一番下がベースの音名でない古い辞書には自動で足される）。

### やらないこと
- 古い辞書の「自動で足されるベース」を編集画面に表示する機能。入れると編集画面がトークンのベースを知る必要があり範囲が広がる。今回は、開き直して一番下に C を置けば消える、で足りる。

## 進め方
- 計画・レビュー・ブラウザ検証は Fable、実装は Sonnet（headless、otogaki の cwd）。ブリーフを `docs/tasks/2026-10-03-slash-bass-voicing.md` に書く。
- Sonnet には**先に失敗するテストを書かせる**。修正前に落ちるテストを報告させ、見立てと違えば止めて報告させる。

## 変更ファイル
- `src/Main.elm`（試聴キーのセット、自動登録、offset ガード）
- `src/Data/Chord.elm`（`withSlashBass` の移設と `toPitchesWith`）
- `src/Data/StrumExpand.elm`（移設後の関数を使う）
- `src/Data/VoicingPreset.elm`（`withBassLowest`）
- `src/Data/Help.elm`
- `tests/VoicingTest.elm`、`tests/StrumExpandTest.elm`、`tests/VoicingPresetTest.elm`

## 検証
```bash
cd ~/repos/otogaki && nix develop -c bash -c "elm-test && npx vite build"
```
追加するテスト:
- 辞書の最低音がベースと同じ音名のスラッシュコードで、`soundingPitches` と `toPitchesWith` のどちらも余分なベースを足さない（`bestForm` が成功する場合と、7 音で失敗する場合の両方）。
- 最低音がベースでない既存形は従来通り C2 帯に足される（`tests/VoicingTest.elm:91-102` の既存テストが通り続ける）。
- `withBassLowest` の 3 ケース（ベースなし／転回形／ハイブリッド）。

ブラウザ（Chrome、dev サーバー 5173）:
1. `G#/C` を新規に書いてボイシングを登録 → 編集画面の試聴キーが G# になり、一番下が C3 で表示される。
2. ピアノロールのプレビューに C2 が出ない（C3・D#3・G#3）。
3. 一番下の C を 1 オクターブ下げる → プレビューのベースも C2 に動く。上げ直すと戻る。
4. スラッシュなしのコードと、古い形の辞書（最低音が G#）の鳴り方が変わらない。
