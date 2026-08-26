import { Elm } from "../src/Main.elm";
import { clearRefBuffer, debugRawContext, ensureAudio, handleCommand, loadRefAudio, restoreRefAudioFromCache, setElmSender } from "./audio.js";
import * as audioCache from "./audioCache.js";
import { loadProject, saveProject } from "./storage.js";
import { loadTheme, saveTheme } from "./theme.js";

// Elm 起動前に同期的に data-theme を付与し、テーマのフラッシュ（一瞬ライトで描画されてからダークに切り替わる）を避ける。
const initialTheme = loadTheme();
document.documentElement.setAttribute("data-theme", initialTheme);

const app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: {
    ...(loadProject() ?? {}),
    theme: initialTheme,
    // 幅だけでは iPad Pro 横向きなど幅の広いタッチ端末を拾えないので、pointer:coarse も併用する。
    pointerCoarse: window.matchMedia("(pointer: coarse)").matches,
  },
});

setElmSender((msg) => app.ports.fromAudio.send(msg));

// 参考オーディオの自動復元。IndexedDB に前回のファイルが残っていれば、リロード後も選び直さずに波形が出る。
audioCache
  .loadRefAudio()
  .then((cached) => {
    if (cached) return restoreRefAudioFromCache(cached.blob, cached.name);
  })
  .catch((err) => console.error("[audio] 参考オーディオの自動復元に失敗:", err));

app.ports.toAudio.subscribe(async (msg) => {
  const ready = await ensureAudio();
  if (!ready) return;
  handleCommand(msg);
});

// タブが前面に戻ったタイミングで AudioContext の復帰を試みる（iOS はバックグラウンドで中断する）。
// user gesture 外なので失敗しうるが、成功すれば次の再生操作を待たずに直る。
document.addEventListener("visibilitychange", () => {
  if (document.visibilityState === "visible") {
    ensureAudio().catch(() => {});
  }
});

// 開発時のみ: DevTools から __otogaki.audioContext().suspend() を叩いて、iOS での
// AudioContext 中断をデスクトップで決定論的に再現する。
if (import.meta.env.DEV) {
  window.__otogaki = { audioContext: debugRawContext };
}

app.ports.saveToLocalStorage.subscribe(saveProject);

app.ports.clearRefAudio.subscribe(() => {
  clearRefBuffer();
  audioCache.clearRefAudio().catch((err) =>
    console.error("[audio] 参考オーディオキャッシュの削除に失敗:", err)
  );
  // 同じファイルを選び直せるように input の値を空にする（値が残ったままだと change イベントが発火しない）。
  const el = document.getElementById("ref-audio-input");
  if (el) el.value = "";
});

app.ports.setTheme.subscribe((theme) => {
  document.documentElement.setAttribute("data-theme", theme);
  saveTheme(theme);
});

app.ports.copyToClipboard.subscribe((text) => {
  navigator.clipboard.writeText(text).catch((err) =>
    console.error("[clipboard] コピーに失敗:", err)
  );
});

// 参考オーディオのファイル選択（Elm が描画する input を JS で拾う）
document.addEventListener("change", (e) => {
  const t = e.target;
  if (t && t.id === "ref-audio-input" && t.files && t.files[0]) {
    loadRefAudio(t.files[0]).catch((err) =>
      console.error("[audio] 参考オーディオの読込に失敗:", err)
    );
  }
});

// select で選んだ後はフォーカスを外す（直後の Space が再生/停止に届くように）
document.addEventListener("change", (e) => {
  if (e.target && e.target.tagName === "SELECT") e.target.blur();
});

// Space / ↑↓ でページがスクロールするのを止める（ショートカット用）
window.addEventListener("keydown", (e) => {
  const tag = e.target && e.target.tagName;
  const hotkeys = [" ", "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight", "Home", "End"];
  const isSelectAll = (e.ctrlKey || e.metaKey) && (e.key === "a" || e.key === "A");
  if ((hotkeys.includes(e.key) || isSelectAll) && !(["INPUT", "TEXTAREA", "SELECT"].includes(tag))) {
    e.preventDefault();
  }
});

// マウスには Pointer Events の暗黙キャプチャがないため、明示キャプチャで
// ノート要素外に出てもpointermove/pointerupが届くようにする。
// data-pointer-capture を持つ要素に限定（オーバーレイ方式の他ドラッグと干渉させない）。
document.addEventListener("pointerdown", (e) => {
  const el = e.target.closest?.("[data-pointer-capture]");
  if (el && e.button === 0) {
    try { el.setPointerCapture(e.pointerId); } catch (_) {}
  }
}, { capture: true });

// タッチはpointerdownした要素に暗黙キャプチャされるため、そのままだと下の要素へpointerenter/pointermoveが
// 届かない。data-pointer-release-captureを持つ要素（ブロック表示のコードドラッグ用）は明示的に解除し、
// セルのpointerenterでドロップ先小節を特定できるようにする。
document.addEventListener("pointerdown", (e) => {
  const el = e.target.closest?.("[data-pointer-release-capture]");
  if (el) {
    try { el.releasePointerCapture(e.pointerId); } catch (_) {}
  }
}, { capture: true });

// ピアノロールのグリッドは、空白スワイプをネイティブスクロールに委譲するため touch-action:
// pan-x pan-y を持つ。しかし長押しで矩形選択・ループ範囲ドラッグに昇格した後は、それを止めたい。
// touch-action は pointerdown 時点の値で固定されてしまうため途中変更では効かない。代わりに、
// 非パッシブな touchmove で、ブラウザがスクロールを開始する「前」に1回 preventDefault すれば、
// そのジェスチャの間ずっと抑止される。data-suppress-touch-scroll を持つ要素（グリッドの svg）に
// 限定し、closest() で内側のノート rect からも拾えるようにする。
document.addEventListener("touchmove", (e) => {
  if (!e.cancelable) return; // 既にスクロール中。呼んでも無効（コンソール警告が出るだけ）
  if (e.target.closest?.("[data-suppress-touch-scroll]")) e.preventDefault();
}, { passive: false, capture: true });
