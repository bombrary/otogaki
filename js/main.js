import { Elm } from "../src/Main.elm";
import { ensureAudio, handleCommand, loadRefAudio, setElmSender } from "./audio.js";
import { loadProject, saveProject } from "./storage.js";

const app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: loadProject(),
});

setElmSender((msg) => app.ports.fromAudio.send(msg));

app.ports.toAudio.subscribe(async (msg) => {
  await ensureAudio();
  handleCommand(msg);
});

app.ports.saveToLocalStorage.subscribe(saveProject);

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
  if ((hotkeys.includes(e.key) || isSelectAll) && !["INPUT", "TEXTAREA", "SELECT"].includes(tag)) {
    e.preventDefault();
  }
});
