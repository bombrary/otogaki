const KEY = "otogaki-project";
// 旧名称時代の保存データ。読み込みのみフォールバックする
const LEGACY_KEY = "compact-daw-project";

let timer = null;

export function saveProject(json) {
  clearTimeout(timer);
  timer = setTimeout(() => {
    try {
      localStorage.setItem(KEY, JSON.stringify(json));
    } catch (err) {
      console.error("[storage] save failed:", err);
    }
  }, 1000);
}

export function loadProject() {
  try {
    const raw = localStorage.getItem(KEY) ?? localStorage.getItem(LEGACY_KEY);
    return raw ? JSON.parse(raw) : null;
  } catch (err) {
    console.error("[storage] load failed:", err);
    return null;
  }
}
