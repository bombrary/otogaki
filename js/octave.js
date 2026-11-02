const KEY = "otogaki-octave";

export function saveOctave(value) {
  try {
    localStorage.setItem(KEY, value);
  } catch (err) {
    console.error("[octave] save failed:", err);
  }
}

// 未保存なら null。Elm 側の fromString が未知の値を既定（c3）にフォールバックする。
export function loadOctave() {
  try {
    return localStorage.getItem(KEY);
  } catch (err) {
    console.error("[octave] load failed:", err);
    return null;
  }
}
