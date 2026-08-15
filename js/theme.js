const KEY = "otogaki-theme";

export function saveTheme(theme) {
  try {
    localStorage.setItem(KEY, theme);
  } catch (err) {
    console.error("[theme] save failed:", err);
  }
}

export function loadTheme() {
  try {
    return localStorage.getItem(KEY) ?? "system";
  } catch (err) {
    console.error("[theme] load failed:", err);
    return "system";
  }
}
