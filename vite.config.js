import { defineConfig } from "vite";
import { plugin as elm } from "vite-plugin-elm";

export default defineConfig({
  // debug: false — Elm のタイムトラベルデバッガは大きな配列（波形ピーク等）を
  // メッセージごとに非末尾再帰で走査してスタックオーバーフローする
  plugins: [elm({ debug: false })],
});
