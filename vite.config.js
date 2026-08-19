import { defineConfig } from "vite";
import { plugin as elm } from "vite-plugin-elm";

export default defineConfig({
  // GitHub Pages の project page は https://<user>.github.io/otogaki/ 以下に配置されるので、
  // ルート相対パスだとアセットが解決できない。base でサブパスを明示する。
  base: "/otogaki/",
  // debug: false — Elm のタイムトラベルデバッガは大きな配列（波形ピーク等）を
  // メッセージごとに非末尾再帰で走査してスタックオーバーフローする
  plugins: [elm({ debug: false })],
});
