{
  description = "otogaki（音書き）: 音楽ドラフトのためのコンパクトDAW";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        elmTools = with pkgs.elmPackages; [ elm elm-format elm-test elm-language-server ];
        dev = pkgs.writeShellApplication {
          name = "otogaki-dev";
          runtimeInputs = [ pkgs.nodejs_22 ] ++ elmTools;
          text = ''
            if [ ! -d node_modules ]; then npm install; fi
            npx vite dev --open
          '';
        };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [ pkgs.nodejs_22 ] ++ elmTools;
        };
        apps.default = {
          type = "app";
          program = "${dev}/bin/otogaki-dev";
        };
      });
}
