{
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    with nixpkgs.legacyPackages.${system};
    let
      scripts = {
        "check" = "cargo test";
        "baba" = ''
          set -eu
          action="''${1:-state}"
          port="''${BABA_PORT:-8080}"
          if [ "$action" = "state" ]; then
            curl -s "http://127.0.0.1:$port/state"
          else
            curl -s -X POST "http://127.0.0.1:$port/move" -d "$action"
          fi
        '';
        "run" = ''
          set -eu
          cargo build
          cd $PROJECT_ROOT
          ln -sf target/debug 'Baba Is Clone.app'
          cat << EOF > target/debug/info.plist
          <plist version="1.0">
            <dict>
              <key>CFBundleExecutable</key>
              <string>baba_is_clone</string>
            </dict>
          </plist>
          EOF
          'Baba Is Clone.app'/baba_is_clone $@
          rm target/debug/info.plist
          rm 'Baba Is Clone.app'
        '';
      };
    in {
      devShell = stdenvNoCC.mkDerivation {
        name = "shell";
        buildInputs = lib.attrsets.mapAttrsToList writeShellScriptBin scripts ++ [
          cargo
          curl
          rustfmt
        ];
      };
    });
}
