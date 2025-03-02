{
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    with nixpkgs.legacyPackages.${system};
    let
      scripts = {
        "build" = "cargo build";
        "check" = "cargo test";
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
        "baba" = "cargo run --bin baba $@";
      };
    in {
      devShell = stdenvNoCC.mkDerivation {
        name = "shell";
        buildInputs = lib.attrsets.mapAttrsToList writeShellScriptBin scripts ++ [
          cargo
        ];
      };
    });
}
