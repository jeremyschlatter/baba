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
        "start-baba" = ''
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
          port="''${BABA_PORT:-8080}"
          'Baba Is Clone.app'/baba_is_clone level levels/ --llm-api --llm-port "$port" &
          pid=$!
          # Wait for server to be ready
          for i in $(seq 1 30); do
            if curl -s "http://127.0.0.1:$port/state" >/dev/null 2>&1; then
              echo "Baba Is You started on port $port (pid $pid)"
              exit 0
            fi
            sleep 0.1
          done
          echo "Failed to start server" >&2
          exit 1
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
