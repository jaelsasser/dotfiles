#!/usr/bin/env bats
# emacs.bats — exercises the eager emacs bootstrap (install.el) headlessly against a
# throwaway $HOME/XDG: chezmoi-deploy the emacs farm, then drive elpaca to completion +
# byte-compile under --batch. NOT run by ./run-tests.sh (slow, network: clones ~50
# packages); invoke via `task test:emacs`. noninteractive ⇒ a byte-compile warning or a
# failed package build is a non-zero exit.

setup() {
    REPO="$(cd "$(dirname "$BATS_TEST_FILENAME")/.." && pwd)"
    TMP="$(mktemp -d)"
}
teardown() { rm -rf "$TMP"; }

@test "emacs bootstrap installs every package and byte-compiles warning-free" {
    command -v emacs   >/dev/null 2>&1 || skip "emacs not installed"
    command -v chezmoi >/dev/null 2>&1 || skip "chezmoi not installed"

    HOME="$TMP" XDG_CONFIG_HOME="$TMP/.config" chezmoi apply \
        --source "$REPO" \
        --destination "$TMP" \
        --persistent-state "$TMP/state.boltdb" \
        --exclude=scripts,externals

    local emacs="$TMP/.config/emacs"
    run env HOME="$TMP" \
            XDG_CONFIG_HOME="$TMP/.config" \
            XDG_DATA_HOME="$TMP/.local/share" \
            XDG_CACHE_HOME="$TMP/.cache" \
        emacs --batch -q \
            -l "$emacs/early-init.el" \
            -l "$emacs/init.el" \
            -l "$emacs/install.el"

    [ "$status" -eq 0 ]
}
