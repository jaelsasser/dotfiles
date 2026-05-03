#!/usr/bin/env bats
#
# Regression tests for stow.sh and per-package link.sh wrappers.
#
# Each test runs against a fresh $HOME under $BATS_TEST_TMPDIR, so the
# real ~/.config and ~/.claude are never touched. Tests pick lightweight
# packages (bin, git) for end-to-end stow.sh runs, and call link.sh
# directly to exercise the wrapper without triggering its package's
# configure.sh side effects.
#
# Run: bats stow.bats   (install bats-core: `brew install bats-core`)

setup() {
    REPO="$(cd "$(dirname "$BATS_TEST_FILENAME")" && pwd)"
    export HOME="$BATS_TEST_TMPDIR/home"
    export XDG_CONFIG_HOME="$HOME/.config"
    export XDG_DATA_HOME="$HOME/.local/share"
    export XDG_CACHE_HOME="$HOME/.cache"
    mkdir -p "$XDG_CONFIG_HOME" "$XDG_DATA_HOME" "$XDG_CACHE_HOME"
}

@test "stow.sh runs from any CWD" {
    cd /tmp
    "$REPO/stow.sh" -R bin
    [ -L "$XDG_CONFIG_HOME/bin/ediff.sh" ]
}

@test "stow.sh processes every positional target" {
    "$REPO/stow.sh" -R bin git
    [ -L "$XDG_CONFIG_HOME/bin/ediff.sh" ]
    [ -L "$XDG_CONFIG_HOME/git/config" ]
}

@test "stow.sh forwards STOW_ACTION to link.sh" {
    run bash -c "bash -x '$REPO/stow.sh' -D claude 2>&1"
    [[ "$output" == *"claude/link.sh -D"* ]]
}

@test "claude/link.sh forwards \$1 to stow" {
    run bash -c "bash -x '$REPO/claude/link.sh' -D 2>&1"
    [[ "$output" == *"-D claude"* ]]
}

@test "claude/link.sh defaults to -R when called bare" {
    run bash -c "bash -x '$REPO/claude/link.sh' 2>&1"
    [[ "$output" == *"-R claude"* ]]
}
