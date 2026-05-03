#!/usr/bin/env bash
# Shared helpers for claude/tests/.
#
# Usage in a .bats file:
#   load '../lib/helpers'   (unit/ or integration/)

# Absolute path to the dotfiles repo root, derived from the test file's path.
# Works for both unit/ (3 levels up) and integration/ (3 levels up).
REPO="$(cd "$(dirname "$BATS_TEST_FILENAME")/../../.." && pwd)"

# Isolate seam fixtures to BATS_TEST_TMPDIR so nothing leaks into $HOME.
# Call at the top of each test that touches seam files.
setup_seams() {
    export CLAUDE_SEAMS_DIR="$BATS_TEST_TMPDIR/seams"
    mkdir -p "$CLAUDE_SEAMS_DIR"
}

# Create a throwaway git project in BATS_TEST_TMPDIR/proj.
# Sets: PROJ, STYLE, DOCOMMIT, and PROJ_CACHE (for teardown).
setup_commit_proj() {
    PROJ="$BATS_TEST_TMPDIR/proj"
    rm -rf "$PROJ"
    mkdir -p "$PROJ"
    cd "$PROJ"
    git init -q
    git -c user.email=t@t -c user.name=t commit --allow-empty -q -m init
    STYLE="$REPO/claude/skills/commit/scripts/commit-style.sh"
    DOCOMMIT="$REPO/claude/skills/commit/scripts/do-commit.sh"
}
