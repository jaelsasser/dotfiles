#!/usr/bin/env bash
# run-tests.sh — run the full bats test suite from the repo root.
# Usage: ./run-tests.sh [bats-options] [file|dir ...]
#
# With no arguments, runs all .bats files recursively (stow.bats + claude/tests/).
# Pass a path to restrict to a directory or file:
#   ./run-tests.sh claude/tests/unit/seam.bats
#   ./run-tests.sh --filter "tack" claude/tests/unit/
set -eu
REPO="$(cd "$(dirname "$0")" && pwd)"
cd "$REPO"
exec bats -r "${@:-.}"
