#!/usr/bin/env bash
# run.sh — run a test command, save output, diff vs. previous run.
#
# Usage: run.sh <test-cmd> [args...]
# Always exits 0. The real test exit code is on the `exit:` line.
set -eu

[[ $# -gt 0 ]] || { echo "ERROR: test command required" >&2; exit 2; }

toplevel=$(git rev-parse --show-toplevel 2>/dev/null) || toplevel="$PWD"
key=$(printf '%s' "$toplevel" | shasum | head -c8)
cache="$HOME/.claude/cache/tests/$key"
mkdir -p "$cache"

# Rotate previous run
[[ -f "$cache/output" ]] && mv "$cache/output" "$cache/output.prev"
[[ -f "$cache/exit"   ]] && mv "$cache/exit"   "$cache/exit.prev"

# Run
exit_code=0
( cd "$toplevel" && "$@" ) > "$cache/output" 2>&1 || exit_code=$?
printf '%s\n' "$exit_code" > "$cache/exit"

# Emit current output
cat "$cache/output"

# Meta block
printf '\n--- meta ---\nexit: %s\ncmd: %s\n' "$exit_code" "$*"

# Diff vs. previous run (omitted on first run)
if [[ -f "$cache/output.prev" ]]; then
  printf '\n--- diff vs. previous run ---\n'
  diff "$cache/output.prev" "$cache/output" || true
fi
