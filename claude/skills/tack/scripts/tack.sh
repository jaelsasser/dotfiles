#!/usr/bin/env bash
# tack.sh — append a fresh ## Tack scratchpad section to the active session seam.
# Each call writes an independent section; multiple calls accumulate.
# Usage: tack.sh [--sid <session-id>] < CONTENT
set -eu

sid=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --sid)     sid="${2:-}"; shift 2 ;;
    -h|--help) echo "tack.sh [--sid <id>] < CONTENT"; exit 0 ;;
    *)         echo "ERROR: unknown arg $1" >&2; exit 2 ;;
  esac
done

[[ -n "$sid" ]] || sid="${CLAUDE_SESSION_ID:-}"
[[ -n "$sid" ]] || { echo "ERROR: --sid required (or set \$CLAUDE_SESSION_ID)" >&2; exit 2; }

CACHE_ROOT="${CLAUDE_SEAMS_DIR:-$HOME/.claude/cache/seams}"
cache="${CACHE_ROOT}/${sid}"
mkdir -p "$cache"

cursor_file="$cache/cursor"
cursor=$(cat "$cursor_file" 2>/dev/null || echo 1)
[[ -f "$cursor_file" ]] || echo "$cursor" > "$cursor_file"
target="$cache/seam-${cursor}.md"

printf '\n## Tack\n' >> "$target"
cat >> "$target"

echo "$target"
