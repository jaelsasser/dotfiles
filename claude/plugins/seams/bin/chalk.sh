#!/usr/bin/env bash
# chalk.sh — append a fresh ## Chalk scratchpad section to the active session seam.
# Each call writes an independent section; multiple calls accumulate.
# Usage: chalk.sh [--sid <session-id>] [--seam <N>] < CONTENT
set -eu

sid=""
seam_override=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --sid)     sid="${2:-}"; shift 2 ;;
    --seam)    seam_override="${2:-}"; shift 2 ;;
    -h|--help) echo "chalk.sh [--sid <id>] [--seam <N>] < CONTENT"; exit 0 ;;
    *)         echo "ERROR: unknown arg $1" >&2; exit 2 ;;
  esac
done

[[ -n "$sid" ]] || sid="${CLAUDE_SESSION_ID:-}"
[[ -n "$sid" ]] || { echo "ERROR: --sid required (or set \$CLAUDE_SESSION_ID)" >&2; exit 2; }

CACHE_ROOT="${CLAUDE_SEAMS_DIR:-$HOME/.claude/cache/seams}"
cache="${CACHE_ROOT}/${sid}"
mkdir -p "$cache"

cursor_file="$cache/cursor"
if [[ -n "$seam_override" ]]; then
  cursor="$seam_override"
else
  cursor=$(cat "$cursor_file" 2>/dev/null || echo 0)
  [[ -f "$cursor_file" ]] || echo "$cursor" > "$cursor_file"
fi
target="$cache/seam-${cursor}.md"

printf '\n## Chalk\n' >> "$target"
cat >> "$target"

echo "$target"
