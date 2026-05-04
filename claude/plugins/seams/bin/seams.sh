#!/usr/bin/env bash
# seams.sh [<sid>...] — list sessions with pending seams and their cursor status.
# With no args: all pending sessions. With args: only matching session IDs.
set -eu

_sids_filter=("$@")
_match_sid() {
  (( ${#_sids_filter[@]} == 0 )) && return 0
  local s="$1" f
  for f in "${_sids_filter[@]}"; do [[ "$s" == "$f" ]] && return 0; done
  return 1
}

cache="${CLAUDE_SEAMS_DIR:-$HOME/.claude/cache/seams}"

[[ -d "$cache" ]] || { echo "no seams cache"; exit 0; }
found=0
for dir in $(ls -dt "$cache"/*/ 2>/dev/null); do
  [[ -d "$dir" ]] || continue
  sid=$(basename "$dir")
  _match_sid "$sid" || continue
  cursor_file="$dir/cursor"
  [[ -f "$cursor_file" ]] || continue
  cursor=$(cat "$cursor_file")
  pending=()
  for f in "$dir"/seam-*.md; do
    [[ -f "$f" ]] || continue
    n=$(basename "$f" .md); n="${n#seam-}"
    (( n >= cursor )) && pending+=("seam-${n}.md")
  done
  (( ${#pending[@]} > 0 )) || continue
  mtime=$(date -r "$dir" '+%Y-%m-%d %H:%M:%S' 2>/dev/null \
          || stat --printf '%y\n' "$dir" 2>/dev/null | cut -d. -f1 \
          || echo "unknown")
  first_line=$(head -1 "$dir/seam-${cursor}.md" 2>/dev/null || echo "(empty)")
  plan_path=""
  [[ -f "$dir/plan" ]] && plan_path=$(cat "$dir/plan")
  printf 'sid: %s\n  cursor: %s\n  count: %d\n  mtime: %s\n  pending: %s\n  head: %s\n  plan: %s\n\n' \
    "$sid" "$cursor" "${#pending[@]}" "$mtime" "${pending[*]}" "$first_line" "$plan_path"
  found=1
done
(( found )) || echo "no prior seams found"
