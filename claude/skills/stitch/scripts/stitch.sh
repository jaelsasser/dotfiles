#!/usr/bin/env bash
# stitch.sh — fork a seam from a prior session into the current FRESH session.
#
# Usage: stitch.sh <source-sid>
#
# Reads source cache read-only; copies seams into $CLAUDE_SESSION_ID dir;
# advances dst cursor to the next existing seam. Source dir is never modified.
# /stitch is one of the two cursor-advancing events (the other is /compact
# via SessionStart). /baste and /tack never advance the cursor.
set -eu

SOURCE_SID="${1:-}"
DEST_SID="${CLAUDE_SESSION_ID:-}"
cache="${CLAUDE_SEAMS_DIR:-$HOME/.claude/cache/seams}"

[[ -n "$SOURCE_SID" ]] || { echo "ERROR: usage: stitch.sh <source-sid>" >&2; exit 2; }
[[ -n "$DEST_SID" ]]   || { echo "ERROR: CLAUDE_SESSION_ID unset" >&2; exit 2; }
[[ "$SOURCE_SID" != "$DEST_SID" ]] || {
  echo "ERROR: source and destination session IDs are identical — /stitch requires a fresh session" >&2
  exit 2
}
src="$cache/$SOURCE_SID"
dst="$cache/$DEST_SID"
[[ -d "$src" ]]        || { echo "ERROR: source dir not found: $src" >&2; exit 2; }
[[ -f "$src/cursor" ]] || { echo "ERROR: no cursor in $src" >&2; exit 2; }
if [[ -d "$dst" ]]; then
  existing=("$dst"/seam-*.md)
  if (( ${#existing[@]} > 0 )) && [[ -e "${existing[0]}" ]]; then
    echo "ERROR: $dst already has seams; clear it first if you want to re-take" >&2
    exit 2
  fi
fi
src_cursor=$(cat "$src/cursor")
# Compute next_cursor: smallest M > src_cursor with seam-M.md in source.
next_cursor=""
for f in "$src"/seam-*.md; do
  [[ -f "$f" ]] || continue
  n=$(basename "$f" .md); n="${n#seam-}"
  (( n > src_cursor )) || continue
  if [[ -z "$next_cursor" ]] || (( n < next_cursor )); then
    next_cursor=$n
  fi
done
[[ -n "$next_cursor" ]] || {
  echo "ERROR: source cursor $src_cursor is terminal — no further seams to advance to" >&2
  exit 2
}
mkdir -p "$dst"
# Copy (read-only on source) — multiple fresh SIDs may stitch from the same ancestor.
for f in "$src"/seam-*.md; do
  [[ -f "$f" ]] || continue
  n=$(basename "$f" .md); n="${n#seam-}"
  cp "$f" "$dst/seam-${n}.md"
done
[[ -f "$src/plan" ]] && cp "$src/plan" "$dst/plan"
echo "$next_cursor" > "$dst/cursor"
# Mark dst's active seam ## Baste consumed — /stitch is the consumption
# event for the resume bundle in the new session.
active="$dst/seam-${next_cursor}.md"
if [[ -f "$active" ]]; then
  tmp=$(mktemp)
  sed 's|^## Baste[[:space:]]*$|## Baste (consumed)|' "$active" > "$tmp" \
    && mv "$tmp" "$active" || rm -f "$tmp"
fi
printf 'stitch: %s → %s (cursor %s → %s)\n' \
  "$SOURCE_SID" "$DEST_SID" "$src_cursor" "$next_cursor"
if [[ -f "$active" ]]; then
  printf '\n--- seam-%s.md ---\n' "$next_cursor"
  cat "$active"
fi
