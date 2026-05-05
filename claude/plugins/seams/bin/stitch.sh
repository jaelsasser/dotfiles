#!/usr/bin/env bash
# stitch.sh — bridge seams into the current context.
#
# Same-session (no arg): read cursor's seam, mark ## Tack (consumed), advance cursor.
# Cross-session  (<src>): copy seams from a prior session into CLAUDE_SESSION_ID.
#
# Usage: stitch.sh [<source-sid>]
#
# /seams:stitch advances the cursor; /seams:pattern (at ExitPlanMode) creates it.
# Source dir is never modified in cross-session mode.
set -eu

SOURCE_SID="${1:-}"
DEST_SID="${CLAUDE_SESSION_ID:-}"
cache="${CLAUDE_SEAMS_DIR:-$HOME/.claude/cache/seams}"

[[ -n "$DEST_SID" ]] || { echo "ERROR: CLAUDE_SESSION_ID unset" >&2; exit 2; }

# ── Same-session bridge ────────────────────────────────────────────────────────
if [[ -z "$SOURCE_SID" ]] || [[ "$SOURCE_SID" == "$DEST_SID" ]]; then
  dst="$cache/$DEST_SID"
  cursor_file="$dst/cursor"
  [[ -f "$cursor_file" ]] || { echo "no upcoming seam"; exit 0; }
  cursor=$(cat "$cursor_file")
  active="$dst/seam-${cursor}.md"
  [[ -f "$active" ]] || { echo "no upcoming seam"; exit 0; }

  # Mark ## Tack consumed so a second /seams:stitch in the same session is a no-op.
  tmp=$(mktemp)
  sed 's|^## Tack[[:space:]]*$|## Tack (consumed)|' "$active" > "$tmp" \
    && mv "$tmp" "$active" || rm -f "$tmp"

  # Advance cursor to next existing seam.
  nm=""
  for f in "$dst"/seam-*.md; do
    [[ -f "$f" ]] || continue
    n=$(basename "$f" .md); n="${n#seam-}"
    if (( n > cursor )); then
      [[ -z "$nm" ]] || (( n < nm )) && nm=$n
    fi
  done
  [[ -n "$nm" ]] && echo "$nm" > "$cursor_file"

  printf '\n--- seam-%s.md ---\n' "$cursor"
  cat "$active"
  exit 0
fi

# ── Cross-session fork ────────────────────────────────────────────────────────
[[ "$SOURCE_SID" != "$DEST_SID" ]] || {
  echo "ERROR: source and destination session IDs are identical — use /seams:stitch for same-session bridge" >&2
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
for f in "$src"/seam-*.md; do
  [[ -f "$f" ]] || continue
  n=$(basename "$f" .md); n="${n#seam-}"
  cp "$f" "$dst/seam-${n}.md"
done
[[ -f "$src/plan" ]] && cp "$src/plan" "$dst/plan"
echo "$next_cursor" > "$dst/cursor"
active="$dst/seam-${next_cursor}.md"
if [[ -f "$active" ]]; then
  tmp=$(mktemp)
  sed 's|^## Tack[[:space:]]*$|## Tack (consumed)|' "$active" > "$tmp" \
    && mv "$tmp" "$active" || rm -f "$tmp"
fi
printf 'stitch: %s → %s (cursor %s → %s)\n' \
  "$SOURCE_SID" "$DEST_SID" "$src_cursor" "$next_cursor"
if [[ -f "$active" ]]; then
  printf '\n--- seam-%s.md ---\n' "$next_cursor"
  cat "$active"
fi
