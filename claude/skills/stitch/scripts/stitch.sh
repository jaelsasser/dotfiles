#!/usr/bin/env bash
# stitch.sh — seam migration helper for the /stitch skill.
#
# list                    Enumerate prior session seam dirs with pending files.
# take <source-sid>       Read source cursor + seams (read-only); copy into
#                         $CLAUDE_SESSION_ID dir; advance dst cursor to the
#                         next existing seam. Source dir is never modified.
#
# /stitch is one of the two cursor-advancing events (the other is /compact
# via SessionStart). /baste and /tack never advance the cursor.
set -eu

COMMAND="${1:-}"
cache="${CLAUDE_SEAMS_DIR:-$HOME/.claude/cache/seams}"

case "$COMMAND" in
  list)
    [[ -d "$cache" ]] || { echo "no seams cache"; exit 0; }
    found=0
    for dir in $(ls -dt "$cache"/*/ 2>/dev/null); do
      [[ -d "$dir" ]] || continue
      sid=$(basename "$dir")
      cursor_file="$dir/cursor"
      [[ -f "$cursor_file" ]] || continue
      cursor=$(cat "$cursor_file")
      # Collect pending seam files (N >= cursor)
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
      printf 'sid: %s\n  cursor: %s\n  mtime: %s\n  pending: %s\n  head: %s\n  plan: %s\n\n' \
        "$sid" "$cursor" "$mtime" "${pending[*]}" "$first_line" "$plan_path"
      found=1
    done
    (( found )) || echo "no prior seams found"
    ;;

  take)
    SOURCE_SID="${2:-}"
    DEST_SID="${CLAUDE_SESSION_ID:-}"
    [[ -n "$SOURCE_SID" ]] || { echo "ERROR: take requires <source-sid>" >&2; exit 2; }
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
    ;;

  *)
    echo "ERROR: usage: stitch.sh list | take <source-sid>" >&2
    exit 2
    ;;
esac
