#!/usr/bin/env bash
# baste.sh — append stdin to the active session seam.
# See `baste.sh --help` for schema and call shapes.
set -eu

print_agents() {
  cat <<'AGENTS'
baste.sh [--sid <session-id>] [--strip-baste] [--model opus|sonnet|haiku] < CONTENT
  Appends stdin to <repo>/.claude/cache/seams/<sid>/seam-<cursor>.md (creates if missing).
  --sid: session ID (falls back to $CLAUDE_SESSION_ID env var).
  --strip-baste: delete any ## Baste section before append; inserts new content before the
    first ## Tack section (or at end if none). TTY stdin = strip-only (no append).
  --model: rewrite the Model: field after append.
  Cursor is read-only here — only /compact (SessionStart) and /stitch advance it.
  Prints: seam file path.
AGENTS
}

print_help() {
  cat <<'HELP'
baste.sh — append stdin to the active session seam.

Usage
  baste.sh [--sid <session-id>] [--strip-baste] [--model M] < CONTENT

  Stdin appends verbatim to <repo>/.claude/cache/seams/<sid>/seam-<cursor>.md,
  creating the file (and parent dir) if missing. Cursor is read from
  <repo>/.claude/cache/seams/<sid>/cursor; defaults to 1 if absent.

  Cursor advancement is NOT a baste responsibility. /baste is an idempotent
  arm — it rewrites the bundle, never advances. Cursor advances only at
  /compact (SessionStart) and /stitch (fresh session).

  Session id resolves in order: --sid flag, $CLAUDE_SESSION_ID env var. Errors
  if both absent.

Common shapes
  Append a runtime fact:
    printf -- '- new file: foo.py\n' | baste.sh --sid "$SID"

  Strip old ## Baste section, insert fresh bundle before ## Tack (used by /baste):
    printf '## Baste\n...\n' | baste.sh --sid "$SID" --strip-baste

Options
  --sid <id>          Session ID. Falls back to $CLAUDE_SESSION_ID.
  --strip-baste       Delete any ## Baste section from the seam file; insert new
                      content immediately before the first ## Tack section (or
                      append at end if none). With TTY stdin: strip-only (no append).
  --model <M>         Rewrite the Model: field after append. M ∈ opus|sonnet|haiku.
                      Errors (exit 2) if the Model field is absent.

Exit
  0 success; 2 usage error.
HELP
}

sid=""
model=""
strip_baste=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --sid)          sid="${2:-}"; shift 2 ;;
    --model)        model="${2:-}"; shift 2 ;;
    --strip-baste)  strip_baste=1; shift ;;
    -h|--help)      print_help; exit 0 ;;
    --agents)       print_agents; exit 0 ;;
    *) echo "ERROR: unknown arg $1" >&2; exit 2 ;;
  esac
done

[[ -n "$sid" ]] || sid="${CLAUDE_SESSION_ID:-}"
[[ -n "$sid" ]] || { echo "ERROR: --sid <session-id> required (or set \$CLAUDE_SESSION_ID)" >&2; exit 2; }

CACHE_ROOT="${CLAUDE_SEAMS_DIR:-$HOME/.claude/cache/seams}"
cache="${CACHE_ROOT}/${sid}"
mkdir -p "$cache"

cursor_file="$cache/cursor"
cursor=$(cat "$cursor_file" 2>/dev/null || echo 1)
[[ -f "$cursor_file" ]] || echo "$cursor" > "$cursor_file"
target="$cache/seam-${cursor}.md"

if (( strip_baste )); then
  touch "$target"
  if [[ -t 0 ]]; then
    # TTY stdin: strip-only, no append.
    tmp=$(mktemp)
    if awk '
      /^## Baste([[:space:]]+\(consumed\))?[[:space:]]*$/ { in_section = 1; next }
      in_section && /^## / { in_section = 0; print; next }
      in_section { next }
      { print }
    ' "$target" > "$tmp"; then
      mv "$tmp" "$target"
    else
      rm -f "$tmp"
      echo "ERROR: --strip-baste: awk failed on $target" >&2
      exit 2
    fi
  else
    # Piped stdin: strip old ## Baste, read new content, insert before first ## Tack.
    new_tmp=$(mktemp)
    cat > "$new_tmp"
    file_tmp=$(mktemp)
    if awk '
      FNR == NR { new[++nc] = $0; next }
      /^## Baste([[:space:]]+\(consumed\))?[[:space:]]*$/ { in_section = 1; next }
      in_section && /^## / { in_section = 0 }
      in_section { next }
      /^## Tack[[:space:]]*$/ && !inserted {
        for (i = 1; i <= nc; i++) print new[i]
        inserted = 1
      }
      { print }
      END { if (!inserted) for (i = 1; i <= nc; i++) print new[i] }
    ' "$new_tmp" "$target" > "$file_tmp"; then
      mv "$file_tmp" "$target"
    else
      rm -f "$file_tmp" "$new_tmp"
      echo "ERROR: --strip-baste: awk failed on $target" >&2
      exit 2
    fi
    rm -f "$new_tmp"
  fi
else
  cat >> "$target"
fi

if [[ -n "$model" ]]; then
  case "$model" in
    opus|sonnet|haiku) ;;
    *) echo "ERROR: --model must be opus|sonnet|haiku, got '$model'" >&2; exit 2 ;;
  esac
  tmp=$(mktemp)
  if awk -v m="$model" '
    BEGIN { done = 0; armed = 0 }
    !done && /^## Model[[:space:]]*$/ { armed = 1; print; next }
    !done && armed && /^[[:space:]]*$/ { print; next }
    !done && armed && /^#/ { armed = 0; print; next }
    !done && armed { print m; done = 1; armed = 0; next }
    !done && /^Model:[[:space:]]/ { print "Model: " m; done = 1; next }
    { print }
    END { exit (done ? 0 : 1) }
  ' "$target" > "$tmp"; then
    mv "$tmp" "$target"
  else
    rm -f "$tmp"
    echo "ERROR: --model: no Model field found in $target" >&2
    exit 2
  fi
fi

echo "$target"
