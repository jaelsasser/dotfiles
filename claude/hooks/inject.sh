#!/usr/bin/env bash
# inject.sh <EVENT> <FILE>... [--once|--debounce <turns>] [--when <jq-expr>] [-- <FILE>...]
# Injects FILEs as hookSpecificOutput.additionalContext for EVENT, joined by blank lines.
# Relative FILE paths resolve against the script's directory.
# --when <jq-expr>: silently exits unless <jq-expr> evaluated against the hook input is `true`.
# --once: content-hash dedup. Same prose injected at most once per session, regardless of caller.
# --debounce <turns>: content-hash dedup. Same prose injected at most once per <turns> top-level
#   model API calls in the session's transcript (counted as unique requestIds on assistant lines).
#   Parallel tool calls share a requestId, so --debounce 1 suppresses simultaneous-tool duplicates.
#   Subagent work doesn't count: sidechain messages live in separate transcript files.
# --: end of options; remaining args are files (even if they start with `--`).
set -eu

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

usage() {
  echo "usage: inject.sh <event> <file>... [--once|--debounce <turns>] [--when <jq-expr>]" >&2
  exit 1
}

EVENT="${1:-}"
[[ -n "$EVENT" ]] || usage
shift

FILES=()
WHEN_EXPR=""
ONCE=0
DEBOUNCE=0
END_OF_OPTS=0

while [[ $# -gt 0 ]]; do
  if [[ "$END_OF_OPTS" -eq 1 ]]; then
    FILES+=("$1"); shift; continue
  fi
  case "$1" in
    --) END_OF_OPTS=1 ;;
    --once) ONCE=1 ;;
    --debounce)
      [[ -n "${2:-}" ]] || { echo "inject.sh: --debounce requires an argument" >&2; exit 1; }
      [[ "$2" =~ ^[1-9][0-9]*$ ]] || { echo "inject.sh: --debounce requires a positive integer" >&2; exit 1; }
      DEBOUNCE="$2"; shift
      ;;
    --when)
      [[ -n "${2:-}" ]] || { echo "inject.sh: --when requires an argument" >&2; exit 1; }
      WHEN_EXPR="$2"; shift
      ;;
    --*) echo "inject.sh: unknown flag: $1" >&2; exit 1 ;;
    *) FILES+=("$1") ;;
  esac
  shift
done

[[ ${#FILES[@]} -gt 0 ]] || usage
[[ "$ONCE" -eq 1 && "$DEBOUNCE" -gt 0 ]] && { echo "inject.sh: --once and --debounce are mutually exclusive" >&2; exit 1; }

for i in "${!FILES[@]}"; do
  f="${FILES[$i]}"
  [[ "$f" = /* ]] || f="$SCRIPT_DIR/$f"
  [[ -f "$f" ]] || { echo "inject.sh: file not found: $f" >&2; exit 1; }
  FILES[$i]="$f"
done

INPUT=$(cat)

if [[ -n "$WHEN_EXPR" ]]; then
  [[ "$(jq -r "$WHEN_EXPR" <<< "$INPUT")" == "true" ]] || exit 0
fi

PROSE=$(
  sep=""
  for f in "${FILES[@]}"; do
    printf '%s' "$sep"
    cat "$f"
    sep=$'\n\n'
  done
)

if [[ "$ONCE" -eq 1 || "$DEBOUNCE" -gt 0 ]]; then
  SESSION=$(jq -r '.session_id // empty' <<< "$INPUT")
  if [[ -n "$SESSION" ]]; then
    HASH=$(printf '%s' "$PROSE" | shasum -a 256 | awk '{print $1}')
    STAMP_DIR="${HOME}/.claude/inject/${SESSION}"
    mkdir -p "$STAMP_DIR"
    if [[ "$ONCE" -eq 1 ]]; then
      STAMP="${STAMP_DIR}/${HASH}.stamp"
      [[ -f "$STAMP" ]] && exit 0
      touch "$STAMP"
    else
      TRANSCRIPT=$(jq -r '.transcript_path // empty' <<< "$INPUT")
      if [[ -n "$TRANSCRIPT" && -f "$TRANSCRIPT" ]]; then
        STAMP="${STAMP_DIR}/${HASH}.debounce"
        CURRENT=$(jq -r 'select(.type=="assistant") | .requestId // empty' "$TRANSCRIPT" 2>/dev/null | sort -u | wc -l | tr -d ' \n')
        if [[ -f "$STAMP" ]]; then
          LAST=$(cat "$STAMP")
          [[ $((CURRENT - LAST)) -lt "$DEBOUNCE" ]] && exit 0
        fi
        printf '%s' "$CURRENT" > "$STAMP"
      fi
    fi
  fi
fi

jq -n --arg event "$EVENT" --arg prose "$PROSE" \
  '{hookSpecificOutput: {hookEventName: $event, additionalContext: $prose}}'
