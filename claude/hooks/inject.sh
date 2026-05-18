#!/usr/bin/env bash
# inject.sh <EVENT> <FILE> [--once [<category>]] [--when <jq-expr>]
# Injects FILE as hookSpecificOutput.additionalContext for EVENT.
# Relative FILE paths resolve against the script's directory.
# --when <jq-expr>: silently exits unless <jq-expr> evaluated against the hook input is `true`.
# --once [<category>]: fires only once per session; <category> defaults to event+file basename.
set -eu

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

EVENT="${1:-}"
FILE="${2:-}"
WHEN_EXPR=""
ONCE=0
ONCE_CATEGORY=""

if [[ $# -gt 2 ]]; then
  shift 2
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --once)
        ONCE=1
        if [[ -n "${2:-}" && "$2" != --* ]]; then
          ONCE_CATEGORY="$2"; shift
        fi
        ;;
      --when) WHEN_EXPR="${2:-}"; shift ;;
    esac
    shift
  done
fi

[[ -n "$EVENT" && -n "$FILE" ]] || { echo "usage: inject.sh <event> <file> [--once [<category>]] [--when <jq-expr>]" >&2; exit 1; }
[[ "$FILE" = /* ]] || FILE="$SCRIPT_DIR/$FILE"
[[ -f "$FILE" ]] || { echo "inject.sh: file not found: $FILE" >&2; exit 1; }

INPUT=$(cat)

if [[ -n "$WHEN_EXPR" ]]; then
  [[ "$(jq -r "$WHEN_EXPR" <<< "$INPUT")" == "true" ]] || exit 0
fi

if [[ "$ONCE" -eq 1 ]]; then
  SESSION=$(jq -r '.session_id // empty' <<< "$INPUT")
  if [[ -n "$SESSION" ]]; then
    CATEGORY="${ONCE_CATEGORY:-${EVENT}-$(basename "$FILE" .md)}"
    SENTINEL="/tmp/inject-${CATEGORY}-${SESSION}.fired"
    [[ -f "$SENTINEL" ]] && exit 0
    touch "$SENTINEL"
  fi
fi

jq -n --arg event "$EVENT" --rawfile prose "$FILE" \
  '{hookSpecificOutput: {hookEventName: $event, additionalContext: $prose}}'
