#!/usr/bin/env bash
# inject.sh <EVENT> <FILE> [--permission_mode <mode>]
# Injects FILE as hookSpecificOutput.additionalContext for EVENT.
# --permission_mode <mode>: silently exits unless the session permission mode matches.
# Fires once per session per event; subsequent calls are silent.
set -eu

EVENT="${1:-}"
FILE="${2:-}"
REQUIRED_MODE=""
[[ "${3:-}" == "--permission_mode" ]] && REQUIRED_MODE="${4:-}"

[[ -n "$EVENT" && -n "$FILE" ]] || { echo "usage: inject.sh <event> <file> [--permission_mode <mode>]" >&2; exit 1; }
[[ -f "$FILE" ]] || { echo "inject.sh: file not found: $FILE" >&2; exit 1; }

INPUT=$(cat)

if [[ -n "$REQUIRED_MODE" ]]; then
  MODE=$(jq -r '.permission_mode // empty' <<< "$INPUT")
  [[ "$MODE" == "$REQUIRED_MODE" ]] || exit 0
fi

SESSION=$(jq -r '.session_id // empty' <<< "$INPUT")
if [[ -n "$SESSION" ]]; then
  SENTINEL="/tmp/inject-${EVENT}-${SESSION}.fired"
  [[ -f "$SENTINEL" ]] && exit 0
  touch "$SENTINEL"
fi

jq -n --arg event "$EVENT" --rawfile prose "$FILE" \
  '{hookSpecificOutput: {hookEventName: $event, additionalContext: $prose}}'
