#!/usr/bin/env bash
# inject.sh <EVENT> <FILE> [--no-once] [--permission_mode <mode>]
# Injects FILE as hookSpecificOutput.additionalContext for EVENT.
# --permission_mode <mode>: silently exits unless the session permission mode matches.
# --no-once: fires on every call; without this, fires once per session per event+file.
set -eu

EVENT="${1:-}"
FILE="${2:-}"
REQUIRED_MODE=""
NO_ONCE=0

if [[ $# -gt 2 ]]; then
  shift 2
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --no-once) NO_ONCE=1 ;;
      --permission_mode) REQUIRED_MODE="${2:-}"; shift ;;
    esac
    shift
  done
fi

[[ -n "$EVENT" && -n "$FILE" ]] || { echo "usage: inject.sh <event> <file> [--no-once] [--permission_mode <mode>]" >&2; exit 1; }
[[ -f "$FILE" ]] || { echo "inject.sh: file not found: $FILE" >&2; exit 1; }

INPUT=$(cat)

if [[ -n "$REQUIRED_MODE" ]]; then
  MODE=$(jq -r '.permission_mode // empty' <<< "$INPUT")
  [[ "$MODE" == "$REQUIRED_MODE" ]] || exit 0
fi

if [[ "$NO_ONCE" -eq 0 ]]; then
  SESSION=$(jq -r '.session_id // empty' <<< "$INPUT")
  if [[ -n "$SESSION" ]]; then
    SENTINEL="/tmp/inject-${EVENT}-$(basename "$FILE" .md)-${SESSION}.fired"
    [[ -f "$SENTINEL" ]] && exit 0
    touch "$SENTINEL"
  fi
fi

jq -n --arg event "$EVENT" --rawfile prose "$FILE" \
  '{hookSpecificOutput: {hookEventName: $event, additionalContext: $prose}}'
