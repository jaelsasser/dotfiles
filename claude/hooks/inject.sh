#!/usr/bin/env bash
# inject.sh <EVENT> <FILE> [--plan-mode-only]
# Injects FILE as hookSpecificOutput.additionalContext for EVENT.
# --plan-mode-only: silently exits on UserPromptSubmit when not in plan mode.
set -eu

EVENT="${1:-}"
FILE="${2:-}"
PLAN_GATE=0
[[ "${3:-}" == "--plan-mode-only" ]] && PLAN_GATE=1

[[ -n "$EVENT" && -n "$FILE" ]] || { echo "usage: inject.sh <event> <file> [--plan-mode-only]" >&2; exit 1; }
[[ -f "$FILE" ]] || { echo "inject.sh: file not found: $FILE" >&2; exit 1; }

if [[ "$EVENT" == "UserPromptSubmit" && "$PLAN_GATE" -eq 1 ]]; then
  MODE=$(jq -r '.permission_mode // empty')
  [[ "$MODE" == "plan" ]] || exit 0
fi

jq -n --arg event "$EVENT" --rawfile prose "$FILE" \
  '{hookSpecificOutput: {hookEventName: $event, additionalContext: $prose}}'
