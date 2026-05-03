#!/usr/bin/env bash
# Wired in claude/settings.json under hooks.PreToolUse (matcher EnterPlanMode)
# AND hooks.UserPromptSubmit. Injects plan-time-only guidance from claude/PLAN.md.

EVENT="${1:-PreToolUse}"
PLAN_PATH="$(dirname "$(realpath "${BASH_SOURCE[0]}")")/../PLAN.md"

if [[ "$EVENT" == "UserPromptSubmit" ]]; then
  MODE=$(jq -r '.permission_mode // empty')
  [[ "$MODE" == "plan" ]] || exit 0
fi

jq -n --arg event "$EVENT" --rawfile prose "$PLAN_PATH" \
  '{hookSpecificOutput: {hookEventName: $event, additionalContext: $prose}}'
