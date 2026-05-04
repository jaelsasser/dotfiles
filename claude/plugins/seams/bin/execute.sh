#!/usr/bin/env bash
# Wired in hooks/hooks.json under:
#   PostToolUse (matcher ExitPlanMode) — fires once per session at first approved plan exit.
# Injects execution-phase guidance from EXECUTE_PLANNER.md (or EXECUTE.md as fallback).
# Uses the hookSpecificOutput.additionalContext schema.
set -eu

dir="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
EXECUTE_PATH="$dir/../EXECUTE.md"
EXECUTE_PLANNER_PATH="$dir/../EXECUTE_PLANNER.md"
INPUT=$(cat)

SESSION_ID=$(jq -r '.session_id // empty' <<<"$INPUT" 2>/dev/null || true)
if [[ -n "$SESSION_ID" ]]; then
  MARKER="${TMPDIR:-/tmp}/claude-execute-${SESSION_ID}"
  [[ -f "$MARKER" ]] && exit 0
  : > "$MARKER"
fi

prose_file="$EXECUTE_PLANNER_PATH"
[[ -f "$prose_file" ]] || prose_file="$EXECUTE_PATH"
jq -n --rawfile prose "$prose_file" \
  '{hookSpecificOutput: {hookEventName: "PostToolUse", additionalContext: $prose}}'
