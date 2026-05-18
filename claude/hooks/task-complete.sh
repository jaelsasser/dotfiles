#!/usr/bin/env bash
# task-complete.sh <PROSE_FILE>
# PostToolUse hook for TaskCreate|TaskUpdate.
# Accumulates task state in a per-session sidecar; fires PROSE_FILE injection when all tasks complete.
set -eu

PROSE="${1:-}"
[[ -f "$PROSE" ]] || { echo "task-complete.sh: file not found: ${PROSE}" >&2; exit 1; }

INPUT=$(cat)
SESSION=$(jq -r '.session_id // empty' <<< "$INPUT")
TOOL=$(jq -r '.tool_name // empty' <<< "$INPUT")

[[ -n "$SESSION" && -n "$TOOL" ]] || exit 0

STATE="/tmp/inject-tasks-${SESSION}.json"
FIRED="/tmp/inject-tasks-fired-${SESSION}"

case "$TOOL" in
  TaskCreate)
    ID=$(jq -r '.tool_response.task.id // empty' <<< "$INPUT")
    [[ -n "$ID" ]] || exit 0
    if [[ -f "$STATE" ]]; then
      jq --arg id "$ID" '. + [{"id": $id, "status": "pending"}]' "$STATE" > "${STATE}.tmp"
    else
      jq -n --arg id "$ID" '[{"id": $id, "status": "pending"}]' > "${STATE}.tmp"
    fi
    mv "${STATE}.tmp" "$STATE"
    ;;
  TaskUpdate)
    [[ -f "$STATE" ]] || exit 0
    ID=$(jq -r '.tool_input.taskId // empty' <<< "$INPUT")
    STATUS=$(jq -r '.tool_input.status // empty' <<< "$INPUT")
    [[ -n "$ID" && -n "$STATUS" ]] || exit 0
    jq --arg id "$ID" --arg status "$STATUS" \
      'map(if .id == $id then .status = $status else . end)' "$STATE" > "${STATE}.tmp"
    mv "${STATE}.tmp" "$STATE"
    ;;
  *)
    exit 0
    ;;
esac

DONE=$(jq 'length > 0 and all(.status == "completed")' "$STATE")
[[ "$DONE" == "true" ]] || exit 0

# Dedup: skip re-fire if state hasn't changed since last fire
SNAPSHOT=$(jq -cS '.' "$STATE")
[[ -f "$FIRED" && "$(cat "$FIRED")" == "$SNAPSHOT" ]] && exit 0
echo "$SNAPSHOT" > "$FIRED"

jq -n --arg event "PostToolUse" --rawfile prose "$PROSE" \
  '{hookSpecificOutput: {hookEventName: $event, additionalContext: $prose}}'
