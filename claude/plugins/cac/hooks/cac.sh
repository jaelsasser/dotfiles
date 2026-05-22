#!/usr/bin/env bash
# CAC (compact-and-continue) hook dispatcher.
#
#   cac.sh --nag    PreToolUse hook: hard-block every tool call while the
#                   restricted-mode marker is live.
#   cac.sh --bail   UserPromptSubmit hook: operator-typed cancellation;
#                   unlink the marker and inject the EXITING-cancelled
#                   message. No-op when the marker is absent.
#   cac.sh --done   SessionStart matcher=compact hook: compaction completed;
#                   unlink the marker and inject the EXITING-complete
#                   message. No-op when the marker is absent (i.e. the
#                   /compact didn't come from /cac:compact-and-continue).
set -eu

NAG_REASON='<important>Tool calls restricted due to pending compaction: all reads will be lost, writes force operator intervention; please immediately end your turn.</important>'
BAIL_MSG='<important>Transcript compaction pending, tool calls restricted until compaction or "Continue."</important>'
DONE_MSG='<important>Transcript compaction complete, all tool call restrictions lifted.</important>'

usage() {
    echo "usage: cac.sh --nag | --bail | --done" >&2
    exit 1
}

arm_path() {
    local sid
    sid=$(jq -r '.session_id // empty')
    [ -n "$sid" ] || return 1
    printf '%s/.claude/cache/%s.cac.json' "$HOME" "$sid"
}

cmd_nag() {
    local arm
    arm=$(arm_path <<<"$INPUT") || exit 0
    [ -f "$arm" ] || exit 0

    jq -n --arg reason "$NAG_REASON" \
        '{hookSpecificOutput: {hookEventName: "PreToolUse", permissionDecision: "deny", permissionDecisionReason: $reason}}'
}

cmd_bail() {
    local arm
    arm=$(arm_path <<<"$INPUT") || exit 0
    [ -f "$arm" ] || exit 0
    rm -f "$arm"

    jq -n --arg ctx "$BAIL_MSG" \
        '{hookSpecificOutput: {hookEventName: "UserPromptSubmit", additionalContext: $ctx}}'
}

cmd_done() {
    local arm
    arm=$(arm_path <<<"$INPUT") || exit 0
    [ -f "$arm" ] || exit 0
    rm -f "$arm"

    printf '%s\n' "$DONE_MSG"
}

[ $# -eq 1 ] || usage
INPUT=$(cat)

case "$1" in
    --nag) cmd_nag ;;
    --bail) cmd_bail ;;
    --done) cmd_done ;;
    *) usage ;;
esac
