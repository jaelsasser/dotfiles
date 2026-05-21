#!/usr/bin/env bash
# CAC (compact-and-continue) hook dispatcher.
#
#   cac.sh --check   Stop hook: block re-engagement while the arm marker
#                    is live, but allow the first Stop within the
#                    grace window through so the agent's own
#                    acknowledgement turn ends naturally.
#   cac.sh --bail    UserPromptSubmit hook: cancel a pre-fire arm by
#                    removing the marker. Idempotent.
set -eu

GRACE_SECS=5

usage() {
    echo "usage: cac.sh --check | --bail" >&2
    exit 1
}

mtime() {
    # BSD/macOS stat first; fall back to GNU/Linux stat.
    stat -f %m "$1" 2>/dev/null || stat -c %Y "$1"
}

arm_path() {
    local sid
    sid=$(jq -r '.session_id // empty')
    [ -n "$sid" ] || return 1
    printf '%s/.claude/cache/%s.cac.json' "$HOME" "$sid"
}

cmd_check() {
    local arm now mt age remaining time
    arm=$(arm_path <<<"$INPUT") || exit 0
    [ -f "$arm" ] || exit 0

    now=$(date +%s)
    [ -f "$arm" ] || exit 0
    mt=$(mtime "$arm")
    age=$((now - mt))
    [ "$age" -lt "$GRACE_SECS" ] && exit 0

    remaining=$(jq -r '.fires_at_epoch // empty' <"$arm" 2>/dev/null || true)
    if [ -n "$remaining" ]; then
        remaining=$((remaining - now))
        [ "$remaining" -lt 0 ] && remaining=0
        time=$(printf '%d:%02d' $((remaining / 60)) $((remaining % 60)))
    else
        time="<unknown>"
    fi

    jq -n --arg reason "CAC armed: /compact fires in $time. Halt — the operator wants the veto window. If the harness forces another turn anyway, re-call mcp__cac__arm with immediate=True to fast-forward." \
        '{decision: "block", reason: $reason}'
}

cmd_bail() {
    local arm
    arm=$(arm_path <<<"$INPUT") || exit 0
    rm -f "$arm"
}

[ $# -eq 1 ] || usage
INPUT=$(cat)

case "$1" in
    --check) cmd_check ;;
    --bail) cmd_bail ;;
    *) usage ;;
esac
