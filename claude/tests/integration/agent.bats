#!/usr/bin/env bats
#
# End-to-end smokes: launch Haiku agents against the live ~/.claude/settings.json.
# Catches harness wiring drift and hook output that fails Claude Code schema validation.

load '../lib/helpers'

setup() {
    HOOKS="$REPO/claude/hooks"
    export TMPDIR="$BATS_TEST_TMPDIR/"
}

@test "smoke: claude -p in plan mode delivers PLAN.md content with no validation errors" {
    command -v claude >/dev/null || skip "claude CLI not installed"

    run claude -p \
        --no-session-persistence \
        --output-format stream-json \
        --include-hook-events \
        --permission-mode plan \
        --model claude-haiku-4-5 \
        "Respond with only the word: ok"
    [ "$status" -eq 0 ]

    if [[ "$output" == *"Hook JSON output validation failed"* ]]; then
        echo "FAIL: hook output rejected by Claude Code schema" >&2
        echo "$output" | grep -B1 -A4 "Hook JSON output validation failed" >&2
        return 1
    fi

    if [[ "$output" != *"Commit boundaries"* ]]; then
        echo "FAIL: PLAN.md content not seen in stream output" >&2
        echo "$output" | grep -i "hook_response\|hook_started" | head -5 >&2
        return 1
    fi
}

@test "multiturn: yield detail survives /compact via COMPACT.md preservation" {
    command -v claude >/dev/null || skip "claude CLI not installed"

    SDIR="$BATS_TEST_TMPDIR/session"
    mkdir -p "$SDIR"
    cd "$SDIR"

    MAGIC="MIG-7HqP-2025-XKQ4"

    run claude -p \
        --output-format stream-json \
        --include-hook-events \
        --permission-mode plan \
        --model claude-haiku-4-5 \
        "Use TodoWrite to record one todo for an upcoming chunk: 'commit: schema applied (yield after completion) next chunk needs: ${MAGIC}'. Then call ExitPlanMode."
    [ "$status" -eq 0 ]
    [[ "$output" == *"PostToolUse:ExitPlanMode"* ]]
    [[ "$output" == *"already handled"* ]]

    run claude -p --continue \
        --output-format stream-json \
        --include-hook-events \
        --model claude-haiku-4-5 \
        "/compact"
    [ "$status" -eq 0 ]
    [[ "$output" == *"preservation target"* ]]
    [[ "$output" == *"SessionStart:compact"* ]]
    [[ "$output" == *"build a live todo list"* ]]

    run claude -p --continue \
        --output-format text \
        --model claude-haiku-4-5 \
        "What identifier did I ask you to record after 'next chunk needs:'? Reply with just the identifier."
    [ "$status" -eq 0 ]
    if [[ "$output" != *"$MAGIC"* ]]; then
        echo "FAIL: magic token '$MAGIC' did not survive /compact" >&2
        echo "Agent reply: $output" >&2
        return 1
    fi
}
