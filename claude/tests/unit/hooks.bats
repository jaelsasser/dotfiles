#!/usr/bin/env bats
#
# Unit tests for claude/hooks/: execute.sh, planmode.sh, precompact.sh.

load '../lib/helpers'

setup() {
    HOOKS="$REPO/claude/hooks"
    export TMPDIR="$BATS_TEST_TMPDIR/"
    setup_seams
}

# --- execute.sh PostToolUse: first-fire-per-session marker logic ---

@test "execute.sh PostToolUse first call emits valid hookSpecificOutput JSON" {
    run bash -c 'echo "{\"session_id\":\"sess-A\"}" | "$0" PostToolUse' "$HOOKS/execute.sh"
    [ "$status" -eq 0 ]
    echo "$output" | jq -e '.hookSpecificOutput.hookEventName == "PostToolUse"' >/dev/null
    echo "$output" | jq -e '.hookSpecificOutput.additionalContext | length > 0' >/dev/null
}

@test "execute.sh PostToolUse second call same session is silent" {
    echo '{"session_id":"sess-B"}' | "$HOOKS/execute.sh" PostToolUse >/dev/null
    run bash -c 'echo "{\"session_id\":\"sess-B\"}" | "$0" PostToolUse' "$HOOKS/execute.sh"
    [ "$status" -eq 0 ]
    [ -z "$output" ]
}

# --- execute.sh SessionStart: plain stdout, baste injection ---

@test "execute.sh SessionStart emits plain stdout even when marker exists" {
    echo '{"session_id":"sess-D"}' | "$HOOKS/execute.sh" PostToolUse >/dev/null
    run bash -c "cd '$BATS_TEST_TMPDIR' && echo '{\"session_id\":\"sess-D\"}' | '$HOOKS/execute.sh' SessionStart"
    [ "$status" -eq 0 ]
    [[ "${output:0:1}" != "{" ]]
    [[ "$output" == *"build a live todo list"* ]]
}

@test "execute.sh SessionStart prepends Plan: line when plan file present" {
    local cache="$CLAUDE_SEAMS_DIR/exec-plan-sid"
    mkdir -p "$cache"
    printf '1\n' > "$cache/cursor"
    cat > "$cache/seam-1.md" <<'EOF'
# Seam 0 → 1
Model: sonnet

## Baste
Do the thing.
EOF
    printf '/the/plan.md\n' > "$cache/plan"
    run bash -c "cd '$BATS_TEST_TMPDIR' && echo '{\"session_id\":\"exec-plan-sid\"}' | '$HOOKS/execute.sh' SessionStart"
    [ "$status" -eq 0 ]
    [[ "$output" == *"Plan: /the/plan.md"* ]]
    [[ "$output" == *"Do the thing."* ]]
}

# --- planmode.sh PreToolUse ---

@test "planmode.sh PreToolUse emits valid hookSpecificOutput JSON" {
    run "$HOOKS/planmode.sh" PreToolUse
    [ "$status" -eq 0 ]
    echo "$output" | jq -e '.hookSpecificOutput.hookEventName == "PreToolUse"' >/dev/null
    echo "$output" | jq -e '.hookSpecificOutput.additionalContext | length > 0' >/dev/null
}

# --- planmode.sh UserPromptSubmit: gated on permission_mode ---

@test "planmode.sh UserPromptSubmit in plan mode emits valid JSON" {
    run bash -c 'echo "{\"permission_mode\":\"plan\"}" | "$0" UserPromptSubmit' "$HOOKS/planmode.sh"
    [ "$status" -eq 0 ]
    echo "$output" | jq -e '.hookSpecificOutput.hookEventName == "UserPromptSubmit"' >/dev/null
}

@test "planmode.sh UserPromptSubmit outside plan mode is silent" {
    run bash -c 'echo "{\"permission_mode\":\"acceptEdits\"}" | "$0" UserPromptSubmit' "$HOOKS/planmode.sh"
    [ "$status" -eq 0 ]
    [ -z "$output" ]
}

# --- precompact.sh: plain stdout + seam handover injection ---

@test "precompact.sh emits COMPACT.md prose unconditionally" {
    run bash -c "cd '$BATS_TEST_TMPDIR' && '$HOOKS/precompact.sh'"
    [ "$status" -eq 0 ]
    [[ "${output:0:1}" != "{" ]]
    [[ "$output" == *"preservation target"* ]]
}

@test "precompact.sh appends handover when session dir is armed" {
    local cache="$CLAUDE_SEAMS_DIR/pc-sid"
    mkdir -p "$cache"
    echo "1" > "$cache/cursor"
    cat > "$cache/seam-1.md" <<'EOF'
# Seam 1 → seam 2

## Next window needs
- migration version 0042
EOF
    run bash -c "echo '{\"session_id\":\"pc-sid\"}' | CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$HOOKS/precompact.sh'"
    [ "$status" -eq 0 ]
    [[ "$output" == *"preservation target"* ]]
    [[ "$output" == *"migration version 0042"* ]]
}
