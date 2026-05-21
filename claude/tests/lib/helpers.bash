# Shared bats helpers for claude/tests/.
#
# Every test runs against an isolated $HOME under $BATS_TEST_TMPDIR so the
# real ~/.claude is never touched.

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"

setup() {
    export HOME="$BATS_TEST_TMPDIR"
    mkdir -p "$HOME/.claude/cache"
    YIELD_ARMED="$REPO_ROOT/claude/hooks/yield-armed.sh"
}

# Pipe Stop-hook input to yield-armed.sh. $2 sets stop_hook_active (default false).
armed_hook() {
    local sid="$1" active="${2:-false}"
    printf '{"session_id":"%s","stop_hook_active":%s}' "$sid" "$active" \
        | "$YIELD_ARMED"
}
