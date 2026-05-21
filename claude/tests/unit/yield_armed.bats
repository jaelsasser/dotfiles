#!/usr/bin/env bats
# Stop-hook re-engagement defense for the yield armed window.

load ../lib/helpers

@test "no arm files → silent exit" {
    run armed_hook "no-arm"
    [ "$status" -eq 0 ]
    [ -z "$output" ]
}

@test "stop_hook_active=true still blocks when armed (peer cooperation broken)" {
    local sid="active-sid" fires=$(($(date +%s) + 270))
    printf '{"state":"ARMED","fires_at_epoch":%d}' "$fires" \
        > "$HOME/.claude/cache/${sid}.compact.yield-arm"
    run armed_hook "$sid" "true"
    [ "$status" -eq 0 ]
    [[ "$(jq -r .decision <<<"$output")" == "block" ]]
}

@test "ARMED → block with fire-time and scary reason" {
    local sid="armed-sid" fires=$(($(date +%s) + 270))
    printf '{"state":"ARMED","fires_at_epoch":%d}' "$fires" \
        > "$HOME/.claude/cache/${sid}.compact.yield-arm"
    run armed_hook "$sid"
    [ "$status" -eq 0 ]
    [[ "$(jq -r .decision <<<"$output")" == "block" ]]
    local reason; reason=$(jq -r .reason <<<"$output")
    [[ "$reason" == *"Yield armed"* ]]
    [[ "$reason" =~ fires\ in\ (4:30|4:2[0-9]) ]]
    [[ "$reason" == *"operator's express intention"* ]]
}

@test "ARMED past deadline clamps to 0:00 in reason" {
    local sid="past-sid" fires=$(($(date +%s) - 30))
    printf '{"state":"ARMED","fires_at_epoch":%d}' "$fires" \
        > "$HOME/.claude/cache/${sid}.compact.yield-arm"
    run armed_hook "$sid"
    [[ "$(jq -r .reason <<<"$output")" == *"0:00"* ]]
}

@test "reason counters peer 'continue the plan' / 'goal not met' framing" {
    local sid="armed-sid" fires=$(($(date +%s) + 270))
    printf '{"state":"ARMED","fires_at_epoch":%d}' "$fires" \
        > "$HOME/.claude/cache/${sid}.compact.yield-arm"
    run armed_hook "$sid"
    local reason; reason=$(jq -r .reason <<<"$output")
    [[ "$reason" == *"continue the plan"* ]]
    [[ "$reason" == *"goal isn't met"* ]]
}
