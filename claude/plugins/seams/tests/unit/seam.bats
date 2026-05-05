#!/usr/bin/env bats
#
# Unit tests for the seam subsystem: get-seam.sh, chalk.sh, tack.sh, stitch.sh, seams.sh.

load '../lib/helpers'

setup() {
    BIN="$REPO/claude/plugins/seams/bin"
    CHALK="$BIN/chalk.sh"
    TACK="$BIN/tack.sh"
    STITCH="$BIN/stitch.sh"
    SEAMS="$BIN/seams.sh"
    setup_seams
}

# --- chalk.sh ---

@test "chalk.sh: each call appends a fresh ## Chalk section" {
    run bash -c "printf '- a\n' | CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$CHALK' --sid c-sid"
    [ "$status" -eq 0 ]
    run bash -c "printf '- b\n' | CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$CHALK' --sid c-sid"
    [ "$status" -eq 0 ]
    count=$(grep -c '^## Chalk$' "$CLAUDE_SEAMS_DIR/c-sid/seam-0.md")
    [ "$count" -eq 2 ]
}

@test "chalk.sh: defaults cursor to 0" {
    run bash -c "printf '- x\n' | CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$CHALK' --sid ck-sid"
    [ "$status" -eq 0 ]
    [ -f "$CLAUDE_SEAMS_DIR/ck-sid/seam-0.md" ]
}

# --- tack.sh ---

@test "tack.sh: each call appends a fresh ## Tack section" {
    run bash -c "printf '- a\n' | CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$TACK' --sid tk-sid"
    [ "$status" -eq 0 ]
    run bash -c "printf '- b\n' | CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$TACK' --sid tk-sid"
    [ "$status" -eq 0 ]
    count=$(grep -c '^## Tack$' "$CLAUDE_SEAMS_DIR/tk-sid/seam-0.md")
    [ "$count" -eq 2 ]
}

@test "tack.sh: defaults cursor to 0" {
    run bash -c "printf '- y\n' | CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$TACK' --sid tck-sid"
    [ "$status" -eq 0 ]
    [ -f "$CLAUDE_SEAMS_DIR/tck-sid/seam-0.md" ]
}

@test "tack.sh and chalk.sh accumulate independently in same seam" {
    bash -c "printf '%s\n' '- chalk note' | CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$CHALK' --sid mix-sid"
    bash -c "printf '%s\n' '- tack note'  | CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$TACK'  --sid mix-sid"
    local f="$CLAUDE_SEAMS_DIR/mix-sid/seam-0.md"
    grep -q '^## Chalk$' "$f"
    grep -q '^## Tack$'  "$f"
    grep -q 'chalk note'  "$f"
    grep -q 'tack note'   "$f"
}

# --- get-seam.sh ---

@test "get-seam.sh: round-trips tack content" {
    bash -c "printf '# Seam 0\nModel: opus\n' | CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$TACK' --sid gs-sid"
    run bash -c "CLAUDE_SESSION_ID=gs-sid CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$BIN/get-seam.sh' --read"
    [ "$status" -eq 0 ]
    [[ "$output" == *"Model: opus"* ]]
}

# --- seams.sh ---

@test "seams.sh: prints pending sessions with plan and count fields" {
    local cache="$CLAUDE_SEAMS_DIR/st-src"
    mkdir -p "$cache"
    printf '0\n' > "$cache/cursor"
    printf '# Seam 0\n' > "$cache/seam-0.md"
    printf '/some/plan.md\n' > "$cache/plan"
    run bash -c "CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$SEAMS'"
    [ "$status" -eq 0 ]
    [[ "$output" == *"st-src"* ]]
    [[ "$output" == *"count: 1"* ]]
    [[ "$output" == *"plan: /some/plan.md"* ]]
}

# --- stitch.sh (same-session) ---

@test "stitch.sh same-session: marks ## Tack consumed and advances cursor" {
    local cache="$CLAUDE_SEAMS_DIR/ss-sid"
    mkdir -p "$cache"
    printf '0\n' > "$cache/cursor"
    printf '# Seam 0\n\n## Tack\n/model sonnet\n' > "$cache/seam-0.md"
    printf '# Seam 1\n' > "$cache/seam-1.md"
    run bash -c "CLAUDE_SESSION_ID=ss-sid CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$STITCH'"
    [ "$status" -eq 0 ]
    grep -q "Tack (consumed)" "$cache/seam-0.md"
    [ "$(cat "$cache/cursor")" = "1" ]
    [[ "$output" == *"seam-0.md"* ]]
}

@test "stitch.sh same-session: no upcoming seam returns gracefully" {
    run bash -c "CLAUDE_SESSION_ID=empty-sid CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$STITCH'"
    [ "$status" -eq 0 ]
    [[ "$output" == "no upcoming seam" ]]
}

# --- stitch.sh (cross-session) ---

@test "stitch.sh cross-session: migrates seams and advances dst cursor" {
    local src="$CLAUDE_SEAMS_DIR/cs-take-src"
    mkdir -p "$src"
    echo "1" > "$src/cursor"
    printf '# Seam 1\n' > "$src/seam-1.md"
    printf '# Seam 2\n## Tack\n/model sonnet\n' > "$src/seam-2.md"
    printf '/the/plan.md\n' > "$src/plan"
    run bash -c "CLAUDE_SESSION_ID=cs-take-dst CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$STITCH' cs-take-src"
    [ "$status" -eq 0 ]
    local dst="$CLAUDE_SEAMS_DIR/cs-take-dst"
    [ -f "$dst/seam-1.md" ]
    [ -f "$dst/seam-2.md" ]
    [ "$(cat "$dst/cursor")" = "2" ]
    [ "$(cat "$dst/plan")" = "/the/plan.md" ]
    grep -q "Tack (consumed)" "$dst/seam-2.md"
}
