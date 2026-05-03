#!/usr/bin/env bats
#
# Unit tests for the seam subsystem: get-seam.sh, tack.sh, baste.sh, seamster.sh, stitch.sh.

load '../lib/helpers'

setup() {
    HOOKS="$REPO/claude/hooks"
    TACK="$REPO/claude/skills/tack/scripts/tack.sh"
    BASTE="$REPO/claude/skills/baste/scripts/baste.sh"
    STITCH="$REPO/claude/skills/stitch/scripts/stitch.sh"
    SEAMSTER="$REPO/claude/hooks/seamster.sh"
    setup_seams
}

# --- tack.sh ---

@test "tack.sh: each call appends a fresh ## Tack section" {
    run bash -c "printf '- a\n' | CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$TACK' --sid t-sid"
    [ "$status" -eq 0 ]
    run bash -c "printf '- b\n' | CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$TACK' --sid t-sid"
    [ "$status" -eq 0 ]
    count=$(grep -c '^## Tack$' "$CLAUDE_SEAMS_DIR/t-sid/seam-1.md")
    [ "$count" -eq 2 ]
}

# --- baste.sh ---

@test "baste.sh: appends content and round-trips via get-seam.sh" {
    bash -c "printf '# Seam 1\nModel: opus\n' | CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$BASTE' --sid b-sid"
    run bash -c "CLAUDE_SESSION_ID=b-sid CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$HOOKS/get-seam.sh' --read"
    [ "$status" -eq 0 ]
    [[ "$output" == *"Model: opus"* ]]
}

@test "baste.sh: --strip-baste removes old ## Baste and inserts before ## Tack" {
    mkdir -p "$CLAUDE_SEAMS_DIR/sb-sid"
    echo "1" > "$CLAUDE_SEAMS_DIR/sb-sid/cursor"
    cat > "$CLAUDE_SEAMS_DIR/sb-sid/seam-1.md" <<'EOF'
# Seam 1
## Baste
old bundle
## Tack
- keep me
EOF
    run bash -c "printf '## Baste\nnew bundle\n' | CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$BASTE' --sid sb-sid --strip-baste"
    [ "$status" -eq 0 ]
    local f="$CLAUDE_SEAMS_DIR/sb-sid/seam-1.md"
    ! grep -q "old bundle" "$f"
    grep -q "new bundle" "$f"
    grep -q "keep me" "$f"
    # ## Baste must appear before ## Tack
    baste_line=$(grep -n '^## Baste$' "$f" | head -1 | cut -d: -f1)
    tack_line=$(grep -n '^## Tack$' "$f" | head -1 | cut -d: -f1)
    (( baste_line < tack_line ))
}

# --- seamster.sh ---

@test "seamster.sh: seeds default seam-1 with plan path for single-window plan" {
    local tmp; tmp=$(mktemp "$HOME/.claude/plans/sm-XXXXXX"); local plan="${tmp}.md"; mv "$tmp" "$plan"
    printf '# Plan\nNo seams here.\n' > "$plan"
    echo '{"session_id":"sm-sid"}' | CLAUDE_SEAMS_DIR="$CLAUDE_SEAMS_DIR" "$SEAMSTER"
    local cache="$CLAUDE_SEAMS_DIR/sm-sid"
    [ -f "$cache/seam-1.md" ]
    [ "$(cat "$cache/cursor")" = "1" ]
    grep -q "Plan: $plan" "$cache/seam-1.md"
    [ "$(cat "$cache/plan")" = "$plan" ]
    rm -f "$plan"
}

@test "seamster.sh: preserves seams below cursor on sub-plan re-entry" {
    local tmp; tmp=$(mktemp "$HOME/.claude/plans/sm2-XXXXXX"); local plan="${tmp}.md"; mv "$tmp" "$plan"
    cat > "$plan" <<'EOF'
# Plan

## Seams

### Seam 4 → 5
Model: sonnet
Needs planning: no
Goal: Window four.
Anchors:
- Plan: test
EOF
    local cache="$CLAUDE_SEAMS_DIR/sm2-sid"
    mkdir -p "$cache"
    printf '3\n' > "$cache/cursor"
    printf 'seam1\n' > "$cache/seam-1.md"
    printf 'seam3\n' > "$cache/seam-3.md"
    echo '{"session_id":"sm2-sid"}' | CLAUDE_SEAMS_DIR="$CLAUDE_SEAMS_DIR" "$SEAMSTER"
    [ "$(cat "$cache/seam-1.md")" = "seam1" ]
    [ "$(cat "$cache/seam-3.md")" = "seam3" ]
    [ -f "$cache/seam-4.md" ]
    rm -f "$plan"
}

# --- stitch.sh ---

@test "stitch.sh list: prints pending sessions with plan field" {
    local cache="$CLAUDE_SEAMS_DIR/st-src"
    mkdir -p "$cache"
    printf '1\n' > "$cache/cursor"
    printf '# Seam 1\n' > "$cache/seam-1.md"
    printf '/some/plan.md\n' > "$cache/plan"
    run bash -c "CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$STITCH' list"
    [ "$status" -eq 0 ]
    [[ "$output" == *"st-src"* ]]
    [[ "$output" == *"plan: /some/plan.md"* ]]
}

@test "stitch.sh take: migrates seams and advances dst cursor" {
    local src="$CLAUDE_SEAMS_DIR/st-take-src"
    mkdir -p "$src"
    echo "2" > "$src/cursor"
    printf '# Seam 2\n' > "$src/seam-2.md"
    printf '# Seam 3\n## Baste\n/model sonnet\n' > "$src/seam-3.md"
    printf '/the/plan.md\n' > "$src/plan"
    run bash -c "CLAUDE_SESSION_ID=st-take-dst CLAUDE_SEAMS_DIR='$CLAUDE_SEAMS_DIR' '$STITCH' take st-take-src"
    [ "$status" -eq 0 ]
    local dst="$CLAUDE_SEAMS_DIR/st-take-dst"
    [ -f "$dst/seam-2.md" ]
    [ -f "$dst/seam-3.md" ]
    [ "$(cat "$dst/cursor")" = "3" ]
    [ "$(cat "$dst/plan")" = "/the/plan.md" ]
    grep -q "Baste (consumed)" "$dst/seam-3.md"
}
