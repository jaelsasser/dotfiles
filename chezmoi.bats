#!/usr/bin/env bats
# chezmoi.bats — the non-standard surface of this dotfiles tree, exercised against a
# throwaway $HOME so the real one is never touched. Four cases, one per chezmoi
# mechanism this repo bends: per-host email data, claude real-file deploy + local-only
# coexistence, the settings.json modify_ merge, and templated OS gating. Stock chezmoi
# (regular-file copies, the executable_ bit) is upstream's to test — we assert only the
# parts that would silently break *our* layout. apply() runs the whole tree with scripts
# + network externals excluded; any unrenderable template fails all four, so a clean
# apply is covered implicitly.

setup() {
    REPO="$(cd "$(dirname "$BATS_TEST_FILENAME")" && pwd)"
    TMP="$(mktemp -d)"
}

teardown() {
    rm -rf "$TMP"
}

apply() {
    # XDG_CONFIG_HOME, not HOME, is where chezmoi finds its own config — pin it
    # inside $TMP too, or a real ~/.config/chezmoi/chezmoi.toml [data] email leaks
    # in and shadows the .chezmoidata default we're asserting.
    HOME="$TMP" XDG_CONFIG_HOME="$TMP/.config" chezmoi apply \
        --source "$REPO" \
        --destination "$TMP" \
        --persistent-state "$TMP/state.boltdb" \
        --exclude=scripts,externals \
        "$@"
}

@test "git config deploys as a regular file with a per-host-overridable email" {
    apply
    [ -f "$TMP/.config/git/config" ] && [ ! -L "$TMP/.config/git/config" ]
    grep -q 'email = 103758+jaelsasser@users.noreply.github.com' "$TMP/.config/git/config"
    printf '[data]\n    email = "work@corp.example"\n' > "$TMP/chezmoi.toml"
    apply --config "$TMP/chezmoi.toml"
    grep -q 'email = work@corp.example' "$TMP/.config/git/config"
    ! grep -q 'noreply.github.com' "$TMP/.config/git/config"
}

@test "claude deploys real files; local-only entries coexist; cursor cross-links" {
    apply
    # managed config lands as real files/dirs — no farm symlinks
    [ -f "$TMP/.claude/USER_CLAUDE.md" ] && [ ! -L "$TMP/.claude/USER_CLAUDE.md" ]
    [ -d "$TMP/.claude/skills/handoff" ] && [ ! -L "$TMP/.claude/skills/handoff" ]
    [ -f "$TMP/.claude/skills/handoff/SKILL.md" ]
    # CLAUDE.md is a same-dir symlink: the source tree carries no literal CLAUDE.md
    # for a harness to misread as directory-level instructions
    [ "$(readlink "$TMP/.claude/CLAUDE.md")" = "USER_CLAUDE.md" ]
    grep -q '## Me' "$TMP/.claude/CLAUDE.md"
    # no exact_: a local-only skill beside the managed ones survives a re-apply
    mkdir -p "$TMP/.claude/skills/local-only"
    apply
    [ -d "$TMP/.claude/skills/local-only" ]
    # cursor re-shares the *deployed* claude skill (homeDir-relative symlink)
    [ -L "$TMP/.cursor/skills/handoff" ] && [ -e "$TMP/.cursor/skills/handoff" ]
    readlink "$TMP/.cursor/skills/handoff" | grep -q '/.claude/skills/handoff$'
}

@test "settings.json modify_ merge is idempotent and harness-key preserving" {
    mkdir -p "$TMP/.claude"
    printf '%s\n' '{"feedbackSurveyState":{"seen":true},"statusLine":{"x":1}}' \
        > "$TMP/.claude/settings.json"
    apply
    jq -e '.feedbackSurveyState.seen == true' "$TMP/.claude/settings.json" >/dev/null
    jq -e 'has("statusLine") | not'           "$TMP/.claude/settings.json" >/dev/null
    jq -e '.hooks.PreToolUse != null'         "$TMP/.claude/settings.json" >/dev/null
    cp "$TMP/.claude/settings.json" "$TMP/before.json"
    apply
    diff <(jq -S . "$TMP/before.json") <(jq -S . "$TMP/.claude/settings.json")
}

@test "darwin OS-gating hides the linux-only configs" {
    [ "$(uname -s)" = Darwin ] || skip "darwin-only gating"
    apply
    [ ! -e "$TMP/.config/i3/config" ]
    [ ! -e "$TMP/.config/xmonad" ]
}
