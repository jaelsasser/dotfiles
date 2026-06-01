#!/usr/bin/env bats
# chezmoi.bats — user-facing behaviour of the chezmoi source tree, exercised
# against a throwaway $HOME so the real one is never touched. Replaces stow.bats.
#
# Every test applies the whole source tree with run-scripts and network
# externals excluded (they'd sudo-edit /etc and hit the network); modify_
# scripts are *not* excluded — they're file entries, and the settings.json
# merge is the marquee behaviour to cover.

setup() {
    REPO="$(cd "$(dirname "$BATS_TEST_FILENAME")" && pwd)"
    TMP="$(mktemp -d)"
}

teardown() {
    rm -rf "$TMP"
}

apply() {
    HOME="$TMP" chezmoi apply \
        --source "$REPO" \
        --destination "$TMP" \
        --persistent-state "$TMP/state.boltdb" \
        --exclude=scripts,externals \
        "$@"
}

@test "managed config lands as a regular file" {
    apply
    [ -f "$TMP/.config/git/config" ]
    [ ! -L "$TMP/.config/git/config" ]
    [ -s "$TMP/.config/git/config" ]
    grep -q 'email = 103758+jaelsasser@users.noreply.github.com' "$TMP/.config/git/config"
}

@test "git email is per-host: machine-local data overrides the default" {
    apply
    grep -q 'email = 103758+jaelsasser@users.noreply.github.com' "$TMP/.config/git/config"
    printf '[data]\n    email = "work@corp.example"\n' > "$TMP/chezmoi.toml"
    apply --config "$TMP/chezmoi.toml"
    grep -q 'email = work@corp.example' "$TMP/.config/git/config"
    ! grep -q 'noreply.github.com' "$TMP/.config/git/config"
}

@test "executable bit survives the executable_ rename" {
    # A plain copy would ship 0644; this is the one place the bit fails silently.
    apply
    [ -x "$TMP/.config/bin/ediff.sh" ]
}

@test "claude tree is a per-entry farm: real dir, entries symlinked into the repo" {
    apply
    # the dir itself must stay real so local-only skills can live beside it
    [ -d "$TMP/.claude/skills" ]
    [ ! -L "$TMP/.claude/skills" ]
    # each managed skill is a symlink resolving back into the live repo tree
    [ -L "$TMP/.claude/skills/handoff" ]
    readlink "$TMP/.claude/skills/handoff" | grep -q '/claude/skills/handoff$'
    [ -e "$TMP/.claude/skills/handoff" ]
    # CLAUDE.md points at the user-level instructions
    readlink "$TMP/.claude/CLAUDE.md" | grep -q '/claude/USER_CLAUDE.md$'
}

@test "vim spine and nvim lua tier deploy as regular files" {
    apply
    [ -f "$TMP/.config/vim/vimrc" ] && [ ! -L "$TMP/.config/vim/vimrc" ]
    grep -q 'XDG_STATE_HOME' "$TMP/.config/vim/vimrc"
    [ -f "$TMP/.config/nvim/init.lua" ] && [ ! -L "$TMP/.config/nvim/init.lua" ]
    grep -q 'vim/vimrc' "$TMP/.config/nvim/init.lua"   # lua tier sources the spine
    [ ! -e "$TMP/.config/nvim/init.vim" ]              # old entrypoint gone from source
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

@test "cursor shares the deployed claude skill" {
    apply
    [ -L "$TMP/.cursor/skills/handoff" ]
    readlink "$TMP/.cursor/skills/handoff" | grep -q '/.claude/skills/handoff$'
    [ -e "$TMP/.cursor/skills/handoff" ]
}

@test "darwin OS-gating hides the linux-only configs" {
    [ "$(uname -s)" = Darwin ] || skip "darwin-only gating"
    apply
    [ ! -e "$TMP/.config/i3/config" ]
    [ ! -e "$TMP/.config/xmonad" ]
}
