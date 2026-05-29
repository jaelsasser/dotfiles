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
        --exclude=scripts,externals
}

@test "managed config lands as a regular file" {
    apply
    [ -f "$TMP/.config/git/config" ]
    [ ! -L "$TMP/.config/git/config" ]
    [ -s "$TMP/.config/git/config" ]
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

@test "migrate handover sweeps stow's relative+absolute repo links, spares the rest" {
    # Stow plants *relative* symlinks; the old configure.sh planted *absolute*
    # ones. The teardown must catch both (and only repo-ward links). Driven by
    # real stow so the fixture matches production link form.
    command -v stow >/dev/null 2>&1 || skip "stow not installed"
    FR="$(mktemp -d)"
    mkdir -p "$FR/git" "$FR/zsh/antidote" "$TMP/.config/git" "$TMP/.local/share" "$TMP/.claude/skills/my-local"
    printf '[user]\n' > "$FR/git/config"
    stow --no-folding -t "$TMP/.config/git" -d "$FR" git       # relative link
    ln -s "$FR/zsh/antidote" "$TMP/.local/share/antidote"      # absolute link (configure.sh-style)
    printf 'LOCAL\n' > "$TMP/.claude/skills/my-local/SKILL.md" # must survive
    ln -s /opt/elsewhere "$TMP/.config/foreign"                # foreign — must survive

    run bash -c 'set -eu; MIGRATE_LIB=1 . "$1"; teardown_stow "$2" "$3"' \
        _ "$REPO/dist/migrate-to-chezmoi.sh" "$TMP" "$FR"
    [ "$status" -eq 0 ]

    [ ! -L "$TMP/.config/git/config" ]              # relative stow link swept
    [ ! -L "$TMP/.local/share/antidote" ]           # absolute link swept
    [ -f "$TMP/.claude/skills/my-local/SKILL.md" ]  # local file untouched
    [ -L "$TMP/.config/foreign" ]                   # foreign symlink untouched
    rm -rf "$FR"
}
