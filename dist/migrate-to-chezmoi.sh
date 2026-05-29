#!/usr/bin/env bash
# migrate-to-chezmoi.sh — one-shot, idempotent cutover from the old GNU Stow
# layout to chezmoi. Run once on each machine previously installed with the
# retired stow.sh; safe to re-run.
#
# Stow's --no-folding planted a symlink farm: per-file links pointing back into
# this repo. The restructure moved those repo files (git/ -> home/dot_config/git/,
# the antidote submodule -> an external, ...), so most of those links now dangle.
# chezmoi wants to own those paths. This script:
#
#   1. ensures chezmoi + jq are installed,
#   2. sweeps away every symlink under the known XDG targets whose *raw* target
#      points back into this repo (real files and foreign symlinks are never
#      touched), and
#   3. clones the source dir from origin into ~/.local/share/chezmoi, applies it,
#      and registers this dev checkout as the `local` side-load remote.
#
# The claude/cursor trees deploy as a per-entry farm (real dirs + per-file
# symlinks), so chezmoi never RemoveAll's a directory: local-only files sitting
# beside the managed ones survive. The sweep is therefore non-destructive — it
# only removes links this repo planted.
set -eu

# --- teardown_stow <home> <repo> ---------------------------------------------
# Factored out so it can be exercised against a fixture $HOME in tests without
# running the chezmoi apply (which would prompt for sudo on the /etc injection).
#
# A symlink is removed iff its target resolves into <repo>. GNU Stow plants
# *relative* links (../../<repo>/git/config) while the old configure.sh hooks
# planted *absolute* ones (antidote, CLAUDE.md). The restructure left the stow
# links dangling, so realpath/readlink -f can't resolve them — the target is
# normalized *lexically* (no filesystem access), anchored at the link's own
# directory for relative targets. Anchor and <repo> are both logical (`pwd`),
# never physical (`pwd -P`), so a symlinked component of $HOME can't desync them.
teardown_stow() {
    home=$1 repo=$2

    # Lexically collapse . and .. in an absolute path — works on dangling links.
    _norm() {
        _ni=$1 _no=
        OLDIFS=$IFS; IFS=/
        for _nc in $_ni; do
            case $_nc in
                ''|.) ;;
                ..) _no=${_no%/*} ;;
                *) _no=$_no/$_nc ;;
            esac
        done
        IFS=$OLDIFS
        printf '%s' "${_no:-/}"
    }

    _rm_if_into_repo() {
        [ -L "$1" ] || return 0
        _tgt=$(readlink "$1")
        case $_tgt in
            /*) _abs=$_tgt ;;
            *)  _abs="$(cd "$(dirname "$1")" 2>/dev/null && pwd)/$_tgt" ;;
        esac
        case "$(_norm "$_abs")/" in
            "$repo"/*) rm -f "$1" && printf '  unlinked %s\n' "$1" ;;
        esac
    }

    # Per-file farm links under the config/data roots stow populated.
    find "$home/.config" "$home/.claude" -type l 2>/dev/null | while IFS= read -r link; do
        _rm_if_into_repo "$link"
    done

    # Single links the old configure.sh hooks planted directly. (~/.tmux.conf,
    # ~/.tmuxp, ~/.xmonad pointed into ~/.config — not the repo — so they match
    # chezmoi's new targets and need no teardown.)
    _rm_if_into_repo "$home/.local/share/antidote"   # was: -> repo/zsh/antidote
    _rm_if_into_repo "$home/.zshenv"                 # was: rare sudo-failed fallback
}

# --- main --------------------------------------------------------------------
main() {
    repo="$(cd "$(dirname "$0")/.." && pwd)"

    if ! command -v chezmoi >/dev/null 2>&1; then
        if [ "$(uname -s)" = Darwin ] && command -v brew >/dev/null 2>&1; then
            brew install chezmoi
        else
            sh -c "$(curl -fsLS get.chezmoi.io)" -- -b "$HOME/.local/bin"
            export PATH="$HOME/.local/bin:$PATH"
        fi
    fi
    command -v jq >/dev/null 2>&1 || {
        printf 'jq is required (brew install jq / apt-get install jq)\n' >&2
        exit 1
    }

    printf 'Tearing down stow symlinks that point into %s\n' "$repo"
    teardown_stow "$HOME" "$repo"

    # Source dir = a standalone clone at ~/.local/share/chezmoi, decoupled from
    # this dev checkout so edits stage until promoted. Clone from origin; wire the
    # dev checkout in as the `local` remote for side-loading integration -> main.
    src="${XDG_DATA_HOME:-$HOME/.local/share}/chezmoi"
    if [ ! -d "$src/.git" ]; then
        if [ -d "$src" ]; then rmdir "$src"; fi   # drop chezmoi's stale empty source dir
        printf 'Cloning source into %s\n' "$src"
        chezmoi init "$(git -C "$repo" remote get-url origin)"
    fi

    printf 'Applying chezmoi...\n'
    chezmoi apply

    git -C "$src" remote get-url local >/dev/null 2>&1 \
        || git -C "$src" remote add local "$repo"

    cat <<'DONE'

Migration complete. Left untouched (user/harness-local):
  ~/.config/zsh/local.zsh   ~/.profile.local   ~/.claude/settings.local.json
A login zsh injection into /etc/zshenv may have prompted once for sudo.
DONE
}

# Run main only when executed directly; `MIGRATE_LIB=1 source` exposes the
# functions for testing without side effects.
[ "${MIGRATE_LIB:-}" = 1 ] || main "$@"
