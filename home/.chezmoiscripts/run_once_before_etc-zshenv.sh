#!/bin/sh
# Point a login zsh at our XDG ZDOTDIR by injecting into the global zshenv.
# zsh reads /etc/zshenv (or /etc/zsh/zshenv) before any per-user file, so this
# is the one bootstrap that cannot itself live under $ZDOTDIR.
#
# Ported from zsh/configure.sh, fixing two latent bugs: the old snippet keyed
# off $XDG_CONFIG_BASE (defined nowhere, so the grep-guard never matched) and
# the fallback called $(cwd) (not a command — meant $(pwd), and dead anyway).
set -eu

[ -f /etc/zsh/zshenv ] && zshenv=/etc/zsh/zshenv || zshenv=/etc/zshenv
snippet='[ -d "${XDG_CONFIG_HOME:-$HOME/.config}/zsh" ] && export ZDOTDIR="${XDG_CONFIG_HOME:-$HOME/.config}/zsh"'

grep -qF "$snippet" "$zshenv" 2>/dev/null && exit 0

printf '%s\n' "$snippet" | sudo tee -a "$zshenv" >/dev/null \
    || ln -sf "$HOME/.config/zsh/.zshenv" "$HOME/.zshenv"
