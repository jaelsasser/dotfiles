#!/usr/bin/env bash
ACTION=${1:--R}
DOTFILES_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DEST="${HOME}/.claude"
mkdir -p "$DEST/commands"
stow -v "$ACTION" claude -t "$DEST" -d "$DOTFILES_ROOT"
