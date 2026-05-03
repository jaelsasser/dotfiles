#!/usr/bin/env bash
DOTFILES_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DEST="${HOME}/.claude"
mkdir -p "$DEST/commands"
stow -R claude -t "$DEST" -d "$DOTFILES_ROOT"
