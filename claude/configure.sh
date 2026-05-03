#!/usr/bin/env bash
DOTFILES_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ln -sf "$DOTFILES_ROOT/claude/USER_CLAUDE.md" "${HOME}/.claude/CLAUDE.md"
