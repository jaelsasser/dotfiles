#!/usr/bin/env bash
DOTFILES_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ln -sf "$DOTFILES_ROOT/claude/USER_CLAUDE.md" "${HOME}/.claude/CLAUDE.md"

# settings.json: dotfile is the canonical view of hooks + permissions only.
# User prefs and any keys the harness writes live in the live ~/.claude/settings.json,
# which is a real file (not symlinked). Re-assert prefs idempotently with jq.
LIVE="$HOME/.claude/settings.json"
TEMPLATE="$DOTFILES_ROOT/claude/settings.json"
TMP=$(mktemp)
jq --argjson hooks "$(jq '.hooks' "$TEMPLATE")" \
   --argjson permissions "$(jq '.permissions' "$TEMPLATE")" \
   '.hooks = $hooks
    | .permissions = $permissions
    | . + {
        showThinkingSummaries: true
      }' "$LIVE" > "$TMP" && mv "$TMP" "$LIVE"
