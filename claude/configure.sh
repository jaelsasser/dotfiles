#!/usr/bin/env bash
DOTFILES_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ln -sf "$DOTFILES_ROOT/claude/USER_CLAUDE.md" "${HOME}/.claude/CLAUDE.md"

# settings.json is a live file (not symlinked) — merge hooks + permissions
# idempotently so user prefs and harness-written keys are preserved.
LIVE="$HOME/.claude/settings.json"
TEMPLATE="$DOTFILES_ROOT/claude/settings.json"
TMP=$(mktemp)
jq --argjson hooks "$(jq '.hooks' "$TEMPLATE")" \
   --argjson permissions "$(jq '.permissions' "$TEMPLATE")" \
   '.hooks = $hooks | .permissions = $permissions | . + {showThinkingSummaries: true}' \
   "$LIVE" > "$TMP" && mv "$TMP" "$LIVE"

claude plugin install "$HOME/.claude/plugins/seams" 2>/dev/null || true
