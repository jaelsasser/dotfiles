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
   --argjson env "$(jq '.env' "$TEMPLATE")" \
   '.hooks = $hooks | .permissions = $permissions | .env = ((.env // {}) + $env) | del(.mcpServers) | del(.statusLine) | . + {showThinkingSummaries: true}' \
   "$LIVE" > "$TMP" && mv "$TMP" "$LIVE"

# Re-register idempotently so command/args edits propagate on re-stow.
if command -v claude >/dev/null 2>&1; then
    claude plugin install -s user cac@dotfiles >/dev/null 2>&1 || true
fi
