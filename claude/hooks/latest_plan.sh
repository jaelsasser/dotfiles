#!/usr/bin/env bash
# Print the plan most recently touched in this Claude Code session, falling
# back to the globally-newest plan if no session-scoped match is found.
set -u

PLANS_DIR="$HOME/.claude/plans"
SESSION_FILE="$HOME/.claude/sessions/$PPID.json"

fallback() { ls -t "$PLANS_DIR"/*.md 2>/dev/null | head -1; exit 0; }

[[ -r "$SESSION_FILE" ]] || fallback
read -r CWD SID < <(jq -r '[.cwd, .sessionId] | @tsv' "$SESSION_FILE" 2>/dev/null) || fallback
[[ -n "${CWD:-}" && -n "${SID:-}" ]] || fallback

# Claude Code encodes the project directory by replacing both '/' and '.' with '-'.
PROJ=$(printf '%s' "$CWD" | tr './' '--')
TRANSCRIPT="$HOME/.claude/projects/$PROJ/$SID.jsonl"
[[ -r "$TRANSCRIPT" ]] || fallback

PLAN=$(jq -r --arg dir "$PLANS_DIR/" '
  .toolUseResult? // empty
  | .. | objects | .filePath?
  | select(type == "string" and startswith($dir))
' "$TRANSCRIPT" 2>/dev/null | tail -1)

[[ -n "$PLAN" && -f "$PLAN" ]] || fallback
printf '%s\n' "$PLAN"
