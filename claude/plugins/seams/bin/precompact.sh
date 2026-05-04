#!/usr/bin/env bash
# Wired in hooks/hooks.json under PreCompact (matcher manual|auto).
# PreCompact does not accept hookSpecificOutput JSON — Claude Code injects
# whatever lands on stdout into the compaction model's context. So cat the
# COMPACT.md prose verbatim, then append the active handover (if any) via
# get-seam.sh. Best-effort: any chain failure exits 0 silently because
# compaction must not be blocked.
set -eu

dir="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"

INPUT=$(cat)
SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty' 2>/dev/null || true)
export CLAUDE_SESSION_ID="${SESSION_ID:-}"

cat "$dir/../COMPACT.md"

HANDOVER=$("$dir/get-seam.sh") || exit 0
echo
cat "$HANDOVER"
