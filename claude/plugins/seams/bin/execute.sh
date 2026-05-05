#!/usr/bin/env bash
# Wired in hooks/hooks.json under:
#   PostToolUse (matcher ExitPlanMode) — fires once per session at first approved plan exit.
#   SessionStart — fires at session start; injects EXECUTE.md plus active seam/plan context.
# PostToolUse: marker-gated; injects EXECUTE_PLANNER.md via hookSpecificOutput.additionalContext.
# SessionStart: plain stdout (Claude Code injects it directly); ignores session marker.
set -eu

dir="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
EVENT="${1:-PostToolUse}"

INPUT=$(cat)
SESSION_ID=$(jq -r '.session_id // empty' <<<"$INPUT" 2>/dev/null || true)

if [[ "$EVENT" == "SessionStart" ]]; then
  cache_root="${CLAUDE_SEAMS_DIR:-$HOME/.claude/cache/seams}"
  if [[ -n "$SESSION_ID" ]]; then
    sess_dir="$cache_root/$SESSION_ID"
    if [[ -f "$sess_dir/plan" ]]; then
      printf 'Plan: %s\n\n' "$(cat "$sess_dir/plan")"
    fi
    if [[ -f "$sess_dir/cursor" ]]; then
      cursor=$(cat "$sess_dir/cursor")
      seam_file="$sess_dir/seam-${cursor}.md"
      if [[ -f "$seam_file" ]]; then
        cat "$seam_file"
        printf '\n'
      fi
    fi
  fi
  cat "$dir/../EXECUTE.md"
  exit 0
fi

# PostToolUse: emit JSON once per session.
if [[ -n "$SESSION_ID" ]]; then
  MARKER="${TMPDIR:-/tmp}/claude-execute-${SESSION_ID}"
  [[ -f "$MARKER" ]] && exit 0
  : > "$MARKER"
fi

prose_file="$dir/../EXECUTE_PLANNER.md"
[[ -f "$prose_file" ]] || prose_file="$dir/../EXECUTE.md"
jq -n --rawfile prose "$prose_file" \
  '{hookSpecificOutput: {hookEventName: "PostToolUse", additionalContext: $prose}}'
