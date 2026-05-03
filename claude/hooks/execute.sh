#!/usr/bin/env bash
# Wired in claude/settings.json under:
#   PostToolUse (matcher ExitPlanMode) — fires once per session at first approved plan exit
#   SessionStart (matcher compact|clear) — fires after every /compact or /clear to re-arm
# Injects execution-phase guidance from claude/EXECUTE_PLANNER.md (planner-just-out) or
# claude/EXECUTE.md (cold executor after /compact or /clear).
#
# PostToolUse uses the hookSpecificOutput.additionalContext schema (shared with
# PreToolUse); SessionStart does not and instead consumes plain stdout.

EVENT="${1:-PostToolUse}"
dir="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
EXECUTE_PATH="$dir/../EXECUTE.md"
EXECUTE_PLANNER_PATH="$dir/../EXECUTE_PLANNER.md"
INPUT=$(cat)

# Factored as a function so verification can call it directly without
# standing up the full SessionStart hook context.
prepend_subplan_gate() {
  local handover="$1"
  local prompt="$2"
  local subplan gate
  subplan=$(awk '/^Needs planning:/ { sub(/^Needs planning:[[:space:]]*/, ""); print; exit }' "$handover")
  if [[ "$subplan" =~ ^yes[[:space:]]*\((.+)\)[[:space:]]*$ ]]; then
    gate="Enter Plan Mode first to resolve: ${BASH_REMATCH[1]}"
  elif [[ "$subplan" =~ ^yes($|[[:space:]]) ]]; then
    gate="Enter Plan Mode first to resolve the deferred design gap."
  else
    printf '%s' "$prompt"
    return 0
  fi
  printf '%s\n\n%s' "$gate" "$prompt"
}

if [[ "$EVENT" == "PostToolUse" ]]; then
  SESSION_ID=$(jq -r '.session_id // empty' <<<"$INPUT")
  if [[ -n "$SESSION_ID" ]]; then
    MARKER="${TMPDIR:-/tmp}/claude-execute-${SESSION_ID}"
    [[ -f "$MARKER" ]] && exit 0
    : > "$MARKER"
  fi
  prose_file="$EXECUTE_PLANNER_PATH"
  [[ -f "$prose_file" ]] || prose_file="$EXECUTE_PATH"
  jq -n --rawfile prose "$prose_file" \
    '{hookSpecificOutput: {hookEventName: "PostToolUse", additionalContext: $prose}}'
else
  # SessionStart (compact|clear): if the prior session yielded, the active
  # handover carries an unconsumed `## Baste` section. Inject it ahead of
  # EXECUTE.md, then soft-remove by renaming the heading to `(consumed)` so
  # a follow-on /clear mid-window doesn't re-fire.
  SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty' 2>/dev/null || true)
  export CLAUDE_SESSION_ID="${SESSION_ID:-}"

  if HANDOVER=$("$dir/get-seam.sh" 2>/dev/null); then
    # Surface parent plan path if recorded at materialization time.
    PLAN_FILE=""
    CACHE_DIR=$(dirname "$HANDOVER")
    [[ -f "$CACHE_DIR/plan" ]] && PLAN_FILE=$(cat "$CACHE_DIR/plan")
    PROMPT=$(awk '
      /^## Baste[[:space:]]*$/ { in_section = 1; next }
      in_section && /^## / { in_section = 0 }
      in_section { print }
    ' "$HANDOVER")
    if [[ -n "$PROMPT" ]]; then
      PROMPT=$(prepend_subplan_gate "$HANDOVER" "$PROMPT")
      [[ -n "$PLAN_FILE" ]] && printf 'Plan: %s\n\n' "$PLAN_FILE"
      printf '%s\n\n---\n\n' "$PROMPT"
      cat "$EXECUTE_PATH"
      tmp=$(mktemp)
      sed 's|^## Baste[[:space:]]*$|## Baste (consumed)|' \
        "$HANDOVER" > "$tmp" && mv "$tmp" "$HANDOVER" || rm -f "$tmp"
      # /compact is a seam: advance cursor to the next existing seam.
      cursor=$(cat "$CACHE_DIR/cursor" 2>/dev/null || echo "")
      if [[ -n "$cursor" ]]; then
        for next in "$CACHE_DIR"/seam-*.md; do
          [[ -f "$next" ]] || continue
          n=$(basename "$next" .md); n="${n#seam-}"
          if (( n > cursor )); then
            if [[ -z "${nm:-}" ]] || (( n < nm )); then nm=$n; fi
          fi
        done
        [[ -n "${nm:-}" ]] && echo "$nm" > "$CACHE_DIR/cursor"
      fi
      exit 0
    fi
  fi
  cat "$EXECUTE_PATH"
fi
