#!/usr/bin/env bash
# seamster.sh — PostToolUse:ExitPlanMode hook (fires only on user approval).
# Reads session_id from stdin JSON; materializes each ## Seams subsection
# into <repo>/.claude/cache/seams/<sid>/seam-N.md and writes the cursor file.
# When ## Seams is absent, seeds a default seam-1.md (unless a cache already
# exists for this session — sub-plan case). Always exits 0.
set -u

print_agents() {
  cat <<'AGENTS'
seamster.sh: PostToolUse:ExitPlanMode hook. Reads newest ~/.claude/plans/*.md,
  materializes each ## Seams subsection into <repo>/.claude/cache/seams/<sid>/seam-N.md,
  writes cursor to the lowest seam number, then strips ## Seams from the plan.
  When ## Seams absent and no prior cache: seeds a default seam-1.md.
  Sub-plan safe: preserves seams below prior cursor. Always exits 0.
AGENTS
}

print_help() {
  cat <<'HELP'
seamster.sh — PostToolUse:ExitPlanMode hook (fires only on user approval).
Materializes per-seam handover seeds from the most-recently-written
~/.claude/plans/*.md file into <repo>/.claude/cache/seams/<sid>/, then strips the
`## Seams` section from the plan file so subsequent re-reads don't pay for
already-materialized content.

Plan-file schema (planner writes during Plan Mode, terminal section)
  ## Seams

  ### Seam N → N+1
  Model: <opus|sonnet|haiku>
  Needs planning: <yes (one-clause reason) | no>
  Goal: <one-paragraph window-N+1 objective — self-sufficient for /clear>
  Anchors:
  - Plan: ~/.claude/plans/<slug>.md
  - Handover: <repo>/.claude/cache/seams/<sid>/seam-<N>.md
  #### Next window needs
  - <planner-predicted runtime fact>

  ### Seam N+1 → N+2
  ...

  `## Seams` is a TERMINAL section. Everything below it belongs to the seeds
  block. Each `### Seam N → ...` starts a new per-seam handover. Heading levels
  in seeds get demoted by two when materialized (so `### Seam` → `# Seam` and
  `#### Next window needs` → `## Next window needs`).

Behavior
  1. Read session_id from stdin JSON.
  2. plan_file = ls -t ~/.claude/plans/*.md | head -1.
  3. If `## Seams` present: extract each seam, honour prior cursor (sub-plan
     safe — skips seams below prior cursor; only rewrites cursor on fresh cache).
  4. If `## Seams` absent and no prior cache: seed a default seam-1.md.
  5. Write plan file path to <repo>/.claude/cache/seams/<sid>/plan.
  6. Atomic-rewrite plan file: strip `## Seams` and below (when present).

Sub-plan safety
  When a cache already exists (cursor file present), new seams are only written
  for numbers >= prior_cursor. Seams below prior_cursor are untouched. The cursor
  file is not overwritten — the sub-plan leaves the cursor where it is.

Posture
  Best-effort. ALWAYS exits 0 — never blocks ExitPlanMode. Errors print
  to stderr but don't fail the tool call.
HELP
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  print_help
  exit 0
elif [[ "${1:-}" == "--agents" ]]; then
  print_agents
  exit 0
elif [[ -n "${1+set}" ]]; then
  CLAUDE_SESSION_ID="$1"
else
  INPUT=$(cat)
  CLAUDE_SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty' 2>/dev/null || true)
  [[ -n "$CLAUDE_SESSION_ID" ]] || { echo "seamster: no session_id in hook input" >&2; exit 0; }
fi

plans_dir="$HOME/.claude/plans"
plan_file=$(ls -t "$plans_dir"/*.md 2>/dev/null | head -n1)
[[ -n "${plan_file}" && -r "$plan_file" ]] || exit 0

CACHE_ROOT="${CLAUDE_SEAMS_DIR:-$HOME/.claude/cache/seams}"
cache="$CACHE_ROOT/$CLAUDE_SESSION_ID"
mkdir -p "$cache"

# Always record the plan file path for /stitch and resume hooks.
printf '%s\n' "$plan_file" > "$cache/plan"

has_seams=0
grep -q '^## Seams[[:space:]]*$' "$plan_file" && has_seams=1

if [[ "$has_seams" -eq 0 ]]; then
  # Single-window plan: seed a default Seam 1 only when no prior cache exists.
  if [[ ! -f "$cache/cursor" ]]; then
    cat > "$cache/seam-1.md" <<SEED
# Seam 0 → 1
Model: inherit
Goal: Execute the plan.
Anchors:
- Plan: ${plan_file}

## Next window needs
- (single-window plan; seamster wrote no planner-supplied predictions)
SEED
    printf '1\n' > "$cache/cursor"
  fi
  exit 0
fi

# Multi-seam plan: read prior cursor (0 = no prior cache / fresh).
prior_cursor=$(cat "$cache/cursor" 2>/dev/null || echo "")

awk -v cache="$cache" -v prior_cursor="$prior_cursor" '
  BEGIN { in_seeds = 0; out = ""; min_seam = -1 }
  /^## Seams[[:space:]]*$/ { in_seeds = 1; next }
  in_seeds && /^### Seam [0-9]+/ {
    if (out != "") close(out)
    n = $3 + 0
    # Skip seams below prior cursor (sub-plan safety).
    if (prior_cursor != "" && n < prior_cursor + 0) {
      out = ""
      next
    }
    if (min_seam < 0 || n < min_seam) min_seam = n
    out = cache "/seam-" n ".md"
  }
  in_seeds && out != "" {
    line = $0
    if (line ~ /^#{3,}/) sub(/^##/, "", line)
    print line > out
  }
  END {
    if (out != "") close(out)
    # Only write cursor on fresh cache (no prior_cursor).
    if (min_seam >= 0 && prior_cursor == "") print min_seam > (cache "/cursor")
  }
' "$plan_file" || { echo "seamster: awk extraction failed" >&2; exit 0; }

tmp=$(mktemp)
trap 'rm -f "$tmp"' EXIT
awk '/^## Seams[[:space:]]*$/ {exit} {print}' "$plan_file" > "$tmp" \
  || { echo "seamster: plan strip failed" >&2; exit 0; }
mv "$tmp" "$plan_file"
trap - EXIT

exit 0
