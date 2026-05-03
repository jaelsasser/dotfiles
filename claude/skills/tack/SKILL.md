---
name: tack
description: Drop a scratchpad note onto the active seam so the next /compact or /clear doesn't lose it. Unstructured — each call appends a fresh ## Tack section the next context window can read. Call when you've spotted a runtime fact (file path, error class, anchor, decision) that should survive the next context reset.
argument-hint: [optional: facts to seed]
allowed-tools: Bash(~/.claude/skills/tack/scripts/tack.sh *)
---

A tack is a per-call append to the active seam file — a fresh `## Tack` section each time. They accumulate as an unstructured scratchpad for the next context window, not a formal data structure. Call when a fact has emerged that the next window won't otherwise have:

- File paths or anchors the next window will reload
- Error classes or symptoms it will keep hitting
- Decisions it must respect (chosen library, rejected approach)
- Runtime values the planner couldn't predict (generated IDs, inferred paths)

Don't tack anything already in the plan, `git status`, or `git log` — the seam is for what *would* be lost.

If `$ARGUMENTS` is non-empty, treat each as a seed fact. Otherwise curate from in-flow context.

Render as terse bullets — identifiers and anchors verbatim, prose compressed — then append in one call:

```
${CLAUDE_SKILL_DIR}/scripts/tack.sh --sid "${CLAUDE_SESSION_ID}" <<'EOF'
- <bullet1>
- <bullet2>
EOF
```

Respond with one `tacked: <bullet>` line per bullet. If nothing was worth preserving, say `nothing to tack` and skip the call.
