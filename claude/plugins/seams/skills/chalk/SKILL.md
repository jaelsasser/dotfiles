---
name: chalk
description: Drop a mid-flow scratchpad note onto the active seam so the next window doesn't lose it. Unstructured — each call appends a fresh ## Chalk section the next context window can read. Call when a runtime fact has emerged that the next window would otherwise rederive.
argument-hint: [optional: facts to seed]
allowed-tools: Bash(chalk.sh *)
---

A chalk mark is a per-call append to the active seam file — a fresh `## Chalk` section each time. They accumulate as an unstructured scratchpad for the next context window, not a formal data structure. Call when a fact has emerged that the next window won't otherwise have:

- File paths or anchors the next window will reload
- Error classes or symptoms it will keep hitting
- Decisions it must respect (chosen library, rejected approach)
- Runtime values the planner couldn't predict (generated IDs, inferred paths)

Don't chalk anything already in the plan, `git status`, or `git log` — the seam is for what *would* be lost.

If `$ARGUMENTS` is non-empty, treat each as a seed fact. Otherwise curate from in-flow context.

Render as terse bullets — identifiers and anchors verbatim, prose compressed — then append in one call:

```
chalk.sh --sid "${CLAUDE_SESSION_ID}" <<'EOF'
- <bullet1>
- <bullet2>
EOF
```

Respond with one `chalked: <bullet>` line per bullet. If nothing was worth preserving, say `nothing to chalk` and skip the call.
