---
name: seamster
description: Auto-use in Plan Mode. Defines the `## Context Windows` schema; guides the planner on seam content. Also the persona loaded for forked sub-agents (/seams:tack, /seams:pattern).
---

## Definitions

- **Session**: one Claude Code conversation from start to end.
- **Context window**: one segment of a session between `/compact` or `/clear` boundaries.
- **Seam**: a handover file (`seam-N.md`) written at plan time and consumed at window start. Carries what the next window would otherwise rederive.
- **Cursor**: pointer to the next unconsumed seam. Written by `/seams:pattern`; advanced by `/seams:stitch`.
- **Active seam**: the seam at the current cursor — the one the running window appends chalk and tack to, and the next window reads via `/seams:stitch`.

## Principle

At each window boundary, spend one low-effort pass asking: *what did this window just learn that the next window would otherwise have to rederive?* File paths found, symbols identified, decisions made, external results fetched — pack them into the window's outgoing `### Seam N`.

## Triggers

**1. Post-plan handover.**
ExitPlanMode crosses a window boundary. After the plan is written, `/seams:pattern` materialises each `### Seam N` into a seam file. Every multi-window plan needs at least `### Seam 0` so the first executor window bootstraps cleanly.

**2. Defer planning at a window boundary.**
Set `Needs planning: yes (reason)` when Context Window N+1's design depends on what Context Window N discovers. The execute hook prepends a Plan Mode gate on resume.

**3. Sub-plan re-entry.**
The operating agent hits unforeseen complexity that breaks the parent plan. Re-enter Plan Mode. The sub-plan emits a fresh `## Context Windows` block for the *remaining* windows from the current cursor onward. `/seams:pattern` skips seam files numbered below the prior cursor (no clobber).

## Context Window sizing

Bias windows small — 3–10 file reads, one cohesive concern. Sub-agent delegation: see the global `## Sub-agents` rule.

## Plan-file schema

Write `## Context Windows` as a **terminal** section. Interleave `### Context Window N` (planning metadata + optional executor TODO items) with `### Seam N` (handover notes for context transitions).

`### Context Window N` holds `Model:`, `Goal:`, `Needs planning:`, `Anchors:`, and optional `- [ ]` TODO items for the executor.

`### Seam N` starts with `Recommendation: /compact | /clear`, then notes appropriate to the recommendation: for `/compact`, a `#### Next window needs` list of planner-predicted facts; for `/clear`, brief planner notes (the tack agent writes the rich bundle at runtime).

`### Context Window 0` is **optional** — include it only when the planner has small in-context work to do before the context is cleared (e.g. two files ready to write). Absent means the executor applies `### Seam 0`'s `Recommendation:` directly. Default to `/compact` — plan-window context (exploration, synthesis, rejected approaches) is usually worth carrying forward; use `/clear` only when CW1 bootstraps cleanly from disk.

```
## Context Windows

### Context Window 0 (Plan)           ← optional; only when planner has in-context TODOs
Model: sonnet
Goal: brief description
Needs planning: no
- [ ] write foo.sh
- [ ] write bar.sh
(end with /seams:tack, then apply Seam 0's Recommendation:)

### Seam 0
Recommendation: /compact

#### Next window needs
- planner-predicted fact 1
- planner-predicted fact 2

### Context Window 1
Model: sonnet
Goal: one paragraph, self-sufficient for cold bootstrap after /seams:stitch
Needs planning: yes (one-clause reason) | no
Anchors:
- Plan: ~/.claude/plans/<slug>.md
- Handover: <repo>/.claude/cache/seams/<sid>/seam-0.md

- [ ] Context Window 1 todo item

### Seam 1
Recommendation: /compact

#### Next window needs
- planner-predicted runtime fact 1
- planner-predicted runtime fact 2
```

`/seams:pattern` (forked at ExitPlanMode) materialises each `### Seam N` into `seam-N.md`. `### Context Window N` content stays in the plan file.

## Auto-compact caveat

Auto-compact mid-window only sees content present at that moment. For `/compact` seams, pack `#### Next window needs` with planner-predictable facts. Mid-window discoveries go in `## Chalk` sections via `/seams:chalk`.

## Reference

Mid-flow notes: `/seams:chalk`. Final consolidation: `/seams:tack`. Post-plan materialisation: `/seams:pattern`. Same-session bridge / cross-session fork: `/seams:stitch`.
