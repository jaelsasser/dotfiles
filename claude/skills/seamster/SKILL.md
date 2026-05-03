---
name: seamster
description: Auto-use in Plan Mode. Defines the `## Seams` schema; materializes per-seam handover seeds at ExitPlanMode for cold-executor handoff.
---

## Principle

At each window boundary, spend one low-effort pass asking: *what did this window just learn that the next window would otherwise have to rederive?* File paths found, symbols identified, decisions made, external results fetched — pack them into the seam. The seam file is that seed.

## Triggers

**1. Post-plan handover (Seam 0 → 1).**
ExitPlanMode always crosses a seam. Even single-window plans emit a Seam 1 so the executor starts fresh without the planner's reasoning overhead. Pack: files read, symbols identified, decisions made (and why), external inputs (web/MCP results), and any unknowns the executor must NOT re-investigate from scratch. `seamster.sh` materializes a default Seam 1 when no `## Seams` section exists.

**2. Defer planning at a seam.**
Set `Needs planning: yes (reason)` when window N+1's design depends on what window N discovers. Plan with the discovery — cheaper than guessing now. The execute hook prepends a Plan Mode gate on resume.

**3. Sub-plan re-entry.**
The operating agent hits unforeseen complexity that breaks the parent plan. Re-enter Plan Mode. The sub-plan sees the parent plan file (surfaced in resume context) plus the active seam's bastes, and emits a fresh `## Seams` block for the *remaining* seams from the current cursor onward. `seamster.sh` preserves seams numbered below the lowest re-emitted seam (no clobber).

## Window sizing

Bias windows small — 3–10 file reads, one cohesive concern. Single-window plans need no `## Seams` section, but `seamster.sh` still seeds Seam 1. Sub-agent delegation: see the global `## Sub-agents` rule.

## Plan-file schema

Write `## Seams` as a **terminal** section; everything below it is per-seam seed content. Each seam gets a `### Seam N → N+1` subsection:

- `Model:` `opus` / `sonnet` / `haiku`
- `Needs planning:` `yes (one-clause reason)` or `no`
- `Goal:` one paragraph, self-sufficient for cold `/clear` bootstrap
- `Anchors:` plan path + handover path (`<repo>/.claude/cache/seams/<sid>/seam-<N>.md`)
- `#### Next window needs` — planner-predicted runtime facts. Empty ⇒ `/clear` is safe; populated ⇒ `/compact` and the list is the preservation target.

`seamster.sh` materializes each subsection into the per-session cache and strips `## Seams` from the plan on ExitPlanMode.

## Auto-compact caveat

Auto-compact mid-window only sees content present when crossing a seam (`tack.sh` appends at seam-crossing via `/tack`). Pack `#### Next window needs` with planner-predictable facts when a window is long-running.

## Reference

Executor-side tack mechanics: see `/tack`. Final consolidation: see `/baste`. Cross-/clear migration: see `/stitch`.
