---
name: spec
description: Produces a PM-tier brief that formats the current design conversation into a handoff document ready to paste into a Claude Code session. Use when the operator asks to "spec this out", "write a brief", "write a handoff for Claude Code", "draft a handoff", "turn this into a brief for Claude Code", or similar phrasing for converting the in-context design discussion into a pasteable engineering brief.
---

## Behaviour

- Output is one Markdown document, generated from this conversation, pasteable into a Claude Code session as-is.
- Format prior design work; don't re-run the design conversation; don't ask the operator clarifying questions.
- Apply the discrimination below autonomously. Don't defer judgment calls back to the operator.
- No frontmatter on the output, no embedded diagrams, no multi-file structure.

## Opening preamble

Every brief opens with this exact block. Fixed — no rewording, no reordering:

```
# <Feature> — brief

> Role: implementing engineer
> Scope: authoritative
> Open questions: your call
> Output: working code, not a spec
```

## Sections

Convention, not template. Section names can shift when a brief calls for it; the discrimination below is what doesn't move.

Default ordering:

- **Problem** — what's happening now, with a concrete failure-mode example.
- **Desired change** — behavioural outcome, not mechanism.
- **Solution shape** — components and their roles, not their interfaces.
- **Properties the solution must have** — semantic constraints, with rationale preserved only where re-litigation is likely.
- **Non-goals** — what's explicitly out of scope.
- **Open questions** — what the engineer decides.
- **Appendix** — examples, rationale for non-obvious constraints, ecosystem context.

Rigidity ranking: the opening preamble is fixed; section names flex; the discrimination is invariant.

## The discrimination

The load-bearing call. Every line in the brief is either *semantic* (constraint on any valid solution) or *tactical* (interface/implementation choice a reasonable engineer makes). Semantic lines belong in the brief; tactical lines belong to the implementing engineer.

The test: *would a different reasonable engineer make the same call?* If yes, it's tactical. If no — if any valid solution must have this property — it's semantic.

Worked examples:

| Statement | Verdict | Why |
|---|---|---|
| The brief must carry judgment criteria separately from the transform. | Semantic | Constraint on any valid solution. |
| The brief has fields named `pattern`, `judgment`, `transform`. | Tactical | Interface choice — YAML, JSON, prose template all valid. |
| The trigger must fire predictively, before the cost is paid. | Semantic | Property of any valid trigger. |
| The trigger description should be ~15 words. | Tactical | Implementation budget choice. |
| The subagent performs writes, not edit plans. | Semantic | Determines the round-trip economics. |
| The subagent's tools are Read, Edit, Glob, Grep. | Tactical | The principle (restricted action space) is semantic; the tool list is an interface choice. |

The failure mode this guards against: briefs that look right but smuggle implementation details into "Properties" or "Solution shape." Run every line through the test before placing it.

## Inline rationale

Preserve *why* only for constraints likely to be re-litigated — ones that look arbitrary in isolation. Default is to state the property and move on. Full rationale lives in the Appendix where Claude Code can ground itself on demand.

## Appendix

For context Claude Code might want but doesn't strictly need to act:

- Worked examples.
- Rationale for non-obvious constraints.
- Relationship to existing patterns / ecosystem context.

Not in the appendix: implementation hints, "here's what I'd do" sketches, signatures, schemas, file layouts.

## Length

Soft target: ~one printed page for the critical path (Problem → Open questions). Appendix is uncapped but grounding-only.
