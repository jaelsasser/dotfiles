---
name: handoff
description: Format the current design conversation into a pasteable Claude Code handoff brief. Use when asked to "spec this out", "write a brief", "draft a handoff", "write a handoff for Claude Code", or to turn the in-context design discussion into a pasteable engineering brief.
---

## Behaviour

- Output one Markdown document, generated from this conversation, pasteable into a Claude Code session as-is.
- Format prior design work; don't re-run the design conversation, don't ask clarifying questions, don't defer judgment calls back to the operator.
- No frontmatter, no embedded diagrams, no multi-file structure.

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

Convention, not template — names can flex when a brief calls for it; the discrimination below is what doesn't move. Default ordering:

- **Problem** — what's happening now, with a concrete failure-mode example.
- **Desired change** — behavioural outcome, not mechanism.
- **Solution shape** — components and their roles, not their interfaces.
- **Properties the solution must have** — semantic constraints; rationale only where re-litigation is likely.
- **Non-goals** — what's explicitly out of scope.
- **Open questions** — what the engineer decides.
- **Appendix** — worked examples, rationale for non-obvious constraints, ecosystem context; grounding, not action. No implementation hints, signatures, or file layouts.

## The discrimination

The load-bearing call. Every line is *semantic* (a constraint on any valid solution) or *tactical* (an interface/implementation choice a reasonable engineer makes). Semantic lines belong in the brief; tactical lines belong to the engineer. Test: *would a different reasonable engineer make the same call?* Yes → tactical; no → semantic.

| Statement | Verdict |
|---|---|
| Brief carries judgment criteria separately from the transform. | Semantic — constraint on any valid solution. |
| Brief has fields named `pattern`, `judgment`, `transform`. | Tactical — interface choice (YAML, JSON, prose all valid). |
| Trigger fires predictively, before the cost is paid. | Semantic — property of any valid trigger. |
| Trigger description is ~15 words. | Tactical — implementation budget choice. |
| Subagent performs writes, not edit plans. | Semantic — sets the round-trip economics. |
| Subagent's tools are Read, Edit, Glob, Grep. | Tactical — the restricted-action-space principle is semantic; the list isn't. |

Guards against briefs that look right but smuggle implementation detail into *Properties* or *Solution shape*. Run every line through the test before placing it.

## Inline rationale

Preserve *why* only for constraints likely to be re-litigated — ones that look arbitrary in isolation. Default: state the property and move on. Full rationale lives in the Appendix, where Claude Code grounds on demand.

## Length

Soft target: ~one printed page for the critical path (Problem → Open questions). Appendix is uncapped but grounding-only.
