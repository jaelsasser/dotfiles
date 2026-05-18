---
name: yield
description: decide /compact vs Continue
when_to_use: task/phase boundaries, after long agent runs, when context feels heavy
argument-hint: [optional: explicit user-provided next-steps guidance]
allowed-tools: Bash($HOME/.claude/hooks/latest_plan.sh)
---

<important unless="the conversation continues after this read">

Decide between `/compact` and `Continue.` by attention-scanning your recent context.

## Locate the plan

In priority order: skill argument → path from a recent `ExitPlanMode` result → the latest plan on disk: `!$HOME/.claude/hooks/latest_plan.sh` (as per `$HOME/.claude/hooks/latest_plan.sh`, re-run to re-check). Phase postconditions, when present, give a concrete "chunk landed" check.

## Decide

Scan recent context for continuation signal: open edits, partial work, fresh user direction, an in-flight plan task, a sub-agent run whose conclusion hasn't landed in a file or the plan.

- **Continuation signal** → `Continue.`
- **No continuation signal** → `/compact` (heavy context with nothing in flight still lands here)

## Output

**`/compact`** — fenced prose, paste-ready as the `/compact` argument:

	/compact <summarizer-instructions>Summarize for {{ the compacted context window's next task }}. Preserve {{ files relevant to next chunk }}, {{ decisions or contracts to preserve }}, and {{ plan-doc path if any }}. Omit {{ work whose result landed in files }}, {{ research already distilled into the plan or decision }}, {{ stale exploratory tool runs }}.</summarizer-instructions>

**`Continue.`** — no fence; one short sentence naming the active work or signal against yielding.

</important>
