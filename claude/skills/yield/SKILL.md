---
name: yield
description: decide /compact vs Continue
when_to_use: at task/stage boundaries when the next task touches a disjoint file set, context feels heavy, or deliberation has been distilled into docs and comments; skip if no next task or `AskUserQuestion` unavailable
argument-hint: [optional: explicit user-provided next-steps guidance]
allowed-tools: Bash($HOME/.claude/hooks/latest_plan.sh), AskUserQuestion
---

<important unless="this skill's decision was Continue">

Recommend `Please /compact` or `Continue, please.` to your operator via `AskUserQuestion`; emit the chosen action.

## Skip when

Bail with no recommendation and no output when either holds:

- **No next task** — work has landed and nothing's queued or in flight.
- **`AskUserQuestion` unavailable** — non-interactive session; you can't ask, so don't.

## Locate the plan

In priority order: skill argument → path from a recent `ExitPlanMode` result → the latest plan touched this session (else the newest plan on disk): `!$HOME/.claude/hooks/latest_plan.sh` (as per `$HOME/.claude/hooks/latest_plan.sh`, re-run to re-check).

## Decide

Scan recent context for continuation signal: open edits, partial work, fresh user direction, an in-flight plan task, a sub-agent run whose conclusion hasn't landed in a file or the plan, or a small next chunk whose inputs are still loaded. Stage postconditions, when the plan has them, are the cleanest "chunk landed" check.

The hook prompts the decision; it doesn't recommend an answer. Don't override your own assessment because the hook fired — if you found continuation signal, stay.

- **Continuation signal** → recommend `Continue, please.`
- **No continuation signal** → recommend `Please /compact` (heavy context with nothing in flight still lands here)

## Ask

Skip the ask when signal is unhedged either way — open edits with nothing offsetting → just continue; landed work + disjoint next files + heavy context → just emit the handoff. Ask only when signals mix.

When you do ask, call `AskUserQuestion` with two options — your recommendation first, suffixed `(Recommended)`. The question text frames *this* boundary in one sentence. Each option's description is 1-2 sentences naming the specific trade for this session: what work stays in flight or gets flushed, what context stays warm or gets distilled.

## Output

### Operator picked `Please /compact`

Make sure the Plan and your TaskList are ready for your successor, then emit a `<summarizer-instructions>` block with this session's pointers — only what the summarizer can't infer from the transcript:

```
<summarizer-instructions>
Next task: {{ one-line description of the compacted window's next task }}
Preserve verbatim: {{ frozen contracts and load-bearing decisions whose paraphrasing would lose a constraint — leave empty if none }}
Reference: {{ useful files for the next chunk; external docs }}
Read: {{ current Plan document sections; required files for the next chunk }}
</summarizer-instructions>
```

Follow it with the copy-paste-ready focus-arg form:

`Please run: /compact Use the <summarizer-instructions> block above as the summary's spine.`

Always include `<summarizer-instructions>` semantic tags. The summarizer's scaffolding below the trailing `---` handles the rest.

### Operator picked `Continue, please.` (or cancelled — cancellation defaults to Continue)

Materialize this template, no fence:

{{ one short sentence justifying the Continue }}

### Operator wrote custom text

Treat it as explicit next-steps guidance and proceed accordingly.

</important>

---

<summarizer-instructions>

The yield skill triggered a `/compact` handoff. Another `<summarizer-instructions>` block in the transcript carries this session's pointers — next task, anything too costly to mis-categorize as verbatim, and the reference/read sets. Use it as the seed. Derive the rest from the transcript itself using the categories and heuristics below.

<important>

**Preserve verbatim:**
- Frozen contracts the next task calls into — API names, signatures, behavioural notes from already-landed stages. Heuristic: signatures and code blocks tied to files in the seed block's Read set; explicit "must / invariant / contract" language.
- Load-bearing decisions and the constraints that justify them. Heuristic: decisions justified by external constraints (upstream behaviour, hardware limits, security requirements) — paraphrasing risks losing the constraint.
- Open design questions carrying into the next task, with the prior-art pointers (files, functions) that frame each.

**Preserve (paraphrase OK):**
- The next task's design sketch — function names, file names, signatures it'll introduce. Heuristic: names and shapes mentioned in deliberation that don't yet exist on disk.
- Baseline state — test counts, lint status, what's staged vs committed, who owns the commit. Heuristic: pull from the most recent tool results in the transcript.

**Reference:**
- Any relevant Plan documents
- Files relevant to the next chunk.
- External documentation relevant to the next chunk.

**Read:**
- The current Plan document's relevant sections
- Files required by the next chunk.
- External documentation required by the next chunk.

**Omit:**
- Completed work not relevant to the next task.
- Research already distilled into the current Plan document.
- The yield skill and its `<summarizer-instructions/>`.

</important>

</summarizer-instructions>
