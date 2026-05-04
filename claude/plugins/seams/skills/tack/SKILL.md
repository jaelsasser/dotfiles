---
name: tack
description: Consolidate the active seam at window handoff — render model/action/resume bundle and append a ## Tack section. Forked sub-agent; call after all window TODOs are complete, before /compact or /clear.
context: fork
agent: seamster
model: inherit
effort: low
argument-hint: [optional guidance]
allowed-tools: Bash(tack.sh *), Bash(get-seam.sh *)
---

If `$ARGUMENTS` is non-empty, treat it as guidance steering what the bundle emphasises.

## Active seam path

!`get-seam.sh "${CLAUDE_SESSION_ID}" 2>/dev/null`

If the above is `no upcoming seam`, treat as generic — no handover file, no persistence.

You have the parent conversation as context. Use it to determine mode: **planned** if Plan Mode ran and produced a `## Context Windows` section this session; **freeform** if a seam exists but no plan ran; **generic** if no upcoming seam.

Never ask for clarification — always produce all four sections below.

1. **Suggested model**: `/model <opus|sonnet|haiku>` with a one-line rationale.

2. **Action**:
   - Planned: use the seam's `Action:` field — `/clear` or `/compact`. If `/compact`, reproduce the `#### Next window needs` list verbatim.
   - Freeform: `/compact` — seam has content worth carrying forward.
   - Generic: `/clear`.

3. **Resume prompt**: for the generic mode only — a fenced code block with a single self-contained sentence describing the next task. Omit for planned and freeform; `/seams:stitch` surfaces those after `/compact` or `/clear`.

4. **Caveats**: at most one or two lines, only if something is unresolved (uncommitted work, open thread, missing test).

Then: if a seam is active, persist the bundle. Append a fresh `## Tack` section via tack.sh:

```
printf '## Tack\n```\n<bundle from steps 1–3 verbatim>\n```\n' \
  | tack.sh --sid "${CLAUDE_SESSION_ID}"
```

Finally, scan this conversation for runtime additions the seam didn't already cover. Append a terse `## Tack addendum` section if and only if you find something. Skip if unsure — silence is the default.

No persistence in the generic case.
