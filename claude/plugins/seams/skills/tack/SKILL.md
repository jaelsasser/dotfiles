---
name: tack
description: Consolidate the active seam at window handoff — render model/recommendation/resume bundle and append a ## Tack section. Forked sub-agent; call after all window TODOs are complete, before /compact or /clear.
context: fork
agent: seamster
model: inherit
effort: low
argument-hint: [optional guidance]
allowed-tools: Bash(tack.sh *), Bash(get-seam.sh *)
---

If `$ARGUMENTS` is non-empty, treat it as guidance that shifts which Caveats are surfaced and how the model rationale is framed; section structure is fixed.

## Active seam path

!`get-seam.sh "${CLAUDE_SESSION_ID}" 2>/dev/null`

If the above is `no upcoming seam`, treat as generic — no handover file, no persistence.

You have the parent conversation as context. Use it to determine mode: **planned** if Plan Mode ran and produced a `## Context Windows` section this session; **freeform** if a seam exists but no plan ran; **generic** if no upcoming seam.

Never ask for clarification — always produce the sections that apply to the active mode:

| Mode | Sections |
|---|---|
| planned | 1, 2, 4 |
| freeform | 1, 2, 4 |
| generic | 1, 2, 3, 4 |

1. **Suggested model**: `/model <opus|sonnet|haiku>` with a one-line rationale.

2. **Recommendation**: frame as a suggestion the user may override.
   - Planned: reproduce the seam's `Recommendation:` field — `/clear` or `/compact`. If `/compact`, reproduce the `#### Next window needs` list verbatim.
   - Freeform: `/compact` — seam has content worth carrying forward.
   - Generic: `/clear`.

3. **Resume prompt**: for the generic mode only — a fenced code block with a single self-contained sentence describing the next task. Omit for planned and freeform; `/seams:stitch` surfaces those after `/compact` or `/clear`.

4. **Caveats**: at most one or two lines, only if something is unresolved (uncommitted work, open thread, missing test).

Then: if a seam is active, persist the bundle. `tack.sh` prepends `## Tack` automatically — pipe only the content:

```
printf '```\n<bundle from sections 1–4 verbatim>\n```\n' \
  | tack.sh --sid "${CLAUDE_SESSION_ID}"
```

Finally, scan this conversation for runtime additions the seam didn't already cover. If found, include them as a terse addendum within the same `printf` before piping to `tack.sh` — bundle and addendum land in a single `## Tack` block. Skip if unsure — silence is the default.

No persistence in the generic case.
