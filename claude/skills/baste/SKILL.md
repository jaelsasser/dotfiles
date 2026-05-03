---
name: baste
description: Consolidate the active seam at handoff — render bundle (model, action, resume prompt, caveats) and persist a ## Baste section. Idempotent arm; cursor advances at /compact or /stitch, not here. User-triggered before /clear, /compact, or /stitch. ## Baste is written before any ## Tack scratchpad sections in the file.
context: fork
agent: seamster
model: inherit
effort: low
argument-hint: [optional guidance]
allowed-tools: Bash(~/.claude/skills/baste/scripts/baste.sh *)
---

If `$ARGUMENTS` is non-empty, treat it as guidance steering what the bundle emphasizes — e.g., `preserve the migration list`, `flag the open errors`.

## Active seam

!`${CLAUDE_SKILL_DIR}/scripts/get-seam.sh --read "${CLAUDE_SESSION_ID}" 2>/dev/null`

If the above is `no active seam`, treat as generic (no handover file, no persistence). If the content has no `mode: planned-seam` header, treat it as freeform (same rules as generic).

Never ask for clarification — always produce all four sections below.

1. **Suggested model**: `/model <opus|sonnet|haiku>` with a one-line rationale — from the `Model:` field if `mode: planned-seam`, otherwise from current task complexity.

2. **Action**:
   - `mode: planned-seam`: `/clear` if `## Next window needs` is empty; otherwise `/compact` and reproduce that list.
   - Freeform seam: `/compact` — seam has content worth carrying forward.
   - Generic: `/clear`.

3. **Resume prompt**: for the generic mode only — a fenced code block with a single self-contained sentence describing the next task. Omit for `mode: planned-seam` and freeform; `/stitch` surfaces those cross-session and the SessionStart hook surfaces them same-session after `/compact`.

4. **Caveats**: at most one or two lines, only if something is unresolved (uncommitted work, open thread, missing test).

Then: if a seam is active, persist the bundle. Baste is the **idempotent arm** — re-running rewrites the same `## Baste` block in the same seam, always before any `## Tack` scratchpad sections. Cursor advancement happens later, at the next `/compact` (SessionStart) or `/stitch`.

Persist: append a fresh `## Baste` section via baste.sh, replacing any prior one:
```
printf '## Baste\n```\n<bundle from steps 1–3 verbatim>\n```\n' \
  | ~/.claude/skills/baste/scripts/baste.sh \
      --sid "${CLAUDE_SESSION_ID}" --strip-baste
```

Finally, scan this conversation for runtime additions the seam didn't already cover. Append a terse `## Model addendum` section if and only if you find something. Skip if unsure — silence is the default.

No persistence in the `mode: generic` case.
