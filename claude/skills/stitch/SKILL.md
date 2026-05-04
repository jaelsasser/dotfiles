---
name: stitch
description: Fork a seam from a prior session into the current FRESH session. Reads the source cache read-only, copies seams, advances the dst cursor to the next seam. One of the two cursor-advancing events (the other is /compact). Refuses if invoked in the source session itself. Optional argument hints which plan or seam to prefer.
context: fork
agent: seamster
disable-model-invocation: true
argument-hint: [optional: --dry-run | plan name, seam number, or topic phrase]
allowed-tools: Bash(~/.claude/skills/stitch/scripts/stitch.sh *), Bash(~/.claude/skills/stitch/scripts/seams.sh *)
---

Your current session ID is `${CLAUDE_SESSION_ID}`. `/stitch` requires a **fresh** session — `stitch.sh` refuses when source SID equals destination SID. Available seams from prior sessions:

!`${CLAUDE_SKILL_DIR}/scripts/seams.sh`

If the above is `no prior seams found` or `no seams cache`: respond "No prior seams found." and stop.

Otherwise pick which session to migrate:
- If `$ARGUMENTS` is `--dry-run` or `dry-run`: identify which session would be chosen using the rules below, report its `sid:`, `head:`, and pending seam count, then stop — do not call `stitch.sh`.
- If there is exactly one pending session, take it.
- If `$ARGUMENTS` is non-empty and clearly matches one session's `plan:` path or `head:` line (plan name, seam number, or topic), take that session.
- If multiple sessions remain ambiguous, call `AskUserQuestion` listing each session's `sid:`, `head:`, and `plan:` fields, and ask which session to resume.

Migrate the chosen session. First print a block:

```
Migrating: <source-sid> -> ${CLAUDE_SESSION_ID}
```

Then migrate:

```
CLAUDE_SESSION_ID="${CLAUDE_SESSION_ID}" ${CLAUDE_SKILL_DIR}/scripts/stitch.sh <source-sid>
```

Respond with the full output of `stitch.sh` verbatim — the confirmation line, then the migrated seam content if any follows.

If `stitch.sh` fails, surface its stderr verbatim. The three refuse cases are:
- `source and destination session IDs are identical` — user is in the source session; tell them to start a fresh Claude session and re-invoke.
- `<dst> already has seams` — destination session already has its own materialized seams; not a clean fork target.
- `source cursor N is terminal — no further seams` — source has no further seam to advance into.

Then scan the `--- seam-N.md ---` content you just emitted for directives inside the `## Baste` section: `/model <M>` lines and `/compact` or `/clear` lines. If any are present, append a block like:

```
Recs to apply:
- model: sonnet        ← whatever /model directive said
- context: /compact    ← /compact or /clear, whichever appeared
```

Any `## Tack` sections in the migrated seam are informal scratchpad — surface them as-is but do not treat them as structured directives.

If no Baste directives are present, omit the block.
