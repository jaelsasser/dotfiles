---
name: stitch
description: Bridge seam content into the current context. Same-session (no arg): reads the active seam, marks it consumed, advances cursor — use after /compact or /clear. Cross-session (prior-sid arg): forks seams from a prior session. One of the two cursor-advancing events.
context: fork
agent: seamster
disable-model-invocation: true
argument-hint: [optional: --dry-run | plan name, seam number, or topic phrase]
allowed-tools: Bash(stitch.sh *), Bash(seams.sh *)
---

Your current session ID is `${CLAUDE_SESSION_ID}`.

## Same-session bridge (no argument)

If `$ARGUMENTS` is empty (or `--dry-run` with no session hint), call:

```
stitch.sh
```

`stitch.sh` reads the cursor's seam file, marks `## Tack` consumed, advances the cursor, and prints the seam content. Respond with the full output verbatim.

Then read the seam's `Recommendation:` line for the suggested context action, and scan `## Tack` for a `/model <M>` directive. If found, append:

```
Recs to apply:
- model: <M>        ← from ## Tack, if present
- context: /compact ← or /clear, from Recommendation: line
```

`## Chalk` sections in the seam are informal mid-flow scratchpad — surface them as-is.

If the output is `no upcoming seam`, respond "No pending seam." and stop.

## Cross-session fork (prior-sid argument)

Available seams from prior sessions:

!`seams.sh`

If the above is `no prior seams found` or `no seams cache`: respond "No prior seams found." and stop.

Pick which session to migrate:
- If `$ARGUMENTS` is `--dry-run` or `dry-run`: identify the session using the rules below, report its `sid:`, `head:`, and pending seam count, then stop — do not call `stitch.sh`.
- If exactly one pending session, take it.
- If `$ARGUMENTS` is a case-insensitive substring match against one session's `plan:` basename or `head:` field, take that session. If multiple sessions match, treat as ambiguous and call `AskUserQuestion` listing each session's `sid:`, `head:`, and `plan:` fields.
- If ambiguous with no `$ARGUMENTS`, call `AskUserQuestion`.

Migrate:

```
Migrating: <source-sid> -> ${CLAUDE_SESSION_ID}
```

```
CLAUDE_SESSION_ID="${CLAUDE_SESSION_ID}" stitch.sh <source-sid>
```

Respond with the full output of `stitch.sh` verbatim. If `stitch.sh` fails, surface its stderr verbatim.

The three cross-session refuse cases:
- `source and destination session IDs are identical` — tell the user to omit the source arg for same-session bridge.
- `<dst> already has seams` — destination already has seams; not a clean fork target.
- `source cursor N is terminal` — source has no further seams.

Then read the `Recommendation:` line and scan `## Tack` for `/model`, and append recs as above.

`## Chalk` sections are informal scratchpad — surface as-is.

## Window execution shape

After surfacing seam content and recs: build a TodoWrite list — one task per substantive step, each planned commit as its own task. End the list with `(/seams:tack after completion)` if further seams remain. Single-window plans skip the tack task entirely.
