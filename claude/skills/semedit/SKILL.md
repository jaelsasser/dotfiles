---
name: semedit
description: Delegate fuzzy-pattern-match-then-edit work to a Sonnet subagent. Absorbs the per-site read-and-decide loop in its own context, edits in place, returns files_touched / changes_by_category / anomalies.
when_to_use: The task contains a per-site judgment loop, where you'll need to read surrounding code at each candidate to decide whether to apply the change.
---

Construct a brief with four distinct fields and dispatch via the Task tool to the `semantic-edit` subagent:

- **pattern** — where to look.
- **judgment** — when each candidate qualifies.
- **transform** — what to do when it qualifies.
- **scope** — file globs.

Keep `pattern`, `judgment`, and `transform` separate. Conflating them makes the subagent apply its own prior on the criteria, which may not match yours. The agent body defines the field semantics canonically; your job is to fill them precisely.

The subagent edits in place and returns three sections: `files_touched`, `changes_by_category`, `anomalies`. Do not pause for per-edit confirmation; review is the diff plus the anomaly list after the agent returns.

**Don't use this** when the change has cross-file invariants — public API renames, schema migrations, anything that needs a test loop. The subagent has neither tests nor cross-file coordination.
