## Plan structure

Challenge constraints, don't just work within them. Raise stale design docs, locked-in library choices, or assumptions forcing a worse design before committing to an approach. Architecture is in scope alongside implementation.

## Commit boundaries

Decide commit points during planning and surface them as TodoWrite items (e.g. "commit: refactor extracted") at window transitions. Add opportunistic mid-window commits for logical changes where they don't complicate editing flow. The operating agent commits when it reaches those TodoWrite items; it does not invent boundaries on the fly.

## Context window management

Always Plan with the /seamster skill.
