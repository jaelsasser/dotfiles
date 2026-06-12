---
paths:
  - "**/USER_CLAUDE.md"
---
Project-local override for `home/dot_claude/USER_CLAUDE.md`, on top of the generic `agent-facing` rule: pin this file's always-on budget.

Soft ceiling ~75 lines, hard ceiling ~150 — roughly 100 instruction slots after the harness's own ~50. Past the soft ceiling, a new rule has to displace one already there: cut rules the model now follows by default before adding more.
