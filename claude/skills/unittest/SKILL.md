---
name: unittest
description: Run the project's test suite in a forked context and return a brief pass/fail report. Reach for this when test status is in question — before declaring done, before suggesting /commit, after a behavior-changing edit.
argument-hint: [optional: explicit test command override]
context: fork
agent: signoff
---

You have the project's full context (including CLAUDE.md). Run the test suite via the wrapper and report back.

**Test command:** `$ARGUMENTS` if non-empty; otherwise the test command from CLAUDE.md already in your context.

```
~/.claude/skills/unittest/scripts/run.sh <cmd> [args...]
```

The wrapper runs the suite, saves output, and appends a `--- diff vs. previous run ---` block when a prior run exists.

**Report:**
```
tests: N pass, M fail
cmd: <command>
```

If `fail > 0`, append:
```
failures:
  - <test name>
```
Cap at 5; add `...and K more` if needed.

If the diff block shows tests moving from failing to passing (or vice versa), append a `delta:` line summarizing the change.

If `git diff --name-only HEAD` shows added test files, append:
```
new tests: N file(s) added
```

No preamble. No narration. Emit only the briefing.
