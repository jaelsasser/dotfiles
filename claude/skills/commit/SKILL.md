---
name: commit
description: Run tests, commit if green, return hash + subject. Refuses on red unless --force.
argument-hint: [--force | optional message hint]
context: fork
agent: signoff
---

Sequence — skip nothing, narrate nothing.

**1. Test gate.** The test command is in your inherited context (CLAUDE.md). Run it via the wrapper:

```
~/.claude/skills/unittest/scripts/run.sh <cmd> [args...]
```

If the wrapper reports a non-zero exit and `$ARGUMENTS` does not contain `--force`:
```
tests: N pass, M fail
failures:
  - <names>
commit: refused (use /commit --force to commit on red)
```
Stop.

**2. Stage.** Check `git status --porcelain` and `git diff --cached --stat`:
- Staged changes exist → proceed.
- Nothing staged, tracked files modified → `git add -u`.
- Nothing staged and nothing modified → emit `commit: refused (nothing to commit)` and stop.

Surface untracked files on a `untracked: ...` line in the briefing if leaving them behind looks like a mistake.

**3. Commit message.** Read CLAUDE.md for commit message conventions (subject format, trailers). If `$ARGUMENTS` contains a non-flag hint, use it as guidance for the subject. Draft the message from `git diff --cached`.

**4. Commit** via `git commit -m`.

**5. Briefing:**
```
tests: N pass, M fail
commit: <sha7> — <subject>
```
