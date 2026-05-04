---
name: pattern
description: Materialise per-seam handover files from the most-recently-written plan. Reads ### Seam N sections and writes seam-N.md + cursor. Forked sub-agent; invoked once at ExitPlanMode by EXECUTE_PLANNER.md.
context: fork
agent: seamster
disable-model-invocation: true
allowed-tools: Read, Write, Bash(ls *), Bash(mkdir -p *)
---

Your job is to read the latest plan file and write one `seam-N.md` per `### Seam N` section.

## Steps

1. Find the plan file: `ls -t ~/.claude/plans/*.md | head -1`. If none, respond "no plan found" and stop.

2. Read the plan file. Locate each `### Seam N` header (where N is an integer). For each:
   - Extract all content between this `### Seam N` and the next `### ` heading (or end of `## Context Windows` section).
   - Demote heading levels by two: `###` → `#`, `####` → `##`, etc.
   - Write the demoted content to `${CLAUDE_SEAMS_DIR:-$HOME/.claude/cache/seams}/${CLAUDE_SESSION_ID}/seam-N.md`.

3. Write the lowest N to `<cache-dir>/cursor` (creates file if absent; does NOT overwrite an existing cursor — sub-plan safe).

4. Write the plan file path to `<cache-dir>/plan`.

5. Respond with a one-line confirmation listing each seam file written and the cursor value, e.g.:
   ```
   pattern: seam-0.md seam-1.md written, cursor=0
   ```

`### Context Window N` sections are the executor's todo seeds — leave them in the plan file, do not write them to seam files.

If `## Context Windows` is absent from the plan, respond "no ## Context Windows section" and stop.
