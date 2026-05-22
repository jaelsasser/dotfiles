---
name: yield
description: hand back to the operator with summarizer instructions ready to copy-paste into a manual /compact
disable-model-invocation: true
argument-hint: [optional: user summary guidance]
allowed-tools: Bash(${CLAUDE_SKILL_DIR}/scripts/instructions.sh)
---

!`${CLAUDE_SKILL_DIR}/scripts/instructions.sh`

Then emit a single trailing line for the operator to copy-paste:

    /compact strictly conform to the transcript's <summarization-instructions>

Do not call any tool. The operator drives the `/compact` themselves.
