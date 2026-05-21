---
name: yield
description: hand back to the operator with summarizer instructions ready to copy-paste into a manual /compact
disable-model-invocation: true
argument-hint: [optional: user summary guidance]
---

!`cat ${CLAUDE_SKILL_DIR}/../INSTRUCTIONS.md`

Then emit a single trailing line for the operator to copy-paste:

    /compact strictly conform to the transcript's <summarization-instructions>

Do not call any tool. The operator drives the `/compact` themselves.
