---
name: compact-and-continue
description: seamless, no operator prompt
when_to_use: automatically trigger when work could proceed with a much shorter transcript: (a) you distilled in-context reasoning into an artifact, (b) your upcoming work reads and writes a disjoint file set, (c) the transcript has accumulated stale or unused reads and writes in excess of 5000 SLOC or 50KB text
argument-hint: [optional: user summary guidance]
allowed-tools: mcp__plugin_cac_helper__cac, Bash(${CLAUDE_SKILL_DIR}/scripts/instructions.sh)
user-invocable: false
---

!`${CLAUDE_SKILL_DIR}/scripts/instructions.sh`

Then call `mcp__plugin_cac_helper__cac()`.
