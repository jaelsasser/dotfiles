---
name: condense
description: seamless compact-and-continue without nagging the operator
when_to_use: automatically trigger when work could proceed with a much shorter transcript: (a) you distilled in-context reasoning into an artifact, (b) your upcoming work reads and writes a disjoint file set, (c) the transcript has accumulated stale or unused reads and writes in excess of 5000 SLOC or 50KB text
argument-hint: [optional: user summary guidance]
allowed-tools: mcp__plugin_cac_server__continue
user-invocable: false
---

!`cat ${CLAUDE_SKILL_DIR}/../INSTRUCTIONS.md`

Then call `mcp__plugin_cac_server__continue()`.
