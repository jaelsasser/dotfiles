---
name: compact-and-continue
description: seamless, no operator prompt
when_to_use: automatically trigger when work could proceed with a much shorter transcript: (a) you distilled in-context reasoning into an artifact, (b) your upcoming work reads and writes a disjoint file set, (c) the transcript has accumulated stale or unused reads and writes in excess of 5000 SLOC or 50KB text
argument-hint: [optional: user summary guidance]
allowed-tools: mcp__plugin_cac_helper__cac
user-invocable: false
---

The `mcp__plugin_cac_helper__cac(...)` tool splits your inputs across two surfaces:

- **`/compact` payload** (durable context, preserved verbatim as a user message in the post-compact transcript): `contracts`, `state`, `files`.
- **Agent kickoff** (the agent's first prompt after compaction completes): `next_task` + a structured `read` list rendered as a 'First read:' bullet list.

The summarizer reads the full tool-call args through the transcript, so you don't need to duplicate the next_task framing into `/compact` for summary visibility.

Fill `next_task` (always) plus whichever of `contracts` / `state` / `files` / `read` carry real per-call content. `read` is a list of `{file, lines, reason}` entries — name specific line ranges, never whole files; `reason` is optional. Trust the harness: don't restate what's obvious from `/compact`'s default summary template. Use only the information at hand; never 

**Tool call restrictions are in effect.** You may only:
- Folds implementation-time deviations back into active plan documents.
- Update completed TaskList work.
- Invoke the `mcp__plugin_cac_helper__cac(...)` tool.

No additional tool calls are allow: don't plan the successor's work, don't fumble around in the repo looking for exact line ranges. **Be quick and brief.**
