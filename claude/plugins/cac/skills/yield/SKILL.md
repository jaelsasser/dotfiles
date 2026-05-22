---
name: yield
description: hand back to the operator with summarizer instructions ready to copy-paste into a manual /compact
disable-model-invocation: true
argument-hint: [optional: user summary guidance]
---

Compose a copy-pasteable `/compact` line for the operator. Operator copy-paste is one shot — the MCP path's split into `/compact` durable-context + post-compact kickoff isn't practical for manual driving. Fold everything into one fat `/compact` payload structured as the next-task agent's brief:

    /compact NEXT TASK: <one-paragraph framing>

    FROZEN CONTRACTS: <interfaces, signatures, behavioural notes from landed stages — reproduced verbatim so the agent doesn't have to re-read source>

    STATE: <baseline at compaction: what passes, what's staged, plan deviations folded, names/shapes only in deliberation>

    FILES: <files / plan docs / external docs the next agent should know exist — path/URL only>

    READ: <specific line ranges the next agent should read on resume — never whole files>

Omit any section with no real content. Cite specific files and line ranges. Do not call any tool — the operator drives the `/compact` themselves.
