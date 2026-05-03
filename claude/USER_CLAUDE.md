## Posture
Never investigate tool/environment failures by reading source, and don't re-run failed sub-agents, without explicit consent. Always ask me before deferring work.

## Planning
**Plan first, execute on approval, for changes touching more than two files or introducing a new public surface.** Write the plan as text; wait for sign-off before making edits. Skip the plan only when the diff fits in a single sentence — typo, log line, rename.

**Group consecutive plan steps sharing a fileset into chunks during Planning.** Append `(yield after completion)` to the final TodoWrite task of each chunk. After completing a yield-marked task, stop and ask me for /clear or /compact before starting the next one. If the whole plan stays in one chunk, no yields are needed.

**Challenge constraints, don't just work within them.** Raise stale design docs, locked-in library choices, or assumptions forcing a worse design with me before committing to an approach. Architecture is in scope alongside implementation. Insert each yield as its own TodoWrite task.

## Context hygiene
**Mid-plan, between work chunks, check whether the next chunk shifts to a substantially different fileset than the last few touched.** If yes, stop before starting it and offer to /clear or /compact (clear if the prior chunks are done; compact if their context still matters). Offer a one-line handoff summary. The check happens between chunks, not at the start of the plan — if a 6-chunk plan stays in one module the whole way, no reminder is warranted.

When /compact is invoked, always preserve: the list of modified files, any pending TODOs, and the test/build commands relevant to current work.

## Tools and MCPs
**Prefer CLI and built-ins over MCP equivalents.** `gh` over GitHub MCP, native Read/Write/Edit over filesystem MCP, ripgrep/glob over codebase-indexing MCPs for small repos. MCPs earn their slot only when stateful interaction is genuinely required (Playwright is the canonical example).

Before reaching for an MCP tool, ask whether a one-line shell command would do the same work. If yes, use the shell.

## Sub-agents
- **Threshold:** delegate only when the task involves ~10+ file reads, ~3+ independent pieces, or exploration that plausibly compresses 10:1 to its summary. Below that, do it inline — briefing overhead dominates.
- Farm mechanical work to Sonnet/Haiku sub-agents. Prioritize quality per token, not wall-clock time.
- Brief with *why*, not just *what*. Flag adjacent things they might mistake for in-scope — sub-agents take instructions literally.
- Sanity-check output before integrating; unverified work's rework cost usually dominates token savings.
- Don't parallelize work converging on a shared shape — file, interface, schema, or naming.
- On conflict, redo the conflicting slice on integrated HEAD rather than merging cold.

## Testing
**Test through public APIs at real boundaries.** New public surfaces get an integration test first. If a test breaks under a behavior-preserving refactor, it's testing implementation — delete it. Reserve unit tests for tricky internals: state machines, algorithms with non-obvious edge cases. Cover error paths and seams over happy-path variants.

When in doubt, follow the Testing Trophy, not the Test Pyramid.

## Commits
**One logical change per commit in merged history.** Refactors and behavior changes go in separate commits, never mixed. WIP granularity on feature branches is one commit per Planned chunk.

**Default to netdev-style commit messages.** When a commit closes a GitHub issue, end the message body with `Closes #<n>` on its own line.

## Coding Sensibilities
**Linux kernel sensibilities adapted to the language at hand:** small files with single, named concerns; composition over inheritance (Effective Java Item 18); reserve polymorphism for genuine plugin boundaries the way `struct file_operations` is in the kernel — pick vtables carefully; SOLID where classes earn it.

**No NIH syndrome.** Prefer upstream libraries over rolling your own. Ask me to approve any hand-rolled parsers, and present language-appropriate upstream alternatives (nom, pest, lark, ANTLR, etc.) as the first option.

**Strongly typed, not stringly typed.** Parse strings and bytes into typed values at the boundary; internal code takes the types, not raw strings. Newtypes earn their keep by encoding an invariant, not by relabeling a `str`.
