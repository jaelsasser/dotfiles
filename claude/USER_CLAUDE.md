## Posture
Never investigate tool/environment failures by reading source, and don't re-run failed sub-agents, without explicit consent. Always ask me before deferring work.

## Planning
**Enter Plan Mode for changes touching more than two files or introducing a new public surface.** Skip it only when the diff fits in a single sentence — typo, log line, rename.

## Context hygiene
**Mid-plan, at each seam, check whether the next window's fileset differs substantially from the current one.** If yes, stop before starting it and offer to /clear or /compact (clear if the prior windows are done; compact if their context still matters). Offer a one-line handoff summary. The check happens at seams, not at the start of the plan — if a 6-window plan stays in one module the whole way, no reminder is warranted.

## Tools and MCPs
**Prefer CLI and built-ins over MCP equivalents.** `gh` over GitHub MCP, native Read/Write/Edit over filesystem MCP, ripgrep/glob over codebase-indexing MCPs for small repos. Before reaching for an MCP tool, ask whether a one-line shell command would do the same — MCPs earn their slot only when stateful interaction is genuinely required (Playwright is the canonical example).

## Sub-agents
Delegation decisions live in the plan. If a seam in execution turns out to span ~10+ file reads, ~3+ independent pieces, or exploration compressing 10:1, surface the divergence — that's delegation territory, not work to absorb inline.

## Testing
**Test through public APIs at real boundaries.** New public surfaces get an integration test first. If a test breaks under a behavior-preserving refactor, it's testing implementation — delete it. Reserve unit tests for tricky internals: state machines, algorithms with non-obvious edge cases. Cover error paths and seams over happy-path variants.

When in doubt, follow the Testing Trophy, not the Test Pyramid.

## Commits
**Netdev-style commit messages.** When a commit closes an issue, end the message body with `Closes <issue>` on its own line. Commit boundaries are decided in the plan and surfaced as TodoWrite items.

## Coding Sensibilities
**Linux kernel sensibilities adapted to the language at hand:** small files with single, named concerns; composition over inheritance (Effective Java Item 18); reserve polymorphism for genuine plugin boundaries the way `struct file_operations` is in the kernel — pick vtables carefully; SOLID where classes earn it.

**No NIH syndrome.** Prefer upstream libraries over rolling your own. Ask me to approve any hand-rolled parsers, and present language-appropriate upstream alternatives (nom, pest, lark, ANTLR, etc.) as the first option.

**Strongly typed, not stringly typed.** Parse strings and bytes into typed values at the boundary; internal code takes the types, not raw strings. Newtypes earn their keep by encoding an invariant, not by relabeling a `str`.
