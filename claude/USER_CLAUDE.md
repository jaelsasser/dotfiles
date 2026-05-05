## Posture
Never investigate tool/environment failures by reading source, and don't re-run failed sub-agents, without explicit consent. Always ask me before deferring work.

## Planning
**Plan work in short Phases** of 3-10 file reads and one cohesive concern. Merge phases modifying heavily-overlapping sets of files when it reduces file re-reads.

**Yield for review after finishing a Phase** so that I can check your work and give feedback before you move on.

**Phases shape commits.** Make opportunistic mid-Phase commits for logical changes only when they don't break the editing flow.

**Model capabilities shape Phases.** Apply your knowledge of what tasks Opus, Sonnet, and Haiku excel when planning Phases. Attach a model recommendation to each phase and split phases when a single model isn't uniformly suited for the task.

**Challenge constraints, don't just work within them.** Raise stale design docs, locked-in library choices, or assumptions forcing a worse design before committing to an approach. Architecture is in scope alongside implementation.

## Context Hygeine
**Flag stale context to the user.** If you're done working with a fileset and will no longer need to refer back to them in detail yield me a turn and suggest that we `/clear` the context window or `/compact <what details to keep, what details to drop>` before carrying on with our task.

**Delegate to sub-agents.** Cadidate tasks for sub-agents spans ~10+ file reads, ~3+ independent pieces, or exploratory work that compresses 10:1 — that's delegation territory, not work to absorb inline. Delegate only when the context window savings outweigh the cost of the brief.

## Testing
**Test through public APIs at real boundaries.** New public surfaces get an integration test first. If a test breaks under a behavior-preserving refactor, it's testing implementation — delete it. Reserve unit tests for tricky internals: state machines, algorithms with non-obvious edge cases. Cover error paths and seams over happy-path variants.

## Commits
**Netdev-style commit messages.** When a commit closes an issue, end the message body with `Closes <issue>` on its own line.

## Coding Sensibilities
**Linux kernel sensibilities adapted to the language at hand:** small files with single, named concerns; composition over inheritance (Effective Java Item 18); reserve polymorphism for genuine plugin boundaries the way `struct file_operations` is in the kernel — pick vtables carefully; SOLID where classes earn it.

**No NIH syndrome.** Prefer upstream libraries over rolling your own. Ask me to approve any hand-rolled parsers, and present language-appropriate upstream alternatives (nom, pest, lark, ANTLR, etc.) as the first option.

**Strongly typed, not stringly typed.** Parse strings and bytes into typed values at the boundary; internal code takes the types, not raw strings. Newtypes earn their keep by encoding an invariant, not by relabeling a `str`.
