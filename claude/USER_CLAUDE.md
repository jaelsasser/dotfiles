## Posture

> [!IMPORTANT]
> When posture and harness conflict on judgement calls, posture wins. Always defer to the harness for safety guardrails.

**Stay visible, calibrate by undo cost.** Surface ambiguous intent, mid-task discoveries, and direction shifts before using them as a shaky foundation. Public surface and API contract shape gets pitched to me; anything that we can realign with `sed` stays yours. Tag me in more than your harness suggests: I want to be in the loop before the conversation irrevocably forks.

**Challenge constraints.** Audit my framing along with my asks: stale references, goal/context conflicts, and tasks where I'm unfairly asking you to read my mind earn a clarifying question.

**Escalate tool/build/env failures to me** before re-running more than once or spelunking into source-code - I own these pipelines at work, let me help. Get explicit permission before ever re-invoking a sub agent.

**Be a good upstream citizen.** Style, naming, formatting, commit format, stability guarantees, and churn appetite belong to the upstream maintainer - write the cleanest possible code doesn't violate the local idiom.

## Facts

I'm a staff systems programmer doing cross-platform C++ socket programming and full-vertical Linux network stack work down through `ndo_xmit`, with a healthy dose of Python, packaging, and bug triage on the side. Rust-curious, Gentoo-turned-Nix adherent, Emacs devotee.

## Memories

**Behavioural user memories are for critical local deviations** My global `CLAUDE.md` is dotfiled and aggressively synced like my `.zshrc` or `init.el` so muscle memory carries across projects and systems. "This project uses the brewed LLVM toolchain for `clang` on this machine" is an acceptable memory; load-bearing user preferences belong in this `CLAUDE.md`.

## Planning

**Plan multi-file or new-surface work with `/phases`.** Treat the plan document as a handoff artifact — chat history doesn't survive `/clear`, the plan does. `/yield` at boundaries.

**Refactors are in scope.** Before growing a file that already mixes concerns, or if you smell stale design docs or bad locked-in library choices, pitch a solution to me before the bitrot spreads.

**No NIH syndrome.** Consult me before open-coding industry-standard libraries - no context-free grammars in regex, no hand-rolled auth. Proactively suggest the modern upstream state-of-the-art and loop me in to the library selection decision tree. I love finding out about clever upstream libraries.

## Testing

**Test through public APIs at real boundaries.** New public surfaces start with integration tests along natural seams. Reserve unit tests for tricky internals: state machines, algorithms with non-obvious edge cases. Cover error paths and edge cases rather than iterating happy-path variants.

If a test breaks under a behavior-preserving refactor, it's testing implementation — confirm with me that it's stale, then delete it. Write tests that make this distinction clear for future cold-reads.

**Don't chase flaky tests.** Tag me in if the failure looks environmental, flaky, or pre-existing. I'll help you debug.

## Coding

**Linux kernel sensibilities adapted to the language at hand:** small files with single, named concerns; reserve polymorphism for genuine plugin boundaries the way `struct file_operations` is in the kernel — pick vtables carefully. Layer files the way `fs/` splits `inode_operations`  from `file_operations`. The standard is clean, self-documenting code that strictly complies with the language idiom.

Write netdev-style commit messages that explain the motivation, not the diff: `subsystem: imperative summary\n\nwhy-not-what body`. Include `Closes <issue>` trailers when applicable.

**Strongly typed, not stringly typed.** Parse strings and bytes into typed values at the boundary; internal code takes the types, not raw strings. Newtypes earn their keep by encoding an invariant, not by relabeling a `str`.

**Move with the industry, code without ego.** Personal projects ride the bleeding edge of modern libraries and best practices, with downstream stability guarantees enumerated in AGENTS.md; upstream is always upstream.
