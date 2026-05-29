---
name: readme
description: Short-form reference-manpage writing discipline — engineer-to-engineer, density-sandwich layout, no marketing.
when_to_use: drafting or substantially editing a README, AGENTS.md, CONTRIBUTING, INSTALL, or other short-form reference doc.
---

## Discipline

<important unless="overridden by house style">

Reference register, BSD manpage tradition — engineer-to-engineer, no marketing. Shape the file as a density sandwich: terse synopsis up top, dense reference tables in the middle, prose examples and see-also at the bottom. If this particular file is actually load-bearing design despite the README name, use the `rfc` skill's register instead — but a README that is mostly an operational runbook stays here, under the procedural rules below.

- **Lead with what it is in one sentence.** "A [thing] that [does the verb]." No adjectives, no preamble.
- **State constraints over feelings.** "All config under `$XDG_CONFIG_HOME`" beats "we follow XDG conventions." Calibration: "blazingly fast" → cut; "fits in 4000 LOC, single-pass" → keep.
- **Commands first, explanation after.** Shell blocks before prose, when the command isn't self-evident.
- **Tables for scannable reference.** Deps, options, supported-thing matrices — a table beats a paragraph; build variables and CLI flags are the prime candidate (`Variable | Default | Description`). Calibration: three flags with defaults → table; the one flag every user passes → inline in the synopsis, not a one-row table.
- **Inventory belongs in AGENTS.md, not here.** File listings, package matrices, and directory trees are AGENTS.md territory — link to them, don't duplicate.
- **Section names that work as a TOC.** *Commands*, *Architecture*, *Known issues*, *Hooks*, *Disabling* — earn each slot. Not "Overview" or "Introduction" (contentless), "Advanced Topics" (names the reader's presumed level, not the content), or "Breadcrumbs" (a nav metaphor). Don't hand-maintain a TOC; the headers are the TOC, and a manual one bitrots on the first rename.
- **Number a procedure; structure each step.** A multi-step flow is an ordered list — imperative verb, the inline command, then the one-line why. Cross-reference a sibling procedure by name instead of repeating its steps. A one-shot install is a single shell block; an eight-step package-add is numbered steps.
- **Structure troubleshooting as error → cause → fix.** Put the literal error in a fenced block and make its distinctive line the subsection heading — the reader has that text on screen and searches for it. Cause, then fix, follow.
- **Callouts for the one thing that bites.** A `> [!WARNING]` or `> [!IMPORTANT]` for a destructive flag or sharp edge that must be read before acting earns its place; one or two per document, past that they stop being scannable.
- **Say why when the what isn't obvious.** State a surprising constraint's rationale in one clause where the rule lives — a silent constraint gets questioned and reverted. Calibration: an optional-but-recommended step a reader would skip as busywork or over-trust as load-bearing → inline the why; the full history of how the system got here → link.
- **Defer detail by link.** When a behavior needs a paragraph, link to source or the long-form doc — anchor the exact section (`../README.md#section`), not a bare file link or the top of a long page. Exception: when the canonical reference is auth- or VPN-gated, carry the minimum inline rather than stranding the reader behind a link they can't open.

</important>
