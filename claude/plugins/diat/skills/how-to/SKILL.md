---
name: how-to
description: Task-oriented docs — a competent reader through a concrete procedure. Use for install/setup steps, an operational procedure or runbook, a step-by-step guide, or a troubleshooting (error→cause→fix) section.
---

## Discipline

<important unless="overridden by house style">

Task-oriented register — assume competence, drive to one concrete goal. Imperative voice; skip the background, that's explanation's job.

- **Number a procedure; structure each step.** A multi-step flow is an ordered list — imperative verb, the inline command, then the one-line why. A one-shot install is a single shell block; an eight-step package-add is numbered steps.
- **Cross-reference, don't repeat.** Point to a sibling procedure by name ("see the *From the Conan Center* steps above") instead of restating it.
- **Troubleshooting is error → cause → fix.** Put the literal error in a fenced block and make its distinctive line the subsection heading — the reader has that text on screen and searches for it. Cause, then fix, follow.
- **Name the failure they'll hit, not every theoretical one.** One real, common stumble with its fix beats an exhaustive error catalogue nobody reads.
- **Callout the destructive step.** A `> [!WARNING]` before an irreversible command earns its place; one or two per document, past that they stop being scannable.

</important>
