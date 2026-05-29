---
name: reference
description: Discipline for writing reference-mode documentation — the accurate, scannable description of how a thing works, its options, flags, variables, interfaces, and behaviour. Use when drafting the descriptive parts of a README, an API or config reference, a man page, or any what-it-is/what-it-does section.
---

## Discipline

<important unless="overridden by house style">

Reference register, BSD manpage tradition — describe the system, not the reader's experience. Engineer-to-engineer, no marketing.

- **Define the thing first.** "A [thing] that [does the verb]." One sentence, no adjectives, no preamble.
- **State constraints over feelings.** "All config under `$XDG_CONFIG_HOME`" beats "we follow XDG conventions." Calibration: "blazingly fast" → cut; "fits in 4000 LOC, single-pass" → keep.
- **Tables for scannable reference.** Deps, options, supported-thing matrices — a table beats a paragraph; build variables and CLI flags are the prime candidate (`Variable | Default | Description`). Calibration: three flags with defaults → table; the one flag every user passes → inline in the synopsis, not a one-row table.
- **Commands and signatures before prose.** Show the invocation, then explain — when the command isn't self-evident.
- **Third-person, declarative.** "The daemon writes…" not "you'll see it write…". Describe behaviour; leave the reader's hands to how-to and tutorial.
- **One term per concept.** Pick a word and keep it — mixing "flag", "option", "switch" for one thing makes the reference unsearchable.

</important>
