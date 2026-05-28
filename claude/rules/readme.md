---
paths:
  - "**/README*"
  - "**/AGENTS.md"
  - "**/CONTRIBUTING*"
  - "**/INSTALL*"
---
File-specific claudeMd extension.

## claudeMd

### Documentation

#### Short-form README

<important unless="overridden by house style">

Reference register, BSD manpage tradition — engineer-to-engineer, no marketing. Shape the file as a density sandwich: terse synopsis up top, dense reference tables in the middle, prose examples and see-also at the bottom. If this particular file is actually load-bearing design despite the README name, follow `rfc.md`'s register instead.

- **Lead with what it is in one sentence.** "A [thing] that [does the verb]." No adjectives, no preamble.
- **Commands first, explanation after.** Shell blocks before prose, when the command isn't self-evident.
- **Tables for scannable reference.** Deps, options, supported-thing matrices — a table beats a paragraph. Not for file listings or package inventories; those belong in AGENTS.md.
- **Inventory belongs in AGENTS.md, not here.** File listings, package matrices, and directory trees are AGENTS.md territory — link to them, don't duplicate.
- **Section names that work as a TOC.** *Commands*, *Architecture*, *Known issues*, *Hooks*, *Disabling* — earn each slot. Not "Overview" or "Introduction."
- **Defer detail by link.** When a behavior needs a paragraph, link to source or to the long-form doc.
- **State constraints over feelings.** "All config under `$XDG_CONFIG_HOME`" beats "we follow XDG conventions." Calibration: "blazingly fast" → cut; "fits in 4000 LOC, single-pass" → keep.

</important>
