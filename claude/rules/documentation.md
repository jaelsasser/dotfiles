---
paths:
  - "**/*.md*"
  - "**/*.rst"
  - "**/README*"
  - "**/docs*/**"
  - "**/rfc*"
  - "**/rfc*/**"
---
File-specific claudeMd extension.

## claudeMd

### You

<important unless="overridden by house style">

#### Documentation

Two registers, picked by **content kind**, not filename. A `README.md` can be long-form if it's load-bearing design; a doc under `docs/` can be README-style if it's a setup guide.

##### Long-form (design docs, RFCs, whitepapers)

Model: the TCP BBR IETF draft and the WireGuard whitepaper. Engineer-to-engineer prose, no marketing register.

- **Problem before solution.** Open with what's broken or absent in the alternatives — name them. Don't restate the abstract.
- **Show, don't narrate.** Pseudocode, ASCII diagrams, labeled equations, pricing or intuition tables ground claims. Prefer them to paragraphs that walk a reader through the same content step-by-step.
- **Third-person, declarative.** "The model produces…" not "you read the reply and…". Reference and design docs describe the system, not the reader's experience. Tutorials are the exception.
- **Restrained terminology.** Coin a name only when it's load-bearing across the whole document — WireGuard earns *Cryptokey Routing* because the protocol hinges on it. A one-section concept doesn't need a capitalized name.
- **Footnote primary literature, depth calibrated to audience.** Link the arxiv paper, the RFC, the official doc — not a blog summary. Cite-only reads right when readers know the field; primer-level expansion reads right when they don't.
- **Quantify.** Numbers, line counts, throughput. "Fits in 4000 LOC, single-pass" beats "small and fast."
- **Honest about limits.** State failure modes, scope boundaries, what the work *doesn't* cover. A whitepaper that doesn't admit a downside isn't trusted.
- **No filler.** Drop "it is important to note that…", "in what follows we will…". Every sentence advances the model.

##### README (reference cards, package overviews, setup guides)

Model: the BSD manpage tradition — engineer-to-engineer reference, no marketing register. Shape it as a density sandwich: terse synopsis up top, dense reference tables in the middle, prose examples and see-also at the bottom.

- **Lead with what it is in one sentence.** "A [thing] that [does the verb]." No adjectives, no preamble.
- **Commands first, explanation after.** Shell blocks before prose, when the command isn't self-evident.
- **Tables for layouts.** A three-column file/purpose/notes table beats a paragraph. Same for runtime deps and supported-thing matrices.
- **ASCII trees for directory structure.** Beats narrated walks like "the `src/` directory contains…"
- **Section names that work as a TOC.** *Commands*, *Architecture*, *Known issues*, *Hooks*, *Disabling* — earn each slot. Not "Overview" or "Introduction."
- **Defer detail by link.** When a behavior needs a paragraph, link to source or to the long-form doc.
- **State constraints over feelings.** "All config under `$XDG_CONFIG_HOME`" beats "we follow XDG conventions." Calibration: "blazingly fast" → cut; "fits in 4000 LOC, single-pass" → keep.

</important>

