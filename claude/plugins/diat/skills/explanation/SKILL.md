---
name: explanation
description: Understanding-oriented docs in a whitepaper register. Use for design docs, RFCs, ADR rationale, ARCHITECTURE, or prose justifying a design.
---

## Discipline

<important unless="overridden by house style">

Design-doc register — TCP BBR / WireGuard whitepaper voice, no marketing.

- **Problem before solution.** Open with what's broken or absent in the alternatives — name them. Don't restate the abstract.
- **Show, don't narrate.** Pseudocode, ASCII diagrams, labeled equations, pricing or intuition tables ground claims. Prefer them to paragraphs that walk a reader through the same content step-by-step.
- **Third-person, declarative.** "The model produces…" not "you read the reply and…". Describe the system, not the reader's experience.
- **Restrained terminology.** Coin a name only when it's load-bearing across the whole document — WireGuard earns *Cryptokey Routing* because the protocol hinges on it. A one-section concept doesn't need a capitalized name.
- **Footnote primary literature, depth calibrated to audience.** Link the arxiv paper, the RFC, the official doc — not a blog summary. Cite-only reads right when readers know the field; primer-level expansion reads right when they don't.
- **Quantify.** Numbers, line counts, throughput. "Fits in 4000 LOC, single-pass" beats "small and fast."
- **Honest about limits.** State failure modes, scope boundaries, what the work *doesn't* cover. A whitepaper that doesn't admit a downside isn't trusted.
- **Plain, declarative prose.** No filler ("it is important to note…"), no fragment openers ("Concrete picture."), no editorial qualifiers ("annoying," "catastrophic"), no noun-phrase clause-subjects, no figurative softening. Calibration: "As a concrete example: …" → keep; "Concrete picture. You…" → rewrite; "$0.09–0.15 per call" → keep; "reads as noise" / "always-on cousin" → cut.

</important>
