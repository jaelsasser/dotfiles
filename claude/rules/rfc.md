---
paths:
  - "**/docs*/**"
  - "**/rfc*"
  - "**/rfc*/**"
  - "**/*.rst"
---
File-specific claudeMd extension.

## claudeMd

### Documentation

#### Long-form RST

<important unless="overridden by house style">

Design-doc register modeled on the TCP BBR IETF draft and the WireGuard whitepaper — engineer-to-engineer prose, no marketing register. If this particular file is actually a setup guide or reference card despite living under `docs/`, follow `readme.md`'s register instead.

- **Problem before solution.** Open with what's broken or absent in the alternatives — name them. Don't restate the abstract.
- **Show, don't narrate.** Pseudocode, ASCII diagrams, labeled equations, pricing or intuition tables ground claims. Prefer them to paragraphs that walk a reader through the same content step-by-step.
- **Third-person, declarative.** "The model produces…" not "you read the reply and…". Reference and design docs describe the system, not the reader's experience. Tutorials are the exception.
- **Restrained terminology.** Coin a name only when it's load-bearing across the whole document — WireGuard earns *Cryptokey Routing* because the protocol hinges on it. A one-section concept doesn't need a capitalized name.
- **Footnote primary literature, depth calibrated to audience.** Link the arxiv paper, the RFC, the official doc — not a blog summary. Cite-only reads right when readers know the field; primer-level expansion reads right when they don't.
- **Quantify.** Numbers, line counts, throughput. "Fits in 4000 LOC, single-pass" beats "small and fast."
- **Honest about limits.** State failure modes, scope boundaries, what the work *doesn't* cover. A whitepaper that doesn't admit a downside isn't trusted.
- **No filler.** Drop "it is important to note that…", "in what follows we will…". Every sentence advances the model.

</important>
