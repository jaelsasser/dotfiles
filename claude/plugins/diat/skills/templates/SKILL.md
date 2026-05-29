---
name: templates
description: Pick the right structure and diátaxis register before writing a documentation file — routes README, AGENTS.md/CLAUDE.md, RFC, ADR, PRD, ARCHITECTURE, runbook, and docs/ files to a per-type template and the writing axes it composes. Use when creating a new doc file or substantially restructuring one.
---

## Routing

Find the doc type, invoke the named axis skills for the register, and read the matching template under `reference/` for its structure and skeleton. Most files are one primary axis with passages of others — that composition is the point, not a smell.

| Doc type | Primary axis | Composes | Template |
|---|---|---|---|
| README | `diat:reference` | `diat:how-to`, `diat:tutorial`, `diat:explanation` | `reference/readme.md` |
| AGENTS.md / CLAUDE.md / tool-instruction file | `diat:reference` | — | `reference/agents.md` |
| RFC / proposal / `docs/`, `*.rst`, `*.tex` | `diat:explanation` | `diat:reference` | `reference/rfc.md` |
| ARCHITECTURE / DESIGN / system overview | `diat:explanation` | `diat:reference` | `reference/architecture.md` |
| ADR (decision record) | `diat:explanation` | — | `reference/adr.md` |
| PRD (requirements) | `diat:explanation` | `diat:reference` | `reference/prd.md` |
| runbook / playbook / incident response | `diat:how-to` | `diat:reference` | `reference/runbook.md` |

Invoke the axis skills named above for the writing register; the template carries only the file-specific structure. Don't follow links into an axis `SKILL.md` — invoke it as a skill.

## Cross-cutting rules

<important unless="overridden by house style">

These hold across every axis and form — engineer-to-engineer, no marketing.

- **Inline vs defer.** When a non-obvious *why* changes behaviour at decision time, state it in one clause where the rule lives — a silent constraint gets questioned and reverted. Otherwise defer by anchored link (`../README.md#section`, never a bare file link or page top). Exception: when the canonical reference is auth- or VPN-gated, carry the minimum inline rather than stranding the reader. Calibration: an optional-but-recommended step a reader would skip as busywork or over-trust as load-bearing → inline the why; the full history of how the system got here → link.
- **Split by surrounding files.** README alone → self-contained: carry the procedures, the troubleshooting, the inline rationale. README + AGENTS.md → README is the human-facing reference, AGENTS.md owns inventory + build/test commands + AI-legible facts; don't duplicate, link. README + AGENTS.md + `docs/` → README is synopsis and jump links, deep dives and rationale live in `docs/`; defer heavily by anchored link.
- **Composition is normal.** A README with a quickstart tutorial inline is fine; don't extract a single procedure into a doc of its own. Reach for a separate file only when the surrounding files call for the split above.
- **Section names work as a TOC.** *Commands*, *Architecture*, *Known issues* — earn each slot. Not "Overview"/"Introduction" (contentless), "Advanced Topics" (names the reader's level, not the content), or "Breadcrumbs" (a nav metaphor). Don't hand-maintain a TOC; the headers are the TOC, and a manual one bitrots on the first rename.

</important>
