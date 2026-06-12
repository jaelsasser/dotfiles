---
name: interview
description: Drive a structured interview before building something new — capture vision, scope, constraints, and phasing, then summarize and write the agreed artifact. Use when asked to "interview me", to spec out a new project/feature/tool, or to think something through before writing code.
---

## Behaviour

Drive the interview; don't jump to building. Capture what / why / how / when, summarize, then produce the artifact the interview agreed on.

Scale depth to the undertaking. Calibration: a one-off script → three questions and stop; a new service or library → the full arc. Stay conversational, not a quiz — ask follow-ups, skip what's already answered, reword when a question doesn't land.

**Don't use this** when the work is already specced or it's a routine change to an existing thing — there's nothing to elicit. This is for the fuzzy front of something new.

## Questions

Universal — ask for everything:

1. What is this, in one sentence?
2. Who's it for — you, a team, the public?
3. What's the MVP, the smallest version worth having?
4. Constraints on tech or approach, and why?
5. What does "done" look like for phase one?

Adapt to the shape, asking only what fits:

- **Library / reusable** — public API surface, versioning, compatibility guarantees.
- **Service / backend** — who consumes it, scaling, observability.
- **CLI / tool** — command structure, the primary interaction.
- **Personal / one-off** — skip the team and deployment questions entirely.

## Summary

Close the interview with a structured summary — the foundation for anything written next:

```
<Name>
Vision:         <1–2 sentences>
For:            <primary user>
Scope / MVP:    <what ships first>
Approach:       <tech or method, and why>
Phase 1 done:   <definition>
Key decisions:  <load-bearing choices, with rationale>
Open questions: <what's still unresolved>
```
