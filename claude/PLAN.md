Task-specific claudeMd extension.

# claudeMd

<important if="permission_mode == planning">

### Plans

**Lay out work in discrete stages.** Each Stage is a function with a contract that handles a focused set of concerns in an isolated context.

**Distill your reasoning into the plan document**: the plan document carries Stage contracts forward. Cold executors entering Stage N+1 read the plan and ingest the Stage's in-scope documents and task list verbatim.

**Scope in refactors before bitrot spreads.** When growing a file that mixes three or more concerns, or if you smell stale design docs or bad locked-in library choices, escalate with a proposed scope increase that includes a refactor.

#### Stage Template

Materialize this template at the end of the Plan document:

```
---

## Stages

A Stage is what a cold executor enters with only the plan doc loaded. Just-in-time adjustments are allowed but update this plan first.

### S1: First Implementation Pass

**Preconditions:** entry state.
- which phases are dependencies
- what must be true at start

**Postconditions:** exit state
- files written
- behaviours landed
- artifacts produced

**Files:**
- read src/foo.c
- read src/bar.c:1100-1200 - prior art
- read `src/baz.c`, write the `vibe()` function

**Model:** suggested model for this phase (You remind me and I apply this)

#### Description

{{ Freeform Stage-specific details, put whatever h4 sections and prose you want here so that Tasks can stay short and punchy. }}

#### Tasks

[ ] S1.0: Recommended `advisor` check-in on cold start to fill in gaps (optional, only when `advisor` skill is available)
[ ] S1.1: First actual task, always starts as P1.1
[ ] S1.2: Another implementation task
[ ] S1.3: A third implementation task, with mid-stream `advisor` recommendation (when available)

### P2: Second Stage...

### P3: Third Stage...
```

#### Keep Stages light

**Weigh phases based on estimated context window usage.** Calibrations: 2000 SLOC or 20KB of prose → light; 10,000 SLOC or 100KB of prose → heavy.

**Merge light phases with overlapping read/write sets and the same model recommendation.**

**Split heavy Stages into non-overlapping read/write sets.** Calibrations: if A/B and C/D never co-occur, `Read(A,B,C,D), Write(A,C)` → `Stage 1: Read(A,B), Write(A); Stage 2: Read(C,D), Write(C)`; if K's edits only derive from H/J its Stage doesn't need E/F/G/I in context, `S1: Read(E, F, G, H), Edit(I, J, K)` → `S1: Read(E, F, G, H), Edit(I, J, K); S2: Read(H, J), Write(K)`.

#### yield before Stage 1?

**Decide whether to invoke the `yield` Skill before starting Stage 1.** Only skip when Plan mode's expensive model and heavy context are genuine force multipliers. Calibrations: golden integration tests or schemas → land them on disk instead of round-tripping through the plan document; data models and fundamental abstractions → realize with full Plan mode context; foundational authn/authz wiring that still needs fan-out file reads → `yield` before diving in.

</important>
