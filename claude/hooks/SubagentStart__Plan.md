One of my task-specific claudeMd extensions.

# claudeMd

## You

<important if="permission-mode == planning || agent_type == Plan">

### Write Staged Plans

**Structure work into Stages.** Each Stage is a function with a contract that handles a focused set of concerns in an isolated context. Use this template:

```
---

## Stages

Execute in rough order. Just-in-time revisions are allowed but record changes here.

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

[ ] S1-0: When the `advisor` skill is available, optionally plan an `advisor` check-in task for precondition validation / JIT design. Hedge towards including this.
[ ] S1-1: First acutal task, always starts at S1.1
[ ] S1-2: Another implementation task...
[ ] S1-3: A third implementation task, optional mid-stream `advisor` guidance before or after landing complex work
[ ] S1-4: A fourth implementation task...

### S2: Second Stage...

### S3: Third Stage...
```

#### Front-load exemplar work

**Design work early.** Order Stages so data models, golden tests, schemas, and foundational abstractions land before fan-out implementation. They anchor every Stage that depends on them.

**Reuse Plan mode context for exemplars.** Plan mode's expensive model and heavy context pay for themselves on one-shot exemplar writes; otherwise the `cac:condense` Skill before diving in. Calibrations: golden integration tests or schemas → land them on disk instead of round-tripping through the plan document; data models and fundamental abstractions → realize with full Plan mode context; foundational authn/authz wiring that still needs fan-out file reads → the cac `condense` Skill.

#### Keep Stages light

**Weigh phases based on estimated context window usage.** Calibrations: 2000 SLOC or 20KB of prose → light; 10,000 SLOC or 100KB of prose → heavy.

**Merge light phases with overlapping read/write sets and the same model recommendation.**

**Split heavy Stages into non-overlapping read/write sets.** Calibrations: if A/B and C/D never co-occur, `Read(A,B,C,D), Write(A,C)` → `Stage 1: Read(A,B), Write(A); Stage 2: Read(C,D), Write(C)`; if K's edits only derive from H/J its Stage doesn't need E/F/G/I in context, `S1: Read(E, F, G, H), Edit(I, J, K)` → `S1: Read(E, F, G, H), Edit(I, J, K); S2: Read(H, J), Write(K)`.

</important>
