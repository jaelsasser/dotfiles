Task-specific claudeMd extension.

# claudeMd

<important if="permission_mode == planning">

### Plans

**Structure work into Stages.** Each Stage is a function with a contract that handles a focused set of concerns in an isolated context.

**Distill your reasoning into the plan document**: the plan document carries Stage contracts forward. Cold executors entering Stage N+1 read the plan and ingest the Stage's in-scope documents and task list verbatim.

**Scope in refactors before bitrot spreads.** When growing a file that mixes three or more concerns, or if you smell stale design docs or bad locked-in library choices, escalate with a proposed scope increase that includes a refactor.

#### Stage Template

```
[...]

## Stages

Execute in rough order. Just-in-time revisions are allowed but record changes here.

### S1: First Stage

**Model:** suggested model for this phase (I apply this)

**Files:**
- read src/foo.c
- read src/bar.c:1100-1200 - prior art
- read `src/baz.c`, write the `vibe()` function

**Preconditions:** entry state.
- which phases are dependencies
- what must be true at start

**Postconditions:** exit state
- files written
- behaviours landed
- artifacts produced

#### Details

Freeform Stage-specific details, put whatever you want here.

#### Suggested Tasks

[ ] S1-0: Recommended `advisor` check-in on cold start to fill in gaps (optional, only when `advisor` skill is available)
[ ] S1-1: First acutal task, always starts as P1.1
[ ] S1-2: Another implementation task
[ ] S1-3: A third implementation task, mid-stream `advisor` guidance (optional)

### S2: Second Stage...

### S3: Third Stage...

[...]
```

#### Keep Stages light

**Weigh phases based on estimated context window usage.** Calibrations: 2000 SLOC or 20KB of prose → light; 10,000 SLOC or 100KB of prose → heavy.

**Merge light phases with overlapping read/write sets the same model recommendation.**

**Split heavy Stages into non-overlapping read/write sets.** Calibrations: if A/B and C/D never co-occur, `S1: Read(A,B,C,D), Write(A,C)` → `S1: Read(A,B), Write(A); S2: Read(C,D), Write(C)`; if K's edits only derive from H/J its Stage doesn't need E/F/G/I in context, `Read(E, F, G, H), Edit(I, J, K)` → `Stage 1: Read(E, F, G, H), Edit(I, J, K); Stage 2: Read(H, J), Write(K)**.

#### Front-load exemplar work

**Design work early.** Order Stages so data models, golden tests, schemas, and foundational abstractions land before fan-out implementation. They anchor every Stage that depends on them.

**Review the design with me.** Surface exemplar writes for review before fanning out. Cheap to redirect a schema; expensive to undo a fan-out built on the wrong one.

**Reuse Plan mode context for exemplars.** Plan mode's expensive model and heavy context pay for themselves on one-shot exemplar writes; otherwise `yield` before diving in. Calibrations: golden integration tests or schemas → land them on disk instead of round-tripping through the plan document; data models and fundamental abstractions → realize with full Plan mode context; foundational authn/authz wiring that still needs fan-out file reads → `yield`.

</important>
