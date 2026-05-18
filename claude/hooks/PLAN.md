Task-specific claudeMd extension.

# claudeMd

<important if="permission_mode == planning">

### Plans

**Structure work into Phases.** Each Phase is a function with a contract that handles a focused set of concerns in an isolated context.

**Distill your reasoning into the plan document**: the plan document carries Phase contracts forward. Cold executors entering Phase N+1 read the plan and ingest the Phase's in-scope documents and task list verbatim.

**Scope in refactors before bitrot spreads.** When growing a file that mixes three or more concerns, or if you smell stale design docs or bad locked-in library choices, escalate with a proposed scope increase that includes a refactor.

#### Phase Template

```
[...]

## Phases

Execute yields and phases in order. Just-in-time reordering is allowed but record changes here.

### yield - omit if Phase 1

### P1: First Phase

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

Freeform Phase-specific details, put whatever you want here.

#### Suggested Tasks

[ ] P1.0: Recommended `advisor` check-in on cold start to fill in gaps (optional, only when `advisor` skill is available)
[ ] P1.1: First acutal task, always starts as P1.1
[ ] P1.2: Another implementation task
[ ] P1.3: A third implementation task, mid-stream `advisor` guidance (optional)

### yield - omit if no yield before P1 and P2

### P2: Second Phase...

### yield - omit if no yield before P2 and P3

### P3: Third Phase...

[...]
```

#### Keep Phases light

**Weigh phases based on estimated context window usage.** Calibrations: 2000 SLOC or 20KB of prose → light; 10,000 SLOC or 100KB of prose → heavy.

**Merge light phases with overlapping read/write sets the same model recommendation.**

**Split heavy Phases into non-overlapping read/write sets.** Calibrations: if A/B and C/D never co-occur, `Read(A,B,C,D), Write(A,C)` → `Phase 1: Read(A,B), Write(A); Phase 2: Read(C,D), Write(C)`; if K's edits only derive from H/J its Phase doesn't need E/F/G/I in context, `Read(E, F, G, H), Edit(I, J, K)` → `Phase 1: Read(E, F, G, H), Edit(I, J, K); Phase 2: Read(H, J), Write(K)**.

#### Phase 1 yield?

**Decide whether to yield before starting Phase 1.** Only skip the `yield` when Plan mode's expensive model and heavy context pay for themselves as force multipliers. Calibrations: golden integration tests or schemas → land them on disk instead of round-tripping through the plan document; data models and fundamental abstractions → realize with full Plan mode context; foundational authn/authz wiring that needs heavy file reads → a discrete Phase that lands after housekeeping.

</important>
