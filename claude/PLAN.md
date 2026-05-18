Task-specific claudeMd extension.

# claudeMd

<important if="permission_mode == planning">

### Plans

**Structure work into Phases.** Each Phase is a function with a contract that handles a focused set of concerns in an isolated context.

**Distill your reasoning into the plan document**: the plan document carries Phase contracts forward. Cold executors entering Phase N+1 read the plan and ingest the Phase's in-scope documents and task list verbatim.

**Scope in refactors before bitrot spreads.** When growing a file that mixes three or more concerns, or if you smell stale design docs or bad locked-in library choices, escalate with a proposed scope increase that includes a refactor.

#### Phases as functions

Define each Phase with five structured fields:
1. **Preconditions:** Entry state - what must be true at start, which Phases are dependencies.
2. **Postconditions:** Exit state - files written, behaviors landed, artifacts produced.
3. **Model and sub-agents:** Provide model recommendation for the task complexity of each individual Phase (I act on this, not you). Split Phases that aren't uniformly suited for the recommended model. Plan around sub-agent swarms for embarrassingly parallel work.
4. **In-scope documents and source code:** Files and lines held in context.
5. **Tasks:** Concrete, ordered steps to seed `TaskCreate`. 

#### Keep Phases light

**Weigh phases based on estimated context window usage.** Calibrations: 2000 SLOC or 20KB of prose → light; 10,000 SLOC or 100KB of prose → heavy.

**Merge light phases with overlapping read/write sets the same model recommendation.**

**Split heavy Phases into non-overlapping read/write sets.** Calibrations: if A/B and C/D never co-occur, `Read(A,B,C,D), Write(A,C)` → `Phase 1: Read(A,B), Write(A); Phase 2: Read(C,D), Write(C)`; if K's edits only derive from H/J its Phase doesn't need E/F/G/I in context, `Read(E, F, G, H), Edit(I, J, K)` → `Phase 1: Read(E, F, G, H), Edit(I, J, K); Phase 2: Read(H, J), Write(K)`.

#### Plan mode is a heavy Phase

**Treat Plan mode as a Phase that starts after `ExitPlanMode`.** Only make edits when the expensive model and heavy context pay for themselves as force multipliers. Calibrations: golden integration tests or schemas → land them on disk instead of round-tripping through the plan document; data models and fundamental abstractions → realize with full Plan mode context; foundational authn/authz wiring that needs heavy file reads → a discrete Phase that lands after housekeeping.

</important>
