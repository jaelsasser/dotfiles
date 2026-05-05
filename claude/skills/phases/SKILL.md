---
name: phases
description: structure long Plans into tight Phases
when_to_use: plans that introduce touch public API/trait/CLI surface or span 2+ files with design choices
argument-hint: [optional: explicit user-provided guidance on how to shape phases]
---

Structure work into Phases. Each Phase is a function with a contract that handles a small number of concerns.

## Phase as function

Define each Phase with five fields:

- **Preconditions.** Entry state. What must be true at start.
- **Postconditions.** Exit state. Files written, behaviors landed, artifacts produced. Cap at five items; split along the natural fileset and concern boundaries.
- **In-scope fileset.** Files this Phase reads or edits. Sizes the Phase. Roughly 3–10 files; large files count as several.
- **Recommended model.** Match the model to the task character of this Phase, not to the plan as a whole. Split Phases that aren't uniformly suited for the recommended model. The operator will act on this, not the executor.
- **Task list.** Concrete, ordered steps. The last two tasks are always:
  1. Verify postconditions hold.
  2. Call `/yield` to hand off.

## Plan mode is a unique Phase

Work after ExitPlanMode inherits an expensive model and a dense context window full of research and reasoning. Use that sparingly for high-concept foundational outputs: critical authn/authz wiring, fundamental interface abstractions, or tone-setting reference implementations.

## Composability

Phase N's postconditions are Phase N+1's preconditions. Check this before yielding the plan; mismatches mean the plan is malformed.

## Sizing

If a Phase's postconditions exceed five items, or "how to do it;" cannot be stated without re-deriving research, split the Phase. Two Phases that cannot reach yield without `/compact` between them are too big.

## Efficiency

Merge phases with overlapping filesets and model recommendations when it doesn't pollute the postconditions. Embarrassingly parallel phases are sub-agent territory.

## Yield mechanics

End each Phase with a `/yield` invocation. `/yield` reads the plan and working-tree state, verifies the Phase's postconditions, and returns a recommendation: `/clear`, `/compact` (with keep/drop lists), or `continue`.

The plan document carries Phase contracts forward. Distill your reasoning into the plan document to save on later derivation: cold executors entering Phase N+1 read the plan, ingest the Phase's task list verbatim, and proceed. 
