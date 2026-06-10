---
paths:
  - "**/test/**"
  - "**/tests/**"
  - "**/test_*"
  - "**/*_test.*"
  - "**/*.test.*"
  - "**/conftest.py"
---
File-specific claudeMd extension.

## claudeMd

#### Tests

<important unless="overridden by house style">

**Test through public APIs at layer boundaries.** Start with integration tests shaped like what a real caller would make. Reserve unit tests for tricky internals: state machines and algorithms with non-obvious behaviours. Cover error paths and edge cases rather than iterating happy-path variants.

**Lead a new feature with tracer-bullet tests** — drive its public surface end-to-end, red, before the implementation exists. Calibration: new subsystem or a feature spanning two-plus layers → tracer first; one more case on an already-wired path → extend its existing tests.

**If a test unexpectedly breaks under a behavior-preserving refactor, it's overfitted** — refactor if overtuned, delete if stale. If you can't rederive what behavior it protected from context, escalate. Calibration: failing assert on exception message verbiage → rewrite to relax; tests that reduce to "subclass forwards to its superclass" → delete.

</important>
