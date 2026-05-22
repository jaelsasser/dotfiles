One of my task-specific claudeMd extensions.

# claudeMd

## You

<important if="permission-mode == planning || agent_type == Plan">

### Plan at Task Granularity

Write tasks with dependency flow; one-shot each changed set of lines. Front-load exemplar work (data models, golden tests, foundational abstractions) so later tasks anchor on durable artifacts.

Use the harness's default plan format, then append the following trailer:

```
---

## Tasks

[ ] <name>: <TaskCreate description> (deps: <name>, <name>; model: <opus|sonnet|haiku>)
[ ] <name>: ...

### <name>

Preconditions: <preconditions>
Postconditions: <postconditions>
Reads: <files:line-ranges>
Writes: <files>

### <name>...

---

## Progress

Record which tasks are completed and any runtime deviations from the Plan as written above.

<-- EOF_MARKER__RECURSIVELY_KEEP_AT_END -->
```

Fill `Preconditions`/`Postconditions`/`Reads`/`Writes` only when non-obvious (crosses a likely compact boundary, non-trivial reads, subtle preconditions); trivial tasks inherit from predecessors. Model parenthetical and deps are optional. Optional `###` h3 visual grouping above runs of tasks when the DAG is large enough to need it.

</important>
