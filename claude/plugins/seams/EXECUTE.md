Before touching code, build a live todo list — one TodoWrite per substantive step, each planned commit as its own task, ending with `(/seams:tack after completion)` if more seams remain.

The `/seams:tack` task calls `chalk.sh --sid <session-id>` with stdin = runtime deltas since plan-time (facts the planner couldn't predict). Call shape: `chalk.sh --sid "${CLAUDE_SESSION_ID}" < CONTENT` — stdin appends to the active session seam. The task then runs `/seams:tack`, a mechanical renderer; fill from rich in-flow context, don't defer judgment to it.

Single-window plans skip the yield task entirely: just substantive steps and commit boundaries.
