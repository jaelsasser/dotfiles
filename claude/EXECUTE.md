Before touching code, build a live todo list — one TodoWrite per substantive step, each planned commit as its own task, ending with `(/baste after completion)` if more seams remain.

The `/baste`  task calls `~/.claude/skills/tack/scripts/tack.sh --sid <session-id>` with stdin = runtime deltas since plan-time (facts the planner couldn't predict) Call shape: `tack.sh --sid "${CLAUDE_SESSION_ID}" < CONTENT` — stdin appends to the active session seam. The task then runs `/baste`, a mechanical renderer; fill from rich in-flow context, don't defer judgment to it.

Single-window plans skip the yield task entirely: just substantive steps and commit boundaries.
