Fork `/seams:pattern` first — it reads the plan and writes the seam files. Wait for the confirmation line before proceeding.

If the plan has a `### Context Window 0` section with TODO items: work through them in order, use `/seams:chalk` for mid-flow facts, and end with `/seams:tack` once all tasks are complete. Then apply the action from `### Seam 0`'s `Action:` field: `/clear` or `/compact`.

If there is no `### Context Window 0` (or it has no TODO items): go straight to `/clear`.

`/seams:tack` is a mechanical renderer; fill from rich in-flow context, don't defer judgment to it.
