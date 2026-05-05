Fork `/seams:pattern` first — it reads the plan and writes the seam files. Wait for `pattern: seam-N.md ... written, cursor=N`. If pattern reports `no plan found` or `no ## Context Windows section`, surface the message and stop — there is no recoverable path.

If the plan has a `### Context Window 0` section with TODO items: work through them in order, use `/seams:chalk` for mid-flow facts, and end with `/seams:tack` once all tasks are complete. Then apply `### Seam 0`'s `Recommendation:` field: `/clear` or `/compact`.

If there is no `### Context Window 0` (or it has no TODO items): apply `### Seam 0`'s `Recommendation:` field directly.

`/seams:tack` is a mechanical renderer; fill from rich in-flow context, don't defer judgment to it.
