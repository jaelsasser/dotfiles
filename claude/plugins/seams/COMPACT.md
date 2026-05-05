Below this line, the precompact hook appends the active seam handover (if any) verbatim after a separator.

If the seam's `Recommendation:` is `/compact`, its `#### Next window needs` list is the preservation target — preserve exactly those runtime-discovered facts and drop the rest. `## Chalk` and `## Tack` sections contain mid-window discoveries and the final handover bundle — preserve those too.

If the `Recommendation:` is `/clear`, no compaction preservation needed — the seam will be bridged manually via `/seams:stitch` at the planned context boundary.

Otherwise (no seam — compaction mid-window without crossing a seam), fall back to preserving: the list of modified files, any pending TODOs, and the test/build commands relevant to current work.
