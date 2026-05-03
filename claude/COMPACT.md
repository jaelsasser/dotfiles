If a seam follows below, its `## Next window needs` list is the preservation target — preserve exactly those runtime-discovered facts and drop the rest. The windows ahead don't need the windows behind, only the facts the planner flagged.

Otherwise (no seam — compaction mid-window without crossing a seam), fall back to preserving: the list of modified files, any pending TODOs, and the test/build commands relevant to current work.
