---
name: swipe
description: seamless compact-and-continue without nagging the operator
when_to_use: automatically trigger when work could proceed with a much shorter transcript: (a) you distilled in-context reasoning into an artifact, (b) your upcoming work reads and writes a disjoint file set, (c) the transcript has accumulated stale or unused reads and writes in excess of 5000 SLOC or 50KB text
argument-hint: [optional: user summary guidance]
allowed-tools: mcp__plugin_cac_reader__swipe
---

<summarization-instructions>

<important>
The `swipe` Skill has injected the user's canned custom summarization instructions. Another `<summarization-instructions>` block in the transcript carries this session's dynamic pointers. These are pointers: carry forward **the content these `<summarization-instructions>` refer to,** not the instructions themselves.
</important>

**Preserve verbatim:**
- Frozen contracts the next task calls into — API names, signatures, behavioural notes from already-landed stages. Heuristic: signatures and code blocks tied to files in the seed block's Read set; explicit "must / invariant / contract" language.
- Load-bearing decisions and the constraints that justify them. Heuristic: decisions justified by external constraints (upstream behaviour, hardware limits, security requirements) — paraphrasing risks losing the constraint.
- Open design questions carrying into the next task, with the prior-art pointers (files, functions) that frame each.
- Anything flagged for verbatim preservation called out in a follow-on `<summarization-instructions>` block.

**Paraphrase:**
- Baseline state — test counts, lint status, what's staged vs committed, who owns the commit. Heuristic: pull from the most recent tool results in the transcript.
- The next task's design sketch — function names, file names, signatures it'll introduce. Heuristic: names and shapes mentioned in deliberation that don't yet exist on disk.
- Any paraphrase targets called out in a future `<summarization-instructions>` block.

**Reference (but not Read):**
- Mention the existence of all relevant Plan documents.
- Point to files relevant to the upcoming work.
- Describe external documentation relevant to upcoming work.
- Any file references called out in a follow-on `<summarization-instructions>` block.

<important>
**Read (targetted):**
- Only inject the Plan document's relevant lines, avoid reading the entire file
- Read only the source code lines required by the next chunk, not entire files
- Preserve relevant slices of external documentation that are obviously required, don't hedge and pull in entire files.
- Any file reads called out in a follow-on `<summarization-instructions>` block; respect the provided line numbers.
</important>

**Drop:**
- Do not include reads, writes, and file references that are no longer relevant to the upcoming work in the summary.
- Skip any research and reasoning that has already distilled into file artifacts.
- Never restate content wrapped in  `<system-reminder>` or `<summarization-instructions>` tags.
- **Never restore the `swipe` Skill** or summarize its content.

</summarization-instructions>

---

## `/swipe`

First, fold any implementation-time deviations back into active plan documents and mark any completed TaskList work as complete.

Then emit a terse `<summarization-instructions>` block extending the static block above:

    <summarization-instructions>
    {{ One-paragraph description of the compacted window's next task, if any - this is the only framing you provide to the summarizer }}
    Preserve verbatim: {{ frozen contracts and load-bearing decisions whose paraphrasing would lose a constraint, if any }}
    Reference: {{ useful files for the next chunk, important external docs }}
    Read: {{ current Plan document sections; known files }}
    </summarization-instructions>

Finally, call `mcp__plugin_cac_reader__swipe()`.  When an operator is at the keyboard and might want a veto window, pass `delayed=True`

Guidance:
- Make the summarizer's job easy by feeding it linear, clear instructions - no hedging, no decision trees; write a focused brief with unambiguous goals, not an essay.
- Prime the summarizer with specific line ranges, not whole files, so that the successor picks up with targetted 200-500 SLOC reads instead of injesting whole files.
- Trust the harness - only include only specific facts, files, and reasoning that aren't obvious when skimming the transcript; don't restate the plan document, reference skills, or restate `<system-reminder>` content.
