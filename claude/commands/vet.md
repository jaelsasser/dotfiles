---
description: Vet the current plan against constraints I flagged earlier in the conversation
argument-hint: [optional focus area]
---

Audit the most recent plan against constraints I raised earlier in this conversation, before any code is written.

If no plan has been drafted in this conversation, say so and stop — don't audit ad-hoc commentary as if it were a plan.

Optional focus from this invocation: $ARGUMENTS

Look back through the conversation for:
- Constraints I raised — prohibitions ("must not X", "don't break Y"), negative requirements ("we can't", "avoid"), or emphasized points ("important", "the gotcha here is").
- Implicit constraints from CLAUDE.md (testing posture, sub-agent threshold, architecture rules, no-NIH) that apply to this plan.

For each constraint found, output:
- The constraint, quoted or paraphrased with conversation reference
- Which plan step addresses it, or `UNADDRESSED`
- Confidence: `high` if the plan step explicitly handles it, `low` if you're inferring

End with exactly one of:
- "All flagged constraints addressed. Proceed?" — every constraint has a clear, high-confidence plan step
- "Unaddressed: [list]. How should we handle these?" — gaps exist
- "Uncertain about: [list]. Need clarification before proceeding."

Don't paper over gaps with reassurance. If you're guessing, say so. If no constraints were flagged in this conversation, say that — don't invent ones to look thorough.
