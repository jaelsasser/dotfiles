---
paths:
  - "**/USER_CLAUDE.md"
  - "**/USER_INSTRUCTION.md"
  - "**/AGENTS.md"
  - "**/SKILL.md"
  - "**/commands/*.md"
  - "**/hooks/*.md"
---
Discipline for *agent-facing* prose — written for an agent to consume, not a human: an `AGENTS.md` `CLAUDE.md` always-on instruction file, skill and command bodies, file-scoped rules, and hook output. The live files are ground truth: when a convention here lags what the instruction file actually does, update this rule to match, not the reverse.

An always-on instruction file paid for on every turn of every session; commands, skills, and hooks are paid for on demand. **This cost asymmetry drives most of the decisions below.**

## When asked to add to the always-on instruction file

Apply this filter, in order. Stop at the first failure.

1. **Is this addressed to the agent or to the user?** Only agent-actionable instructions belong here. Plan Mode itself is fair game — the model enters it via the `EnterPlanMode` tool, so it's an agent action.

2. **Does it address a recurring failure mode?** New rules need a real miss to point at. "What's a good rule" in the abstract is the wrong authoring signal. If the user can't name a specific time the rule would have helped, it doesn't go in.

3. **Does it run against the model's training default?** Rules that match what the model does by default earn no slot. Posture rules earn their cost because the default leans the other way. Calibration: "don't investigate tool failures by reading source" → earns its slot; "write clean code" or "add comments to complex logic" → default behavior, pure dilution.

4. **Is the failure mode something the harness already surfaces?** Claude Code already nudges around writes-without-reads, missing tests, and similar. Rules duplicating harness-surfaced behavior are redundant.

5. **Could this live somewhere else with better economics?** If the rule applies only sometimes (e.g. only during planning, only when delegating, only for a specific domain), prefer a slash command or skill. If the rule fires on a specific lifecycle event and doesn't depend on model judgment ("preserve X during compaction", "run Y after every edit"), prefer a hook — the harness enforces it deterministically with zero per-turn cost. Per-turn cost is a tax; on-demand cost is free until used. Calibration: scoped to a workflow → skill; fires on a lifecycle event without judgment → hook; always-on posture against the default → inline here.

If a rule passes all five, add it. Prefer one sentence of imperative phrasing. Pair negatives with positives ("don't X; prefer Y"). Don't add headers for single-line sections.

## When asked to remove from the always-on instruction file

Drift is a feature. As training data and harness behavior shift, rules become defaults and stop earning their slot. Watch for:

- The user mentions the agent doing the thing unprompted in a fresh project.
- A new project without the rule produces output that already satisfies it.
- The rule restates something the harness now surfaces or the system prompt now covers.

When you spot a candidate, raise it with the user. Don't remove unilaterally — the user has more context on whether they're still being bitten by the failure mode. When asked to shorten the file, start with rules the model now follows by default; those dilute without earning their slot.

## Style conventions (all agent-for-agent files)

- **Imperative voice, present tense.** "Delegate only when X" not "you should delegate when X."
- **Short sections.** Headers at `##` for top-level concerns; nested headers only when the section genuinely splits.
- **Bullets sparingly.** Prose is denser per token. Use bullets only for genuine lists where order doesn't matter or where the user benefits from scannability.
- **No persona, no preamble, no philosophy framing.** Don't start sections with "you are…" or "the goal is…". State the rule.
- **Absolutes are okay when justified.** "Never investigate tool failures by reading source" is more useful than "generally avoid investigating tool failures by reading source." Hedging dilutes.
- **Calibrate with examples, not adjectives.** When a rule's boundary is fuzzy, pin it with `Calibration: <near miss> → <verdict>; <other case> → <verdict>` — the way a well-tuned instruction file does throughout.
- **Gate conditional rules with `<important unless="...">`.** Wrap rules that should yield to project house style or domain idiom in `<important unless="the condition that suspends them">…</important>`, the way an instruction file does for its code defaults. The tag reads "follow this unless the named condition holds."

## Validation before committing

After any edit:
- Read the changed file end to end. Cross-cutting changes (e.g. moving a rule from the instruction file to a skill) need both files to be coherent.
- Check the always-on file isn't bloating. A frontier model follows on the order of 150–200 instructions reliably and the harness already spends part of that, so headroom is finite — every always-on rule taxes the rest. (A project may pin its own soft/hard line-count ceiling as an override.)
- Check that no rule in the always-on file references a slash command or UI mode by name. If it does, it's miscategorized.

## What not to do

- **Don't add tone or persona instructions** ("be concise", "be helpful"). The user's Claude.ai preferences cover tone; baking it in here is redundant and locks personality to one repo.
- **Don't add code-style rules.** They belong in PostToolUse hooks where a linter enforces them deterministically. If asked to add one, push back and suggest the hook.
- **Don't add team-rollout content to the always-on file.** That belongs in `managed-settings.json` or a project-level `team-conventions/` directory, not user-level.
- **Don't auto-trigger refactors.** If you notice an opportunity to restructure the file (e.g. "this could be a skill"), surface it as a suggestion before acting. The user has consistently preferred workshopping changes over accepting unilateral edits.
- **Don't optimize for length.** Shorter is usually better, but a rule that needs two sentences to be unambiguous gets two sentences. Don't compress at the cost of clarity.
