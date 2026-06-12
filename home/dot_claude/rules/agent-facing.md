---
paths:
  - "**/dot_claude/CLAUDE.md"
  - "**/USER_INSTRUCTION.md"
  - "**/AGENTS.md"
  - "**/SKILL.md"
  - "**/commands/*.md"
  - "**/hooks/*.md"
---
Discipline for *agent-facing* prose — written for an agent to consume, not a human: an always-on instruction file (`AGENTS.md`/`CLAUDE.md`), skill and command bodies, file-scoped rules, hook output. The live files are ground truth: when this rule lags what they actually do, update the rule, not the reverse.

The instruction file is paid for on every turn of every session; commands, skills, and hooks are paid for on demand. **That cost asymmetry drives everything below.**

## When asked to add a rule

Apply this filter in order; stop at the first failure.

1. **Agent, not user?** Only agent-actionable instructions belong. (Plan Mode is fair game — the model enters it via the `EnterPlanMode` tool.)
2. **A real, recurring miss?** Point at a specific time it would have helped; "what's a good rule" in the abstract is the wrong signal.
3. **Against the model's default?** Rules matching default behavior earn no slot. Calibration: "don't investigate tool failures by reading source" → earns it; "write clean code" → dilution.
4. **Not already harness-surfaced?** Claude Code nudges around writes-without-reads, missing tests, and the like — don't restate them.
5. **No better home?** Per-turn cost is a tax; on-demand is free until used. Calibration: scoped to a workflow → skill; lifecycle event without judgment → hook; subtree-specific detail → a leaf `AGENTS.md` there; always-on posture against the default → inline here.

Passes all five? Add it — one sentence, imperative, negative paired with positive ("don't X; prefer Y").

## When asked to remove a rule

Drift is a feature: as training and harness shift, rules become defaults and stop earning their slot. Candidates — the model does it unprompted in a fresh project, a new project satisfies the rule without it, or it restates what the harness or system prompt now covers. Raise these with the user rather than cutting unilaterally; when asked to shorten, start with the now-default rules.

## On every edit: the high-effort pass

Touching any file this rule governs runs a deliberate, high-effort audit of the **whole file**, not just your diff — bloat accretes a line at a time. Three lenses:

1. **Audience.** Every line addressed to the agent? Operator steps it never runs — install, deploy, `apply`, promote — are human docs; move them to `README.md`. The tell: a command the file *itself* forbids the agent from running. Calibration: `chezmoi edit`, beside a line saying agents never run it → `README.md`; `./run-tests.sh`, the verb the agent verifies with → stays.
2. **Redundancy.** A line restating a fact stated elsewhere → cut to one. Calibration: a "Key constraints" list reprising the Architecture section → cut; a footgun reprised as a calibration → keep, different job.
3. **Altitude.** Subtree-specific detail belongs in a **leaf `AGENTS.md`** beside that subtree, loaded on demand — not taxing the always-on root every session. Push it down, leave a pointer. Calibration: a 40-line subsystem deep-dive → its own `<subtree>/AGENTS.md`; the prime directive every session leans on → root.

Fold small misfilings inline; a structural move (a section to a leaf doc, a human surface to `README.md`) is a refactor — surface it first.

## Style

State the rule directly. Prose over bullets (denser per token); reserve bullets for genuine lists. Absolutes when justified — "never investigate tool failures by reading source" beats a hedge. Pin fuzzy boundaries with `Calibration: <near miss> → <verdict>; <other> → <verdict>`. Gate rules that yield to house style in `<important unless="the condition that suspends them">…</important>`.

## Don't

- **Tone, persona, or preamble** — "be concise", "you are…", "the goal is…". Claude.ai preferences own tone; framing locks personality to one repo.
- **Code-style rules** — a PostToolUse linter enforces those deterministically; push back and suggest the hook.
- **Team-rollout content** — `managed-settings.json` or a project `team-conventions/`, not the user-level file.
- **Unilateral refactors** — surface "this could be a skill" first; the user prefers workshopping over surprise edits.
- **Compression past clarity** — shorter usually wins, but a rule that needs two sentences gets two.

## Before committing

Read the changed file end to end — cross-cutting moves need both files coherent — then re-run the pass above. No rule should name a slash command or UI mode; if it does, it's miscategorized.
