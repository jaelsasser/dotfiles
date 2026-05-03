# AGENTS.md

You are maintaining `USER_CLAUDE.md` (symlinked to `~/.claude/CLAUDE.md`) and adjacent files (`.claude/commands/`, `.claude/skills/`, `.claude/hooks/`) in this dotfiles directory. This file encodes the design principles behind those files so edits stay consistent with their intent. The content rules and style conventions below apply to all Claude-facing prose and tool output in this directory — `USER_CLAUDE.md`, command bodies, skill bodies, and hook output alike.

## What these files are

- **`USER_CLAUDE.md`** — user-level instructions for Claude Code, loaded on every session across every project. Symlinked to `~/.claude/CLAUDE.md`. Project-level `CLAUDE.md` files override these on conflict.
- **`.claude/commands/*.md`** — slash commands (e.g. `/vet`, `/tutor`). Loaded only when the user invokes them.
- **`.claude/skills/<name>/SKILL.md`** — skills. Description loads at session start (~100 tokens); body loads only when triggered.
- **`.claude/hooks/*.sh`** — deterministic shell scripts wired in `~/.claude/settings.json` (event keys: `PreToolUse`, `PostToolUse`, `PreCompact`, `SessionStart`, etc.). The harness — not the model — invokes them on the matching event; output (e.g. `hookSpecificOutput.additionalContext`) gets injected into the model's input. Zero per-turn cost; fires only on the event. All hook scripts live in `claude/hooks/` and register in `claude/settings.json`. Note: `${CLAUDE_SKILL_DIR}` expands in SKILL.md *body* content but **not** in frontmatter `hooks:` block commands — skill-scoped hooks written that way silently break.

The `USER_CLAUDE.md` content is paid for on every turn of every session. The other files are paid for on demand. **This cost asymmetry drives most of the editing decisions below.**

## When asked to add to USER_CLAUDE.md

Apply this filter, in order. Stop at the first failure.

1. **Is this addressed to Claude or to the user?** Only Claude-actionable instructions belong here. References to keystrokes, slash command invocations, or terminal commands the user types are user-actionable and belong in tutor skill content, runbooks, or the user's own notes — not here. Examples of user-action smuggled in as Claude-action: "`/model opusplan`" (a slash command the user types), "press Shift+Tab" (a keystroke). (Plan Mode itself is fair game — the model enters it via the `EnterPlanMode` tool, so it's a Claude action.)

2. **Does it address a recurring failure mode?** New rules need a real miss to point at. "What's a good rule" in the abstract is the wrong authoring signal. If the user can't name a specific time the rule would have helped, it doesn't go in.

3. **Does it run against Claude's training default?** Rules that match what Claude does by default earn no slot. Posture rules ("don't investigate tool failures by reading source," "challenge stale constraints") earn their cost because the default leans the other way. Content like "write clean code" or "add comments to complex logic" is default behavior and pure dilution.

4. **Is the failure mode something the harness already surfaces?** Claude Code already nudges around writes-without-reads, missing tests, and similar. Rules duplicating harness-surfaced behavior are redundant.

5. **Could this live somewhere else with better economics?** If the rule applies only sometimes (e.g. only during planning, only when delegating, only for a specific domain), prefer a slash command or skill. If the rule fires on a specific lifecycle event and doesn't depend on model judgment ("preserve X during compaction", "run Y after every edit"), prefer a hook — the harness enforces it deterministically with zero per-turn cost. Per-turn cost is a tax; on-demand cost is free until used.

If a rule passes all five, add it. Prefer one sentence of imperative phrasing. Pair negatives with positives ("don't X; prefer Y"). Don't add headers for single-line sections.

## When asked to remove from USER_CLAUDE.md

Drift is a feature. As training data and harness behavior shift, rules become defaults and stop earning their slot. Watch for:

- The user mentions Claude doing the thing unprompted in a fresh project.
- A new project without the rule produces output that already satisfies it.
- The rule restates something the harness now surfaces or the system prompt now covers.

When you spot a candidate, raise it with the user. Don't remove unilaterally — the user has more context on whether they're still being bitten by the failure mode.

## When asked to add a slash command, skill, or hook

A satellite file is the right home when:
- The instruction applies only on demand (tutoring, plan vetting, handoff drafting) → slash command or skill.
- The instruction is long enough that inline cost would be wasteful → slash command or skill.
- The behavior should be opt-in, not ambient (the tutor skill is the canonical example — coaching the user wants when invoked, not coaching that fires whenever a "how do I…" question appears) → skill with auto-trigger disabled.
- The instruction fires on a deterministic lifecycle event and doesn't need model judgment → hook. Register in `claude/hooks/` + `settings.json`. (`${CLAUDE_SKILL_DIR}` does not expand in SKILL.md frontmatter `hooks:` commands, so skill-scoped frontmatter hooks silently fail — always use the global path.)

To prevent ambient triggering, prefer `disable-model-invocation: true` in the skill frontmatter — this blocks auto-invocation and removes the description from per-turn context (zero cost until invoked). Use it for any skill the user always invokes manually and where the description being in context provides no navigational value. The prose "Do not auto-trigger — only consult when /name is explicitly invoked" hint is the right fallback when you want the description visible (so Claude can surface the skill by name in suggestions) without ambient firing — `/tutor` is the current example.

Slash commands are appropriate even when an inline rule could cover the same ground, *if* the failure mode is forgetting to apply the rule. `/vet` exists as a backstop for plan-vetting because the failure mode (skipping the check) is exactly what an inline rule can't fix.

Both halves of a hook are dotfiled: script in `claude/hooks/`, registration in `claude/settings.json` (symlinked to `~/.claude/settings.json`). `./stow.sh claude` puts all of it in place — no manual re-wiring needed.

When a single behavior needs to fire on multiple distinct events (e.g. plan-mode prose injects on both `PreToolUse:EnterPlanMode` and `UserPromptSubmit`), wire one script to both events and pass the event name as `$1` so the script can branch on it. `SessionStart` does *not* receive `permission_mode` in its stdin; `UserPromptSubmit` does — useful when a hook needs to detect mode-state the harness only exposes per-prompt.

## When asked to add to the tutor skill catalog

The tutor skill is a coaching catalog, not a feature reference manual. New entries earn their slot only when:

1. The user has actually missed the habit in practice (or the user explicitly asks for it to be added based on someone else's miss).
2. The habit is user-side — a keystroke, command, or workflow the user controls.
3. The leverage is high enough to be worth surfacing as the *one* nudge the skill outputs per invocation.

Resist the pull toward exhaustiveness. A catalog of 30 habits dilutes signal; the tutor command outputs only one nudge per invocation, and a bloated catalog makes selection worse, not better. If a candidate addition isn't a habit the user *recurringly* misses, leave it out.

The "do not manufacture lessons" instruction in both `tutor.md` and `SKILL.md` is load-bearing. Preserve it on edits.

## Style conventions (all Claude-facing files)

- **Imperative voice, present tense.** "Delegate only when X" not "you should delegate when X."
- **Short sections.** Headers at `##` for top-level concerns; nested headers only when the section genuinely splits.
- **Bullets sparingly.** Prose is denser per token. Use bullets only for genuine lists where order doesn't matter or where the user benefits from scannability.
- **No persona, no preamble, no philosophy framing.** Don't start sections with "you are…" or "the goal is…". State the rule.
- **Absolutes are okay when justified.** "Never investigate tool failures by reading source" is more useful than "generally avoid investigating tool failures by reading source." Hedging dilutes.

## Validation before committing

After any edit:
- Read the changed file end to end. Cross-cutting changes (e.g. moving a rule from CLAUDE.md to a skill) need both files to be coherent.
- Check `USER_CLAUDE.md` line count. Soft ceiling ~75 lines; hard ceiling ~150. The user's research-backed instruction budget is roughly 100 slots after the harness's own ~50, so headroom is finite.
- Check that no rule in `USER_CLAUDE.md` references a slash command or UI mode by name. If it does, it's miscategorized.
- If a slash command or skill was added, confirm the corresponding entry exists in the tutor catalog *only if* the new file represents a habit the user is likely to forget to invoke. Not every new file needs a tutor entry.
- If a hook was added, confirm: (a) the script is executable (`chmod +x`), (b) it exits 0 when run standalone (hooks that return non-zero may block the tool call), and (c) the `settings.json` `command` path uses `$HOME` or `~` (both expand). Do not use `${CLAUDE_SKILL_DIR}` in hook command paths — it only expands in SKILL.md body content, not frontmatter.

## What not to do

- **Don't add tone or persona instructions** ("be concise", "be helpful"). The user's Claude.ai preferences cover tone; baking it in here is redundant and locks personality to one repo.
- **Don't add code-style rules.** They belong in PostToolUse hooks where a linter enforces them deterministically. If asked to add one, push back and suggest the hook.
- **Don't add team-rollout content to `USER_CLAUDE.md`.** That belongs in `managed-settings.json` or a project-level `team-conventions/` directory, not user-level.
- **Don't auto-trigger refactors.** If you notice an opportunity to restructure the file (e.g. "this could be a skill"), surface it as a suggestion before acting. The user has consistently preferred workshopping changes over accepting unilateral edits.
- **Don't optimize for length.** Shorter is usually better, but a rule that needs two sentences to be unambiguous gets two sentences. Don't compress at the cost of clarity.

## Testing claude/

Tests live under `claude/tests/` — `integration/` for end-to-end Haiku-agent smokes and `unit/` for surgical script-level coverage. Shared setup in `claude/tests/lib/helpers.bash`.

Curation rule: if the hot path doesn't need it, don't write it. Unit tests cover what integration smokes can't catch — marker logic, schema-validated hook output shapes, sub-plan cursor preservation. Per-framework or per-permutation coverage is dilution; cut it.

Test isolation: `helpers.bash` exports `CLAUDE_SEAMS_DIR=$BATS_TEST_TMPDIR/seams` so seam fixtures never touch the real `$HOME`. Any new seam-touching test that skips the helper is a bug.

Run with `bats -r claude/tests/`.

## Trigger → action quick reference

| When the user says… | Do this |
|---|---|
| "Add a rule that…" | Run the 5-question filter. If it fails, propose the right home (skill, command, hook) instead. |
| "I keep forgetting to…" | Likely a tutor catalog entry. Confirm it's user-side and recurring before adding. |
| "Claude keeps doing X wrong" | Likely a `USER_CLAUDE.md` rule. Confirm it runs against the training default. |
| "This rule feels redundant" | Drift check candidate. Test with a fresh project before removing. |
| "Make this shorter" | Audit for instructions Claude already follows by default; those are the first cuts. |
| "Should this be a skill?" | If on-demand, yes. If always-relevant, keep inline. If event-triggered and judgment-free, prefer a hook. |
