# AGENTS.md

This package holds the Claude Code configuration: `USER_CLAUDE.md` (symlinked to `~/.claude/CLAUDE.md`), skills under `skills/`, subagents under `agents/`, hooks + `settings.json` wiring under `hooks/`, the `cac` plugin under `plugins/`, and glob-scoped rules under `rules/`.

## What these files are

- **`USER_CLAUDE.md`** — user-level instructions, loaded every session across every project. Symlinked to `~/.claude/CLAUDE.md`; project-level `CLAUDE.md` overrides on conflict.
- **`skills/<name>/SKILL.md`** — skills. Description loads at session start (~100 tokens); body loads only when triggered.
- **`agents/<name>.md`** — Claude Code subagents. Frontmatter (`name`, `description`, `tools`, `model`) registers the agent and gates its tool surface by allowlist; body is the agent's system prompt. Stowed to `~/.claude/agents/`. Dispatched via the Task tool by `subagent_type`.
- **`rules/*.md`** — claudeMd extensions. With `paths:` frontmatter (a YAML list) they load only when editing matching files; without it they load every session. Stowed to `~/.claude/rules/`. (`globs:` is *not* recognized — it silently loads the rule always-on.)
- **`hooks/`, `settings.json`** — deterministic harness wiring (`inject.sh` + event matchers).

## Editing Claude-facing prose

Conventions for editing the instructional prose here — `USER_CLAUDE.md`, command bodies, skill bodies, hook output — live in `rules/agent-for-agent.md`, injected automatically when you edit those files. Read it before changing any of them.

## Testing claude/

Tests live under `claude/tests/unit/` — surgical script-level coverage for the bits that need it (today: `test_yield_mux.py` exercises the cac plugin's multiplexer detection and keystroke injection). Run via `./run-tests.sh` from the repo root, which dispatches to bats (for `stow.bats`) and pytest under uv (for `claude/tests/unit/*.py`).

Curation rule: if the hot path doesn't need it, don't write it. Per-framework or per-permutation coverage is dilution; cut it.
