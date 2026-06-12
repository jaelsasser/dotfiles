# AGENTS.md

The Claude Code configuration. The **deployed** config lives in `home/dot_claude/` as ordinary chezmoi files (→ `home/AGENTS.md`); this repo-root `claude/` tree holds the **dev-only** pieces: the `cac`/`diat` plugins, the `settings.json` merge source, `rubric.*`, and `tests/`.

## Deployed config (`home/dot_claude/`)

- **`USER_CLAUDE.md`** — user-level instructions, loaded every session across every project. Deploys with `~/.claude/CLAUDE.md` symlinked onto it; project-level `CLAUDE.md` overrides on conflict.
- **`skills/<name>/SKILL.md`** — skills. Description loads at session start (~100 tokens); body loads only when triggered.
- **`agents/<name>.md`** — Claude Code subagents. Frontmatter (`name`, `description`, `tools`, `model`) registers the agent and gates its tool surface by allowlist; body is the system prompt. Dispatched via the Task tool by `subagent_type`.
- **`rules/*.md`** — claudeMd extensions. With `paths:` frontmatter (a YAML list) they load only when editing matching files; without it, every session. (`globs:` is *not* recognized — it silently loads the rule always-on.)
- **`hooks/`** — deterministic harness wiring (`executable_inject.sh` + event matchers), referenced by the merged `settings.json`.

Conventions for editing the instructional prose — `CLAUDE.md`, skill bodies, hook output, these `AGENTS.md` files — live in `home/dot_claude/rules/agent-facing.md`, injected automatically when you edit those files. Read it before changing any of them.

## Dev tree (`claude/`)

- **`plugins/`** — the `cac` and `diat` plugins; the marketplace self-registers and installs via `run_onchange_after_claude-plugins.sh.tmpl` on manifest change.
- **`settings.json`** — the `.hooks`/`.permissions`/`.env` merge source for `home/dot_claude/modify_settings.json.tmpl` (→ `home/AGENTS.md`).
- **`tests/unit/`** — surgical script-level coverage (today: `test_yield_mux.py` exercises the cac plugin's multiplexer). Run via `./run-tests.sh` (bats + pytest under uv). Curation: if the hot path doesn't need it, don't write it.
