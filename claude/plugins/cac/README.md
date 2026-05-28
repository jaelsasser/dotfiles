# cac — compact-and-continue

A Claude Code plugin that arms `/compact` from an MCP tool, gates tool calls during compaction, and submits an agent kickoff once the new prefix is live.

## Install

```
claude plugin install -s user cac@dotfiles
```

`configure.sh` runs this automatically on re-stow. After install, `claude mcp list` shows `cac ✓ Connected`.

## Skills

| Skill | Invocation | What it does |
|-------|-----------|--------------|
| `cac:compact-and-continue` | Model-triggered | Calls the `cac` MCP tool; fills durable-context fields and submits to the mux |
| `cac:yield` | `/cac:yield [guidance]` | Emits a copy-pasteable `/compact …` line for mux-less sessions |

## Hooks

| Event | Flag | Behaviour |
|-------|------|-----------|
| `PreToolUse` | `--nag` | Hard-blocks every tool call while the restricted-mode marker is live |
| `UserPromptSubmit` | `--bail` | Removes the marker, injects EXITING-cancelled |
| `SessionStart` (`matcher: compact`) | `--done` | Removes the marker, injects EXITING-complete |

All three no-op when the marker is absent — `--done` stays quiet for `/compact` calls not originated by this plugin.

## MCP server

`mcp-servers/cac/server.py` exposes one tool (`cac`) that writes a restricted-mode marker, submits `/compact <payload>` to the terminal multiplexer, and queues the post-compact kickoff. Requires tmux, dtach, or abduco in the process ancestry; no mux → `NoMuxWriterError`, fall back to `cac:yield`.

Full deps table, multiplexer protocol, and state file layout: `mcp-servers/cac/README.md`.

## Disabling

Remove the MCP path only: `claude mcp remove cac`, then drop `allowed-tools: mcp__plugin_cac_helper__cac` from `skills/compact-and-continue/SKILL.md`. The `cac:yield` skill stays functional without the MCP server.

## Background

Token economics, break-even equation, `CLAUDE.md` preconditions: `RFC.md`.
