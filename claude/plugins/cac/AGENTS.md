# AGENTS.md — cac

## What this plugin is

`cac` (compact-and-continue) gates `/compact` calls through an MCP tool that writes a restricted-mode marker, submits the compaction command to the terminal multiplexer, and queues an agent kickoff after compaction completes. Three hook events coordinate the lifecycle; two skills surface the flow to the agent.

## Files

| Path | Purpose |
|------|---------|
| `.claude-plugin/plugin.json` | Plugin manifest (name, description) |
| `.mcp.json` | MCP server registration — runs `mcp-servers/cac/server.py` via `uv run --script` |
| `hooks/hooks.json` | Hook wiring: `PreToolUse → --nag`, `UserPromptSubmit → --bail`, `SessionStart[compact] → --done` |
| `hooks/cac.sh` | Hook dispatcher — `--nag` blocks tool calls; `--bail` / `--done` remove the marker and inject status messages |
| `mcp-servers/cac/server.py` | MCP server: the `cac(...)` tool — writes marker, submits to mux, watches for post-compact jsonl |
| `mcp-servers/cac/mux.py` | Multiplexer abstraction — `TmuxWriter`, `DtachWriter`, `AbducoWriter`, `detect_mux_writer()` |
| `mcp-servers/cac/README.md` | MCP server reference: deps, mux protocol, state file layout, restricted-mode messages |
| `skills/compact-and-continue/SKILL.md` | Model-triggered skill that calls `mcp__plugin_cac_helper__cac` |
| `skills/yield/SKILL.md` | User-invocable fallback that emits a copy-pasteable `/compact …` line |
| `RFC.md` | Token economics, break-even equation, design rationale |

## Runtime artifacts

| Path | Lifetime |
|------|---------|
| `~/.claude/cache/{sid}.cac.json` | Written before `/compact` lands at the mux; removed by `--done` (post-compact `SessionStart`) or `--bail` (operator keystroke) |

## Key constraints

- **Mux required for the MCP path.** `detect_mux_writer()` walks the PPID chain for tmux (`$TMUX_PANE`), dtach, or abduco. No match → `NoMuxWriterError`; use `cac:yield` instead.
- **Marker must land before tool returns.** `server.py` writes the marker synchronously before `stash()` or `submit()`, so the next `PreToolUse` hook sees it even if Claude re-enters mid-call.
- **Hooks no-op on absent marker.** All three flags exit cleanly when no marker file exists, which keeps `--done` quiet for `/compact` calls not originated by this plugin.
