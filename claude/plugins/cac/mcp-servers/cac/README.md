# cac MCP server

`continue(focus, continuation=...)` submits `/compact <focus>` to the
terminal multiplexer, queues a continuation prompt for after compaction,
and returns the ENTERING message announcing restricted mode. By the time
the tool returns, the restricted-mode marker is on disk and
`stash → /compact` have already landed at the mux. Claude Code's TUI
buffers the continuation and replays it once the post-compact jsonl write
fires.

`continue` is a Python keyword; the MCP tool name is set explicitly via
`@mcp.tool(name="continue")` while the Python identifier is `cac_continue`.

## The restricted-mode protocol

Three messages bracket the restricted-mode window. The model sees all
three in band:

| Message | Source | Trigger |
|---|---|---|
| `(CAC) Transcript compaction pending, ENTERING RESTRICTED MODE; tool calls restricted` | `cac_continue()` return value | Called from `/cac:condense` |
| `(CAC) Transcript compaction cancelled, EXITING RESTRICTED MODE; tool calls allowed` | `cac.sh --bail` via `UserPromptSubmit.additionalContext` | Operator typed before compaction completed |
| `(CAC) Transcript compaction complete, EXITING RESTRICTED MODE; tool calls allowed` | `cac.sh --done` via stdout on `SessionStart` matcher=compact | Compaction completed cleanly |

A fourth message — the `--nag` PreToolUse reason — repeats on every tool
call attempt while the marker is live and is not a transition.

## Install

Bundled with the `cac` plugin (`plugins/cac/`); `configure.sh` registers
the server via `claude mcp add cac --scope user`, so `claude mcp list`
should show `cac ✓ Connected` after a re-stow.

## Runtime requirements

| Dep | Notes |
|---|---|
| `uv` | The server runs as a `uv run --script` PEP 723 single-file script; deps are pinned in the header. |
| Python 3.11+ | Driven by uv. |
| `jq` | Used by `cac.sh`. |
| A supported multiplexer | tmux, dtach, or abduco in the process ancestry. No mux → `NoMuxWriterError` and the `cac:condense` skill falls back to copy-paste prose via the `cac:yield` user-invocable sibling. |

## Supported multiplexers

| Mux | Detection | Injection |
|---|---|---|
| tmux | `$TMUX_PANE` env var | `tmux send-keys -l <text>` + `tmux send-keys Enter` (two calls; literal text guard) |
| dtach | argv match in PPID chain | `dtach -p $SOCKET` with text, then `\r` as a second pipe (separate pty reads — single-burst writes get treated as embedded newline) |
| abduco | argv match in PPID chain | `abduco -a $SESSION` attach-detach with chunked stdin + 0x1c detach char. Stable 0.6 lacks `-p`; see [martanne/abduco#49](https://github.com/martanne/abduco/issues/49). |

## State files

Every `cac_continue()` call writes one marker under `~/.claude/cache/`:

| File | Written by | Removed by |
|---|---|---|
| `{sid}.cac.json` | `cac_continue()`, atomically before the tool returns | `cac.sh --done` on `SessionStart` matcher=compact; `cac.sh --bail` on operator `UserPromptSubmit` |

The marker is purely a presence flag for `--nag` to gate on; its JSON
payload (`session_id`, `written_at`, `written_at_epoch`) is diagnostic.
The `_post_compact` task inside the MCP server no longer touches the
marker — it only owns the post-compact continuation submission.

## Hooks

`plugins/cac/hooks/cac.sh` dispatches on `--nag`, `--bail`, and `--done`.

| Flag | Event | Role |
|---|---|---|
| `--nag` | `PreToolUse` | If the marker exists, emits `hookSpecificOutput.permissionDecision: deny` with a reason telling the model that the session is in RESTRICTED MODE and it must end its turn. Pure file-existence check. |
| `--bail` | `UserPromptSubmit` | If the marker exists, `rm -f`s it and emits the EXITING-cancelled message via `hookSpecificOutput.additionalContext`. No-op when absent. |
| `--done` | `SessionStart` matcher=compact | If the marker exists, `rm -f`s it and emits the EXITING-complete message via stdout. No-op when absent (the `/compact` came from somewhere other than `/cac:condense`). |

## Disabling

Drop `allowed-tools: mcp__plugin_cac_server__continue` from
`plugins/cac/skills/compact/SKILL.md` and unregister with
`claude mcp remove cac`. The `cac:yield` skill is the operator-invocable
fallback that emits a copy-pasteable `/compact ...` line, so disabling the
MCP path leaves a working manual path behind.
