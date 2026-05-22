# cac MCP helper

`cac(...)` submits `/compact <payload>` to the terminal multiplexer, queues an agent kickoff for after compaction, and returns the ENTERING message announcing restricted mode. By the time the tool returns, the restricted-mode marker is on disk and `stash → /compact` have already landed at the mux. Claude Code's TUI buffers the kickoff and replays it once the post-compact jsonl write fires.

The tool **splits the agent's brief between two surfaces**:

- **`/compact` command-args** carry durable context the next-task agent should have permanently in scope — `contracts`, `state`, `files`. This becomes a verbatim user message in the post-compact transcript.
- **Post-compact kickoff** is the agent's first prompt — `next_task` (one-paragraph framing) plus a structured `read` list rendered as a `First read:` bullet block. The summarizer reads the full tool-call args via the transcript regardless, so the next-task framing doesn't need duplication into `/compact` for summary visibility.

`read` is typed: `list[ReadEntry]` where each entry is `{file, lines, reason?}`. See `cac()`'s docstring in `server.py` for per-arg semantics.

## Restricted-mode protocol

A marker file under `~/.claude/cache/` gates tool calls between `/compact` submission and post-compact `SessionStart`. The model sees three in-band transition messages — ENTERING (returned from `cac()`), EXITING-cancelled (from `cac.sh --bail` if the operator types before compaction completes), and EXITING-complete (from `cac.sh --done` after the post-compact `SessionStart`). While the marker is live, the `--nag` `PreToolUse` hook hard-blocks every tool call attempt with a reason telling the model to end its turn.

## Install

Bundled with the `cac` plugin (`plugins/cac/`); `configure.sh` registers the server via `claude mcp add cac --scope user`, so `claude mcp list` should show `cac ✓ Connected` after a re-stow.

## Runtime requirements

| Dep | Notes |
|---|---|
| `uv` | The server runs as a `uv run --script` PEP 723 single-file script; deps are pinned in the header. |
| Python 3.11+ | Driven by uv. |
| `jq` | Used by `cac.sh`. |
| A supported multiplexer | tmux, dtach, or abduco in the process ancestry. No mux → `NoMuxWriterError` and the `cac:compact-and-continue` skill falls back to copy-paste prose via the `cac:yield` user-invocable sibling. |

## Supported multiplexers

| Mux | Detection | Injection |
|---|---|---|
| tmux | `$TMUX_PANE` env var | `tmux send-keys -l <text>` + `tmux send-keys Enter` (two calls; literal text guard) |
| dtach | argv match in PPID chain | `dtach -p $SOCKET` with text, then `\r` as a second pipe (separate pty reads — single-burst writes get treated as embedded newline) |
| abduco | argv match in PPID chain | `abduco -a $SESSION` attach-detach with chunked stdin + 0x1c detach char. Stable 0.6 lacks `-p`; see [martanne/abduco#49](https://github.com/martanne/abduco/issues/49). |

## State files

Every `cac()` call writes one marker under `~/.claude/cache/{sid}.cac.json`, atomically, before the tool returns. The marker is purely a presence flag for `--nag` to gate on; its JSON payload (`session_id`, `written_at`, `written_at_epoch`) is diagnostic. `cac.sh --done` removes it on post-compact `SessionStart`; `cac.sh --bail` removes it on operator `UserPromptSubmit`. The `_post_compact` task inside the MCP server only owns the post-compact continuation submission.

## Hooks

`plugins/cac/hooks/cac.sh` dispatches on `--nag`, `--bail`, and `--done` — see the file header for per-flag wiring. All three are pure file-existence checks on the marker and no-op when absent (which keeps `--done` quiet when the `/compact` came from somewhere other than `cac:compact-and-continue`).

## Disabling

Drop `allowed-tools: mcp__plugin_cac_helper__cac` from `plugins/cac/skills/compact-and-continue/SKILL.md` and unregister with `claude mcp remove cac`. The `cac:yield` skill is the operator-invocable fallback that emits a copy-pasteable `/compact ...` line, so disabling the MCP path leaves a working manual path behind.
