# cac MCP server

`arm(focus, continuation=..., delayed=False)` submits `/compact <focus>` to
the terminal multiplexer. The default fires synchronously: by the time the
tool returns, `stash → /compact → continuation` have already landed at the
mux in that order. Claude Code's TUI buffers the continuation and replays
it once compaction completes.

`delayed=True` is the niche opt-in for when an operator is at the keyboard
and you want them to have a veto window. The call writes
`{sid}.cac.json` under `~/.claude/cache/` and schedules a watcher to fire
~30s before the prompt-cache TTL expires. A `UserPromptSubmit` hook
(`cac.sh --bail`) deletes the marker, so any operator prompt before fire
cancels the watcher. A `Stop` hook (`cac.sh --check`) defends the armed
window against agent re-engagement, with a 5s mtime grace so the agent's
own acknowledgement Stop ends its turn naturally.

## Install

Bundled with the `cac` plugin (`plugins/cac/`); `configure.sh` registers
the server via `claude mcp add cac --scope user`, so `claude mcp list`
should show `cac ✓ Connected` after a re-stow.

## Runtime requirements

| Dep | Notes |
|---|---|
| `uv` | The server runs as a `uv run --script` PEP 723 single-file script; the `mcp` dep is pinned in the header. |
| Python 3.11+ | Driven by uv. |
| `jq` | Used by `cac.sh`. |
| A supported multiplexer | tmux, dtach, or abduco in the process ancestry. No mux → `NoMuxWriterError` and the swipe skill falls back to copy-paste prose. |

## Supported multiplexers

| Mux | Detection | Injection |
|---|---|---|
| tmux | `$TMUX_PANE` env var | `tmux send-keys -l <text>` + `tmux send-keys Enter` (two calls; literal text guard) |
| dtach | argv match in PPID chain | `dtach -p $SOCKET` with text, then `\r` as a second pipe (separate pty reads — single-burst writes get treated as embedded newline) |
| abduco | argv match in PPID chain | `abduco -a $SESSION` attach-detach with chunked stdin + 0x1c detach char. Stable 0.6 lacks `-p`; see [martanne/abduco#49](https://github.com/martanne/abduco/issues/49). |

## State files (delayed mode only)

The synchronous default writes no state. `delayed=True` writes one arm file
under `~/.claude/cache/`:

| File | Phase | Removed by |
|---|---|---|
| `{sid}.cac.json` | ARMED (pre-fire) | `cac.sh --bail` on operator submit, or the watcher's `finally` |

Bail is deletion-based (idempotent under repeated hook fires). The watcher's
post-fire window (`writer.stash()` + two `writer.submit()` calls) is
microseconds long, so there's no meaningful cancellation target after fire —
the continuation lands as buffered stdin and Claude Code's TUI replays it
once compaction completes.

## Hooks

`plugins/cac/hooks/cac.sh` dispatches on `--check` and `--bail`. Both only act
on delayed-mode arms — the sync default writes no marker, so these hooks
are no-ops for it.

| Flag | Event | Role |
|---|---|---|
| `--bail` | `UserPromptSubmit` | Removes `${sid}.cac.json` — universal pre-fire cancellation primitive. |
| `--check` | `Stop` | If the arm marker exists and its mtime is older than the 5s grace window, emits `decision: block` with a terse reason ("CAC armed: /compact fires in M:SS, halt"). The 5s grace lets the agent's own acknowledgement Stop pass through naturally; only re-engagement within the armed window is blocked. |

## Disabling

Drop `allowed-tools: mcp__cac__arm` from `plugins/cac/skills/swipe/SKILL.md` and
unregister with `claude mcp remove cac`. The `swipe` skill falls back to
printing a copy-pasteable `/compact ...` line on `NoMuxWriterError`, so
disabling the MCP path doesn't break the skill.
