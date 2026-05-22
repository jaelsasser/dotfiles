#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = ["mcp", "watchfiles"]
# ///
"""CAC helper: fires /compact at the mux."""

from __future__ import annotations

import asyncio
import contextlib
import json
import os
import subprocess
from datetime import datetime, timezone
from pathlib import Path

import watchfiles
from mcp.server.fastmcp import FastMCP

from mux import MuxWriter, NoMuxWriterError, detect_mux_writer

mcp = FastMCP("helper")

CACHE_DIR = Path.home() / ".claude" / "cache"
SESSIONS_DIR = Path.home() / ".claude" / "sessions"
PROJECTS_DIR = Path.home() / ".claude" / "projects"

_DEFAULT_FOCUS = "strictly conform to the transcript's <summarization-instructions>"
_POST_COMPACT_TIMEOUT_SECS = 300.0
_ENTERING_MSG = (
    "(CAC) Transcript compaction pending, "
    "ENTERING RESTRICTED MODE; tool calls restricted"
)

# session_id → live post-compact task. Re-arming a session cancels its prior task.
_watchers: dict[str, asyncio.Task[None]] = {}


@mcp.tool()
async def cac(
    focus: str | None = None,
    continuation: str | None = "Continue",
) -> str:
    """Fire /compact at the mux, then queue continuation.

    Args:
        focus: Optional extra context prepended to the standard summarization
            instruction. Omit for a bare call — the default focuses correctly.
        continuation: Queued after /compact. Pass None for a clean post-compact
            prompt.
    """
    resolved_focus = f"{focus}; {_DEFAULT_FOCUS}" if focus else _DEFAULT_FOCUS
    try:
        writer = detect_mux_writer()
    except NoMuxWriterError:
        raise RuntimeError(
            f"No multiplexer found. Run manually: `/compact {resolved_focus}`"
        )

    session_id = _resolve_session_id()

    # Supersede any in-flight watcher before writing the marker — its finally
    # is otherwise free of side effects on the marker, but cancelling first
    # keeps the lifecycle linear.
    old = _watchers.get(session_id)
    if old is not None and not old.done():
        old.cancel()
        with contextlib.suppress(asyncio.CancelledError, asyncio.TimeoutError):
            await asyncio.wait_for(old, timeout=2.0)

    # Marker MUST land on disk before the tool returns so the next PreToolUse
    # hook fires the nag-block; do it before stash/submit/create_task too so a
    # gung-ho model that re-enters mid-call still trips the block.
    _write_marker(session_id)

    writer.stash()
    writer.submit(f"/compact {resolved_focus}")
    task = asyncio.create_task(_post_compact(writer, session_id, continuation))
    _watchers[session_id] = task

    return _ENTERING_MSG


async def _post_compact(
    writer: MuxWriter,
    session_id: str,
    continuation: str | None,
) -> None:
    """Wait for the post-compact jsonl write, then submit the continuation.

    Marker cleanup is handled by `cac.sh --done` (SessionStart matcher=compact)
    and `cac.sh --bail` (UserPromptSubmit); this task only owns the
    mux-side continuation submission.
    """
    jsonl_name = f"{session_id}.jsonl"

    async def _watch() -> None:
        async for changes in watchfiles.awatch(str(PROJECTS_DIR), recursive=True):
            for _, path in changes:
                if Path(path).name == jsonl_name:
                    return

    try:
        with contextlib.suppress(asyncio.TimeoutError, Exception):
            await asyncio.wait_for(_watch(), timeout=_POST_COMPACT_TIMEOUT_SECS)
    finally:
        _watchers.pop(session_id, None)

    if continuation is not None:
        asyncio.sleep(0.1)
        writer.stash()
        writer.submit(continuation)


def _write_marker(session_id: str) -> None:
    """Atomic write of the restricted-mode marker. PreToolUse hook checks existence."""
    CACHE_DIR.mkdir(parents=True, exist_ok=True)
    path = CACHE_DIR / f"{session_id}.cac.json"
    tmp = path.with_suffix(path.suffix + ".tmp")
    written_at = datetime.now(timezone.utc)
    tmp.write_text(
        json.dumps(
            {
                "session_id": session_id,
                "written_at": written_at.isoformat(),
                "written_at_epoch": int(written_at.timestamp()),
            }
        )
    )
    os.replace(tmp, path)


def _resolve_session_id() -> str:
    """Read the sessionId Claude Code stamps for our process ancestry.

    Walks the PPID chain so interposing processes (notably `uv run --script`,
    which sits between Claude Code and this script) don't confuse the lookup.
    """
    pid = os.getppid()
    while pid > 1:
        session_file = SESSIONS_DIR / f"{pid}.json"
        if session_file.exists():
            return json.loads(session_file.read_text())["sessionId"]
        result = subprocess.run(
            ["ps", "-p", str(pid), "-o", "ppid="],
            capture_output=True,
            text=True,
            check=False,
        )
        try:
            pid = int(result.stdout.strip())
        except ValueError:
            break
    raise RuntimeError("no Claude Code session file in process ancestry")


if __name__ == "__main__":
    mcp.run()
