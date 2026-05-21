#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = ["mcp", "watchfiles"]
# ///
"""CAC reader: fires /compact at the mux."""

from __future__ import annotations

import asyncio
import contextlib
import json
import os
import subprocess
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import TypedDict

import watchfiles
from mcp.server.fastmcp import FastMCP

from mux import MuxWriter, NoMuxWriterError, detect_mux_writer

mcp = FastMCP("reader")

CACHE_DIR = Path.home() / ".claude" / "cache"
SESSIONS_DIR = Path.home() / ".claude" / "sessions"
PROJECTS_DIR = Path.home() / ".claude" / "projects"

_CACHE_TTL_SECS = 300
_TTL_MARGIN_SECS = 30
_FIRE_DELAY_SECS = _CACHE_TTL_SECS - _TTL_MARGIN_SECS
_DEFAULT_FOCUS = "strictly conform to the transcript's <summarization-instructions>"

# session_id → live watcher task. Re-arming a session cancels its prior task.
_watchers: dict[str, asyncio.Task[None]] = {}


class SwipeResult(TypedDict):
    armed: bool
    fires_at: str


@mcp.tool()
async def swipe(
    focus: str | None = None,
    continuation: str | None = "Continue until `cac:swipe` triggers.",
    delayed: bool = False,
) -> SwipeResult:
    """Fire /compact at the mux, then queue continuation.

    Args:
        focus: Optional extra context prepended to the standard summarization
            instruction. Omit for a bare swipe — the default focuses correctly.
        continuation: Queued after /compact. Pass None for a clean post-compact
            prompt.
        delayed: Veto-window mode: schedules fire ~30s before prompt-cache TTL;
            UserPromptSubmit hook cancels if operator types first.
    """
    resolved_focus = f"{focus}; {_DEFAULT_FOCUS}" if focus else _DEFAULT_FOCUS
    try:
        writer = detect_mux_writer()
    except NoMuxWriterError:
        raise RuntimeError(
            f"No multiplexer found. Run manually: `/compact {resolved_focus}`"
        )

    if not delayed:
        session_id = _resolve_session_id()
        transcript = _find_transcript(session_id)
        writer.stash()
        writer.submit(f"/compact {resolved_focus}")
        if continuation is not None:
            asyncio.create_task(_send_after_compact(writer, continuation, transcript))
        return SwipeResult(armed=True, fires_at=datetime.now(timezone.utc).isoformat())

    session_id = _resolve_session_id()
    fires_at = datetime.now(timezone.utc) + timedelta(seconds=_FIRE_DELAY_SECS)

    old = _watchers.get(session_id)
    if old is not None and not old.done():
        old.cancel()
        with contextlib.suppress(asyncio.CancelledError, asyncio.TimeoutError):
            await asyncio.wait_for(old, timeout=2.0)

    task = asyncio.create_task(
        _watcher(session_id, resolved_focus, continuation, fires_at, writer)
    )
    _watchers[session_id] = task

    return SwipeResult(armed=True, fires_at=fires_at.isoformat())


def _find_transcript(session_id: str) -> Path | None:
    for p in PROJECTS_DIR.rglob(f"{session_id}.jsonl"):
        return p
    return None


async def _send_after_compact(
    writer: MuxWriter,
    continuation: str,
    transcript: Path | None,
    timeout: float = 300.0,
) -> None:
    """Watch the session transcript for a write, then send continuation."""
    if transcript is None:
        await asyncio.sleep(10.0)
        writer.submit(continuation)
        return

    async def _watch() -> None:
        async for _ in watchfiles.awatch(str(transcript)):
            return

    with contextlib.suppress(asyncio.TimeoutError, Exception):
        await asyncio.wait_for(_watch(), timeout=timeout)

    writer.submit(continuation)


def _fire(writer: MuxWriter, focus: str) -> None:
    writer.stash()
    writer.submit(f"/compact {focus}")


async def _watcher(
    session_id: str,
    focus: str,
    continuation: str | None,
    fires_at: datetime,
    writer: MuxWriter,
) -> None:
    CACHE_DIR.mkdir(parents=True, exist_ok=True)
    compact_arm = CACHE_DIR / f"{session_id}.cac.json"

    try:
        _write_state(
            compact_arm,
            {
                "state": "ARMED",
                "fires_at": fires_at.isoformat(),
                "fires_at_epoch": int(fires_at.timestamp()),
            },
        )

        delay = max(0.0, (fires_at - datetime.now(timezone.utc)).total_seconds())
        await asyncio.sleep(delay)
        if not compact_arm.exists():
            return  # bailed: hook deleted the arm

        transcript = _find_transcript(session_id)
        _fire(writer, focus)
        if continuation is not None:
            await _send_after_compact(writer, continuation, transcript)
    finally:
        _watchers.pop(session_id, None)
        compact_arm.unlink(missing_ok=True)


def _write_state(path: Path, payload: dict) -> None:
    """Atomic write so `cac.sh` never reads a half-written JSON file."""
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(json.dumps(payload))
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
