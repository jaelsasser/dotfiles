#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = ["mcp", "pydantic", "watchfiles"]
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
from pydantic import BaseModel, Field

from mux import MuxWriter, NoMuxWriterError, detect_mux_writer

mcp = FastMCP("helper")

CACHE_DIR = Path.home() / ".claude" / "cache"
SESSIONS_DIR = Path.home() / ".claude" / "sessions"
PROJECTS_DIR = Path.home() / ".claude" / "projects"

_POST_COMPACT_TIMEOUT_SECS = 300.0
_ENTERING_MSG = (
    "(CAC) Transcript compaction pending, "
    "ENTERING RESTRICTED MODE; tool calls restricted"
)

# session_id → live post-compact task. Re-arming a session cancels its prior task.
_watchers: dict[str, asyncio.Task[None]] = {}


class ReadEntry(BaseModel):
    """A targeted read the next-task agent should open on resume."""

    file: str = Field(description="Path to the file the agent should read")
    lines: str = Field(
        description="Line ranges to read, e.g. '1-30,45-90'. Never whole files."
    )
    reason: str = Field(
        default="", description="Short blurb explaining why this read matters"
    )


@mcp.tool()
async def cac(
    next_task: str,
    contracts: str = "",
    state: str = "",
    files: str = "",
    read: list[ReadEntry] | None = None,
) -> str:
    """Fire /compact with durable context, queue an agent kickoff for after compaction.

    Splits the brief between two surfaces. /compact's command-args become a
    verbatim user message in the post-compact transcript carrying durable
    context (contracts, state, files). The kickoff submitted after compaction
    completes is the agent's first prompt — it carries the next-task framing
    and the structured read list. The summarizer reads the full tool-call args
    via the transcript regardless of routing, so no need to duplicate.

    Args:
        next_task: One-paragraph framing of what the next agent picks up.
            Becomes the agent's kickoff prompt after compaction completes.
        contracts: Frozen API surfaces the next agent calls into — interfaces,
            signatures, behavioural notes from landed stages. Reproduce
            verbatim so the agent doesn't have to re-read source. → /compact.
        state: Baseline at compaction time — what passes, what's staged, what
            plan deviations were folded back, what names/shapes exist only in
            deliberation. → /compact.
        files: Files, plan documents, external docs the next agent should know
            exist. Path/URL only. → /compact.
        read: Targeted reads the agent should open first on resume. Each entry
            names a file, the specific line ranges, and an optional reason.
            Renders into the kickoff as a 'First read:' bullet list. → kickoff.
    """
    payload = _build_payload(contracts, state, files, fallback=next_task)
    kickoff = _build_kickoff(next_task, read or [])

    try:
        writer = detect_mux_writer()
    except NoMuxWriterError:
        raise RuntimeError(
            f"No multiplexer found. Run manually: `/compact {payload}`, "
            f"then after compaction: `{kickoff}`"
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

    # Push off the event loop: the writer's per-chunk timeout bounds a stall, but
    # even a bounded one would otherwise freeze the whole helper.
    await asyncio.to_thread(writer.stash)
    # Type only `/compact ` so the slash-command dispatch fires unambiguously;
    # paste the body so the TUI collapses it to a `[Pasted text +N lines]`
    # marker in scrollback rather than dumping the whole assembly visibly.
    await asyncio.to_thread(
        writer.submit,
        typed="/compact as per the agent-written guidance:\n\n",
        pasted=f"<agent-written>\n{payload}\n</agent-written>",
    )
    task = asyncio.create_task(_post_compact(writer, session_id, kickoff))
    _watchers[session_id] = task

    return _ENTERING_MSG


def _build_payload(contracts: str, state: str, files: str, *, fallback: str) -> str:
    """Assemble /compact's command-args from durable-context sections."""
    sections = []
    for label, content in (
        ("FROZEN CONTRACTS", contracts),
        ("STATE", state),
        ("FILES", files),
    ):
        if content:
            sections.append(f"{label}: {content}")
    return "\n\n".join(sections) if sections else fallback


def _build_kickoff(next_task: str, read: list[ReadEntry]) -> str:
    """Assemble the post-compact agent kickoff: next-task brief + first-reads."""
    parts = [next_task]
    if read:
        bullets = ["First read:"]
        for entry in read:
            suffix = f" — {entry.reason}" if entry.reason else ""
            bullets.append(f"- {entry.file}:{entry.lines}{suffix}")
        parts.append("\n".join(bullets))
    return "\n\n".join(parts)


async def _post_compact(
    writer: MuxWriter,
    session_id: str,
    kickoff: str,
) -> None:
    """Wait for the post-compact jsonl write, then submit the agent kickoff.

    Marker cleanup is handled by `cac.sh --done` (SessionStart matcher=compact)
    and `cac.sh --bail` (UserPromptSubmit); this task only owns the
    mux-side kickoff submission.
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

    await asyncio.sleep(0.1)
    await asyncio.to_thread(writer.stash)
    await asyncio.to_thread(
        writer.submit,
        typed="Continue.\n\n",
        pasted=f"<agent-written>\n{kickoff}\n</agent-written>",
    )


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
