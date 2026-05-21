"""CAC arm + watcher behaviour.

`arm`'s default fires synchronously inline; `delayed=True` schedules a
watcher with state machine ARMED → fire (or bail) → done. Each watcher test
runs `server._watcher` directly with a `MockWriter` and an isolated cache
dir (`monkeypatch.setattr(server, "CACHE_DIR", tmp_path)`).
"""
from __future__ import annotations

import asyncio
import sys
from dataclasses import dataclass, field
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Callable

import pytest

_SRC = Path(__file__).resolve().parents[2] / "plugins" / "cac" / "mcp-servers" / "cac"
sys.path.insert(0, str(_SRC))
import server  # noqa: E402


@dataclass
class MockWriter:
    calls: list[tuple[str, str]] = field(default_factory=list)

    def stash(self) -> None:
        self.calls.append(("stash", ""))

    def submit(self, text: str) -> None:
        self.calls.append(("submit", text))


@pytest.fixture
def cache(monkeypatch, tmp_path):
    monkeypatch.setattr(server, "CACHE_DIR", tmp_path)
    return tmp_path


# ---------- arm() default (synchronous fire) ----------


def test_arm_default_fires_inline_without_marker(cache, monkeypatch):
    """Default (delayed=False) lands stash → /compact → continuation in order,
    inline, with no marker file written."""
    writer = MockWriter()
    monkeypatch.setattr(server, "detect_mux_writer", lambda: writer)

    result = asyncio.run(server.arm(focus="bar"))

    assert writer.calls == [
        ("stash", ""),
        ("submit", "/compact bar"),
        ("submit", "Continue until `swipe` triggers."),
    ]
    assert result["armed"] is True
    assert list(cache.iterdir()) == []


def test_arm_default_continuation_none_skips_trailing_submit(cache, monkeypatch):
    writer = MockWriter()
    monkeypatch.setattr(server, "detect_mux_writer", lambda: writer)

    asyncio.run(server.arm(focus="bar", continuation=None))

    assert writer.calls == [("stash", ""), ("submit", "/compact bar")]


# ---------- _watcher (delayed mode) ----------


async def _kick(at: float | None, fn: Callable[[], None]) -> None:
    if at is None:
        return
    await asyncio.sleep(at)
    fn()


async def _exercise(
    sid: str,
    *,
    continuation: str | None,
    fire_delay: float = 0.3,
    compact_bail_at: float | None = None,
) -> MockWriter:
    writer = MockWriter()
    fires_at = datetime.now(timezone.utc) + timedelta(seconds=fire_delay)
    compact_arm = server.CACHE_DIR / f"{sid}.cac.json"

    await asyncio.gather(
        server._watcher(sid, "focus", continuation, fires_at, writer),
        _kick(compact_bail_at, lambda: compact_arm.unlink(missing_ok=True)),
    )
    return writer


def _run(coro):
    return asyncio.run(coro)


def test_bail_pre_fire_skips_all_writes(cache):
    """ARMED → bail (compact arm deleted before fire) → no keystrokes."""
    writer = _run(_exercise("A", compact_bail_at=0.1, continuation="cont"))
    assert writer.calls == []


def test_happy_path_submits_compact_then_continuation(cache):
    """/compact and continuation submitted back-to-back; TUI buffers the
    continuation until compaction completes."""
    writer = _run(_exercise("B", continuation="cont"))
    assert writer.calls == [
        ("stash", ""),
        ("submit", "/compact focus"),
        ("submit", "cont"),
    ]


def test_finally_block_cleans_arm_file(cache):
    """The arm file must not survive the watcher's `finally`."""
    sid = "D"
    _run(_exercise(sid, continuation="cont"))
    assert list(cache.glob(f"{sid}.*")) == []
