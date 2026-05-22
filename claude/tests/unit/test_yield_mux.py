"""Mux detection and writer split-write framing."""
from __future__ import annotations

import os
import sys
from pathlib import Path

import pytest

_SRC = Path(__file__).resolve().parents[2] / "plugins" / "cac" / "mcp-servers" / "cac"
sys.path.insert(0, str(_SRC))
import mux  # noqa: E402


@pytest.fixture(autouse=True)
def _no_tmux_pane(monkeypatch):
    monkeypatch.delenv("TMUX_PANE", raising=False)


def _fake_ps(chain: list[tuple[int, int, list[str]]]):
    """[(pid, ppid, argv), ...] → drop-in for `mux._ps_lookup`."""
    table = {pid: (ppid, argv) for pid, ppid, argv in chain}
    return lambda pid: table.get(pid)


# ---------- detect_mux_writer ----------


def test_tmux_pane_shortcircuits(monkeypatch):
    monkeypatch.setenv("TMUX_PANE", "%42")
    writer = mux.detect_mux_writer()
    assert isinstance(writer, mux.TmuxWriter)
    assert writer.pane == "%42"


def test_dtach_walked_past_uv_interposer(monkeypatch):
    """`uv run --script` sits between Claude and us — detection must walk past."""
    monkeypatch.setattr(os, "getppid", lambda: 1000)
    monkeypatch.setattr(mux, "_ps_lookup", _fake_ps([
        (1000, 999, ["uv", "run", "--script", "server.py"]),
        (999, 998, ["claude"]),
        (998, 997, ["dtach", "-a", "/tmp/foo.sock", "zsh"]),
    ]))
    writer = mux.detect_mux_writer()
    assert isinstance(writer, mux.DtachWriter)
    assert writer.socket == "/tmp/foo.sock"


def test_abduco_session_extracted(monkeypatch):
    monkeypatch.setattr(os, "getppid", lambda: 1000)
    monkeypatch.setattr(mux, "_ps_lookup", _fake_ps([
        (1000, 999, ["abduco", "-A", "claude-session", "zsh"]),
    ]))
    writer = mux.detect_mux_writer()
    assert isinstance(writer, mux.AbducoWriter)
    assert writer.session == "claude-session"


def test_no_mux_raises(monkeypatch):
    monkeypatch.setattr(os, "getppid", lambda: 1000)
    monkeypatch.setattr(mux, "_ps_lookup", _fake_ps([(1000, 1, ["zsh"])]))
    with pytest.raises(mux.NoMuxWriterError):
        mux.detect_mux_writer()


# ---------- writer split-write framing ----------


def test_dtach_submit_wraps_pasted_body_and_splits_enter(monkeypatch):
    """Typed prefix prefixes the body; pasted body is bracket-paste-wrapped;
    Enter is a separate `dtach -p` invocation (distinct pty read)."""
    calls = []

    class _Result:
        returncode = 0

    def fake_run(argv, input=None, check=True):
        calls.append((tuple(argv), input))
        return _Result()

    monkeypatch.setattr(mux.subprocess, "run", fake_run)
    monkeypatch.setattr(mux.time, "sleep", lambda _: None)

    mux.DtachWriter(socket="/tmp/foo.sock").submit(typed="/compact ", pasted="bar")

    assert calls == [
        (("dtach", "-p", "/tmp/foo.sock"), b"/compact \x1b[200~bar\x1b[201~"),
        (("dtach", "-p", "/tmp/foo.sock"), b"\r"),
    ]


def test_abduco_submit_pasted_only_then_enter_then_detach(monkeypatch):
    """Pasted-only path (the kickoff): body is bracket-paste-wrapped, then \\r,
    then the 0x1c detach char — three distinct chunks at the inner pty."""
    writes: list[bytes] = []

    class _FakeStdin:
        def write(self, data): writes.append(data)
        def flush(self): pass
        def close(self): pass

    class _FakeProc:
        stdin = _FakeStdin()
        def wait(self, timeout=None): pass
        def kill(self): pass

    monkeypatch.setattr(mux.subprocess, "Popen", lambda *a, **k: _FakeProc())
    monkeypatch.setattr(mux.time, "sleep", lambda _: None)

    mux.AbducoWriter(session="sess").submit(pasted="bar")

    assert writes == [b"\x1b[200~bar\x1b[201~", b"\r", b"\x1c"]


def test_submit_requires_typed_or_pasted():
    with pytest.raises(ValueError):
        mux.DtachWriter(socket="/tmp/foo.sock").submit()
