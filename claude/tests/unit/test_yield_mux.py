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

    def fake_run(argv, input=None, check=True, timeout=None):
        calls.append((tuple(argv), input))
        return _Result()

    monkeypatch.setattr(mux.subprocess, "run", fake_run)
    monkeypatch.setattr(mux.time, "sleep", lambda _: None)

    mux.DtachWriter(socket="/tmp/foo.sock").submit(typed="/compact ", pasted="bar")

    # A sub-chunk-sized body stays a single write — Enter is the only split.
    assert calls == [
        (("dtach", "-p", "/tmp/foo.sock"), b"/compact \x1b[200~bar\x1b[201~"),
        (("dtach", "-p", "/tmp/foo.sock"), b"\r"),
    ]


def test_dtach_submit_chunks_large_body_under_queue(monkeypatch):
    """A body over the push-chunk size splits into ordered <= _PUSH_CHUNK writes,
    with Enter a final separate push — the fix for the single-threaded-master
    deadlock that a monolithic >1KB write triggers on long continuations."""
    inputs = []

    class _Result:
        returncode = 0

    def fake_run(argv, input=None, check=True, timeout=None):
        inputs.append(input)
        return _Result()

    monkeypatch.setattr(mux.subprocess, "run", fake_run)
    monkeypatch.setattr(mux.time, "sleep", lambda _: None)

    big = "x" * (mux._PUSH_CHUNK * 2 + 100)
    mux.DtachWriter(socket="/tmp/foo.sock").submit(pasted=big)

    body = mux._wrap_paste(big).encode()
    *body_writes, enter = inputs
    assert enter == b"\r"
    assert all(len(w) <= mux._PUSH_CHUNK for w in body_writes)
    assert b"".join(body_writes) == body
    assert len(body_writes) == -(-len(body) // mux._PUSH_CHUNK)  # ceil-div


def test_abduco_submit_chunks_large_body(monkeypatch):
    """The attach client forwards stdin into the same pty queue, so a large body
    is sub-chunked under _PUSH_CHUNK before the trailing \\r and detach char."""
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

    big = "y" * (mux._PUSH_CHUNK * 2 + 50)
    mux.AbducoWriter(session="sess").submit(pasted=big)

    assert writes[-2:] == [b"\r", b"\x1c"]  # Enter, then detach char
    body_writes = writes[:-2]
    assert all(len(w) <= mux._PUSH_CHUNK for w in body_writes)
    assert b"".join(body_writes) == mux._wrap_paste(big).encode()


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
