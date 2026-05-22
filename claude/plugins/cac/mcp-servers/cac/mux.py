"""Multiplexer detection and keystroke injection.

`MuxWriter` is the Protocol the watcher writes through. Three concrete impls
cover the supported multiplexer set; `detect_mux_writer` walks the MCP server's
PPID chain to pick one, or raises `NoMuxWriterError` if none match.
"""

from __future__ import annotations

import os
import subprocess
import time
from dataclasses import dataclass
from typing import Protocol

_KEY_DELAY = 0.1  # seconds between text and the Enter keystroke

# Bracket-paste mode wrappers. The TUI honours these and collapses the visible
# representation into a `[Pasted text +N lines]` marker; the underlying message
# content is unaffected. Claude Code's own bg-worker reply path uses the same
# wrapping, so slash-command dispatch on pasted content is confirmed.
_PASTE_BEGIN = "\x1b[200~"
_PASTE_END = "\x1b[201~"


def _wrap_paste(text: str) -> str:
    return f"{_PASTE_BEGIN}{text}{_PASTE_END}"


class NoMuxWriterError(RuntimeError):
    """No supported multiplexer found in the process ancestry."""


class MuxWriter(Protocol):
    def stash(self) -> None: ...  # C-s, no Enter
    def submit(self, *, typed: str = "", pasted: str = "") -> None: ...


def _compose(typed: str, pasted: str) -> str:
    """Compose a submit-time payload: typed prefix first, then bracket-pasted
    body. Either may be empty but not both. The TUI renders the pasted portion
    as `[Pasted text +N lines]` in scrollback while the underlying input buffer
    holds the full expansion."""
    if not typed and not pasted:
        raise ValueError("submit requires typed and/or pasted")
    return typed + (_wrap_paste(pasted) if pasted else "")


@dataclass
class TmuxWriter:
    pane: str

    def stash(self) -> None:
        subprocess.run(["tmux", "send-keys", "-t", self.pane, "C-s"], check=True)

    def submit(self, *, typed: str = "", pasted: str = "") -> None:
        # -l sends the bytes literally — both the typed prefix and the
        # bracket-paste escape sequences pass through to the inner pty intact.
        body = _compose(typed, pasted)
        subprocess.run(
            ["tmux", "send-keys", "-t", self.pane, "-l", body], check=True
        )
        time.sleep(_KEY_DELAY)
        subprocess.run(["tmux", "send-keys", "-t", self.pane, "Enter"], check=True)


@dataclass
class DtachWriter:
    socket: str

    def stash(self) -> None:
        subprocess.run(["dtach", "-p", self.socket], input=b"\x13", check=True)

    def submit(self, *, typed: str = "", pasted: str = "") -> None:
        # The body and the Enter key MUST arrive as separate reads at the inner
        # pty; otherwise TUIs read the burst as a single chunk and the embedded
        # \r is treated as an in-buffer newline instead of an Enter keypress.
        # Two `dtach -p` calls produce two distinct writes to the pty master.
        body = _compose(typed, pasted).encode()
        subprocess.run(["dtach", "-p", self.socket], input=body, check=True)
        time.sleep(_KEY_DELAY)
        subprocess.run(["dtach", "-p", self.socket], input=b"\r", check=True)


@dataclass
class AbducoWriter:
    """Inject input into an abduco session.

    abduco 0.6 (the stable release Homebrew ships) lacks the `-p` flag; only
    HEAD has it. We attach as a transient multi-client peer via `-a`, write
    the payload through stdin, and after a short forwarding delay send the
    detach character (^\\, 0x1c) so the attach client exits cleanly without
    needing an external kill. Our attach's terminal-init sequences land on
    `stdout=DEVNULL` and don't bleed into the user's existing client.
    """

    session: str
    _FORWARD_DELAY = 0.3  # seconds: lets the payload reach the pty before detach

    def _send(self, *chunks: bytes) -> None:
        """Write `chunks` to the attach client's stdin with `_KEY_DELAY` between
        each, then the detach char to exit. Each chunk becomes a separate read
        at the inner pty, so callers can split text from the Enter keystroke."""
        proc = subprocess.Popen(
            ["abduco", "-a", self.session],
            stdin=subprocess.PIPE,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        try:
            assert proc.stdin is not None
            for i, chunk in enumerate(chunks):
                if i > 0:
                    time.sleep(_KEY_DELAY)
                proc.stdin.write(chunk)
                proc.stdin.flush()
            time.sleep(self._FORWARD_DELAY)
            proc.stdin.write(b"\x1c")
            proc.stdin.close()
            proc.wait(timeout=2.0)
        except (OSError, subprocess.TimeoutExpired):
            proc.kill()
            raise

    def stash(self) -> None:
        self._send(b"\x13")

    def submit(self, *, typed: str = "", pasted: str = "") -> None:
        # Body and Enter MUST arrive as separate reads at the inner pty; a
        # single-chunk burst makes the TUI treat the embedded \r as in-buffer
        # newline instead of an Enter keypress.
        body = _compose(typed, pasted).encode()
        self._send(body, b"\r")


_DTACH_ACTION_FLAGS = frozenset({"-a", "-A", "-n", "-c"})
_ABDUCO_ACTION_FLAGS = frozenset({"-a", "-A", "-c"})


def _arg_after(argv: list[str], flags: frozenset[str]) -> str | None:
    for i, arg in enumerate(argv):
        if arg in flags and i + 1 < len(argv):
            return argv[i + 1]
    return None


def _ps_lookup(pid: int) -> tuple[int, list[str]] | None:
    """Return (ppid, argv) for `pid` via ps, or None on lookup failure."""
    try:
        result = subprocess.run(
            ["ps", "-p", str(pid), "-o", "ppid=,args="],
            capture_output=True,
            text=True,
            check=True,
        )
    except subprocess.CalledProcessError:
        return None
    line = result.stdout.strip()
    if not line:
        return None
    ppid_str, _, args = line.partition(" ")
    try:
        ppid = int(ppid_str)
    except ValueError:
        return None
    return ppid, args.split()


def detect_mux_writer() -> MuxWriter:
    """Return a `MuxWriter` for the surrounding multiplexer.

    Fast path: `$TMUX_PANE` short-circuits to a TmuxWriter. Slow path: walk the
    parent process chain, matching `dtach` or `abduco` argv and extracting the
    socket/session name from their action flags. No match → NoMuxWriterError.
    """
    pane = os.environ.get("TMUX_PANE")
    if pane:
        return TmuxWriter(pane)

    pid = os.getppid()
    while pid > 1:
        looked_up = _ps_lookup(pid)
        if looked_up is None:
            break
        ppid, argv = looked_up
        if not argv:
            break
        cmd = os.path.basename(argv[0])
        if cmd == "dtach":
            socket = _arg_after(argv, _DTACH_ACTION_FLAGS)
            if socket:
                return DtachWriter(socket)
        elif cmd == "abduco":
            session = _arg_after(argv, _ABDUCO_ACTION_FLAGS)
            if session:
                return AbducoWriter(session)
        pid = ppid

    raise NoMuxWriterError("no supported multiplexer in process ancestry")
