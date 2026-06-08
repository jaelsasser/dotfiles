#!/usr/bin/env bash
# run-tests.sh — run bats + pytest from the repo root.
# Usage: ./run-tests.sh [bats-options] [file|dir ...]
#
# bats defaults to chezmoi.bats; emacs/emacs.bats is deliberately excluded (slow,
# network: clones ~50 packages) — run it via `task test:emacs`. pytest covers
# claude/tests/unit/*.py under uv with the yield watcher's runtime deps spun
# up in an ephemeral env (no global pip install).
set -eu
REPO="$(cd "$(dirname "$0")" && pwd)"
cd "$REPO"

bats_status=0
bats -r "${@:-chezmoi.bats}" || bats_status=$?

pytest_status=0
if command -v uv >/dev/null 2>&1 && [ -d "claude/tests/unit" ]; then
    uv run --quiet --with pytest --with mcp --with watchfiles --with anyio \
        --python 3.11 \
        python -m pytest claude/tests/unit -q || pytest_status=$?
fi

exit $((bats_status != 0 ? bats_status : pytest_status))
