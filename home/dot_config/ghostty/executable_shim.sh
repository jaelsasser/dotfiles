#!/bin/sh
PATH="/opt/homebrew/bin:$PATH"
if command -v dtach >/dev/null 2>&1; then
	exec dtach -A "${TMPDIR:-/tmp}/ghostty.$$" -z "$SHELL" -l
elif command -v abduco >/dev/null 2>&1; then
	exec abduco -c "ghostty.$$" "$SHELL" -l
else
	exec "$SHELL" -l
fi
