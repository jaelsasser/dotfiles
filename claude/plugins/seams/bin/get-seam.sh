#!/usr/bin/env bash
# get-seam.sh — emit active seam path (or content) from session cursor.
# Reads CLAUDE_SESSION_ID from env. Exit 1 on any missing link.
#
# Usage
#   get-seam.sh [--read] [--seam N]
#
# Stdout: absolute seam file path on success; "no upcoming seam" otherwise.
# With --read: file content instead of path.
# With --seam N: resolve seam-N.md regardless of cursor file.
# Exit: 0 on success, 1 on missing sid/cursor/file.
set -eu

read_content=0
seam_override=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --read)       read_content=1; shift ;;
    --seam)       seam_override="${2:-}"; shift 2 ;;
    *)            break ;;
  esac
done

sid="${CLAUDE_SESSION_ID:-${1:-}}"
[[ -n "$sid" ]] || exit 1
CACHE_ROOT="${CLAUDE_SEAMS_DIR:-$HOME/.claude/cache/seams}"
dir="${CACHE_ROOT}/${sid}"

if [[ -n "$seam_override" ]]; then
  cursor="$seam_override"
else
  cursor=$(cat "$dir/cursor" 2>/dev/null) || { echo "no upcoming seam"; exit 1; }
fi

handover="$dir/seam-${cursor}.md"
if [[ ! -r "$handover" ]]; then
  echo "no upcoming seam"
  exit 1
fi

if (( read_content )); then
  cat "$handover"
else
  echo "$handover"
fi
