#!/usr/bin/env sh
# sideload.sh — promote this dev checkout's working branch into the chezmoi
# source clone WITHOUT a GitHub round-trip, then preview the delta. The clone's
# `main` is the live state chezmoi applies; this rebases it onto the dev repo's
# branch (default: whatever's checked out here) via the `local` remote, so
# committed-but-unpushed work can be tested before it hits origin. Apply is left
# to you — side-loading stages; `chezmoi apply` deploys.
#
# Usage: dist/sideload.sh [branch]   (default branch: this checkout's current)
set -eu

repo="$(cd "$(dirname "$0")/.." && pwd)"
src="$(chezmoi execute-template '{{ .chezmoi.workingTree }}')"
branch="${1:-$(git -C "$repo" branch --show-current)}"

git -C "$src" remote get-url local >/dev/null 2>&1 || git -C "$src" remote add local "$repo"
git -C "$src" fetch local "$branch"
git -C "$src" rebase "local/$branch"

printf '\nStaged %s@%s into %s. Preview the delta:\n\n' "$repo" "$branch" "$src"
chezmoi diff
printf '\nGo live with: chezmoi apply   (publish with: git -C %s push origin main)\n' "$src"
