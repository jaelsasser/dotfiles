#!/bin/sh
# Create XDG cache/data dirs that tools expect to already exist.
# Ported from the old sh/configure.sh (less history dir).
set -eu

mkdir -p "${XDG_CACHE_HOME:-$HOME/.cache}/less"
