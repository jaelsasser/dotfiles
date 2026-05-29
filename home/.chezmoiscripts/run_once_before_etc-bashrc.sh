#!/bin/sh
# Source our XDG bashrc from the system-wide bash init file. Bash has no
# XDG-native config path, so the global rc (/etc/bashrc on macOS/RH,
# /etc/bash.bashrc on Debian) is where we hook in.
#
# Ported from bash/configure.sh, preferring /etc/bashrc, then /etc/bash.bashrc,
# then a ~/.bashrc fallback when neither exists or the sudo write is refused.
set -eu

snippet='source ${XDG_CONFIG_HOME:-$HOME/.config}/bash/bashrc'

if [ -f /etc/bashrc ]; then bashrc=/etc/bashrc
elif [ -f /etc/bash.bashrc ]; then bashrc=/etc/bash.bashrc
else bashrc=''; fi

if [ -n "$bashrc" ]; then
    grep -qF "$snippet" "$bashrc" 2>/dev/null && exit 0
    printf '%s\n' "$snippet" | sudo tee -a "$bashrc" >/dev/null && exit 0
fi

# No system rc, or the sudo write was refused: fall back to ~/.bashrc.
grep -qF "$snippet" "$HOME/.bashrc" 2>/dev/null && exit 0
printf '%s\n' "$snippet" >> "$HOME/.bashrc"
