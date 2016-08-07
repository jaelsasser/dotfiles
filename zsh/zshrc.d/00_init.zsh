#!/usr/bin/env zsh

# emacs bindings for line editor
bindkey -e

# set zsh options
setopt \
    autopushd \
    pushdignoredups \
    pushdminus \
    pushdsilent \
    pushdtohome \
    chasedots \
    extendedglob \
    nomatch \
    notify

# fix for nvim colors on GNU terminal
if [ -z "$HAS_SSH" ] && [ -z "$TMUX" ] && [ -n "$HAS_DEB" ]; then
    export TERM="xterm-256color"
fi
