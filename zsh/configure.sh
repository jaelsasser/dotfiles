#!/usr/bin/env bash

# make sure zsh looks for its zshrc in the right spot on launch
ZDOTINIT='export ZDOTDIR=${XDG_CONFIG_BASE:-"$HOME/.config/zsh"}'
if ! grep -q "$ZDOTINIT" /etc/zprofile 2>&1 > /dev/null; then
    echo $ZDOTINIT | sudo tee -a /etc/zprofile
fi

ZPLUG_HOME=${ZDOTDIR}/zplug.d
if ! [[ -d $ZPLUG_HOME ]]; then
    git clone https://github.com/zplug/zplug \
        --depth 1 $ZPLUG_HOME
fi
