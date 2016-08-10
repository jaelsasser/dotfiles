#!/usr/bin/env bash

ENVFILE=${HOME}/.zshenv
for FILE in "/etc/zshenv /etc/zsh/zshenv"; do
    if [[ -f "$FILE" ]]; then
        ENVFILE=$FILE
        break
    fi
done

# make sure zsh looks for its zshrc in the right spot on launch
ZDOTINIT='export ZDOTDIR=${XDG_CONFIG_BASE:-$HOME/.config/}zsh'
if ! grep -q ${ZDOTINIT} ${ENVFILE} 2>&1 >/dev/null; then
    if ! echo $ZDOTINIT | sudo tee -a ${ENVFILE}; then
        echo "Failed to set custom ZDOTDIR"
    fi
fi

ZPLUG_HOME=${XDG_DATA_BASE:-$HOME/.local/share}/zplug
if ! [[ -d $ZPLUG_HOME ]]; then
    git clone https://github.com/zplug/zplug \
        --depth 1 $ZPLUG_HOME
fi
