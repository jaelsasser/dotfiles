#!/usr/bin/env bash

ZDOTDIR=${XDG_CONFIG_BASE:-$HOME/.config}

# make sure zsh looks for its zshrc in the right spot on launch
ZDOTINIT='export ZDOTDIR=${XDG_CONFIG_BASE:-"$HOME/.config/"}zsh'
if ! grep -q "$ZDOTINIT" /etc/zshenv 2>&1 > /dev/null; then
    if ! echo $ZDOTINIT | sudo tee -a /etc/zshenv; then
        # fallback: ~/.zshenv
        ! [[ -d ~/.zshenv ]] && \
            echo "export $ZDOTINIT" >> ~/.zshenv
    fi
fi

ZPLUG_HOME=${XDG_DATA_BASE:-$HOME/.local/share}/zplug
if ! [[ -d $ZPLUG_HOME ]]; then
    git clone https://github.com/zplug/zplug \
        --depth 1 $ZPLUG_HOME
fi
