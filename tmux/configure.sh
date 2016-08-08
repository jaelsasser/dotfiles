#!/usr/bin/env bash

TPM_PATH="$XDG_CONFIG_HOME/tmux/plugins"
if ! [[ -d $TPM_PATH ]]; then
    mkdir -p $TPM_PATH
    git clone https://github.com/tmux-plugins/tpm $TPM_PATH/tpm
fi

# have to symlink for now
if ! [[ -e "$HOME/.tmuxp" ]]; then
    ln -s $XDG_CONFIG_HOME/tmux/tmuxp $HOME/.tmuxp
fi
