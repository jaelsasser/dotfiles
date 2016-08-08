#!/usr/bin/env bash

TPM_PATH="$XDG_DATA_HOME/tmux/plugins"
if ! [[ -d $TPM_PATH ]]; then
    mkdir -p $TPM_PATH
    git clone https://github.com/tmux-plugins/tpm $TPM_PATH/tpm
fi

# have to symlink this shit for now:
if ! [[ -e "$HOME"/.tmux.conf ]]; then
    ln -s $XDG_CONFIG_HOME/tmux/tmux.conf $HOME/.tmux.conf
fi

if ! [[ -e "$HOME/.tmuxp" ]]; then
    ln -s $XDG_CONFIG_HOME/tmux/tmuxp $HOME/.tmuxp
fi
