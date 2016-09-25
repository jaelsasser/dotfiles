#!/usr/bin/env bash

if ! [[ -e "$HOME/.emacs.d" ]]; then
    ln -s "$XDG_CONFIG_HOME"/emacs $HOME/.emacs.d
fi
