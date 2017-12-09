#!/usr/bin/env bash

if ! [[ -e "$HOME/.emacs.d" ]]; then
    ln -s "$XDG_CONFIG_HOME"/emacs "$HOME"/.emacs.d
fi

if ! [[ -e "$HOME"/.local/share/emacs/lisp ]]; then
    mkdir -p "$HOME"/.local/share/emacs/lisp
    touch "$HOME"/.local/share/emacs/lisp/pass.el.gpg
fi

if ! [[ -e "$HOME"/.local/share/emacs/venv ]]; then
    python3 -m venv "$HOME"/.local/share/emacs/venv
fi
