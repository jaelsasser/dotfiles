#!/usr/bin/env bash
if ! [[ -e "$HOME"/.local/share/emacs/lisp ]]; then
    mkdir -p "$HOME"/.local/share/emacs/lisp
    touch "$HOME"/.local/share/emacs/lisp/pass.el.gpg
fi

if ! [[ -e "$HOME"/.local/share/emacs/venv ]]; then
    python3 -m venv "$HOME"/.local/share/emacs/venv
fi
