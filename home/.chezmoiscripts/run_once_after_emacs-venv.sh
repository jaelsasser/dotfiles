#!/bin/sh
# Emacs expects two machine-local paths to exist before it starts: a lisp dir
# holding a GPG-encrypted secrets stub, and a Python venv for lsp/formatters.
# Ported from emacs/configure.sh; guarded so re-apply is a no-op once present.
set -eu

lisp="${XDG_DATA_HOME:-$HOME/.local/share}/emacs/lisp"
venv="${XDG_DATA_HOME:-$HOME/.local/share}/emacs/venv"

if [ ! -e "$lisp" ]; then
    mkdir -p "$lisp"
    touch "$lisp/pass.el.gpg"
fi

[ -e "$venv" ] || python3 -m venv "$venv"
