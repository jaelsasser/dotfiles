#!/usr/bin/env bash

VIMDOTDIR="${XDG_CONFIG_HOME:-$HOME/.config}/vim"
VENV2="${XDG_DATA_HOME:-$HOME/.local/share}/vim/venv2"
VENV3="${XDG_DATA_HOME:-$HOME/.local/share}/vim/venv3"
PLUG_INIT="${VIMDOTDIR}/autoload/plug.vim"

# install and initialize junegunn/vim-plug
if ! [[ -e $PLUG_INIT ]]; then
    curl -fLo $PLUG_INIT --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    vim +PlugInstall +qall
fi

# let NeoVim work out of the same dir as vim
if ! [[ -e "$(dirname $VIMDOTDIR)"/nvim ]]; then
    ln -s ${VIMDOTDIR} "$(dirname $VIMDOTDIR)"/nvim
fi

# make sure vim has a cache dir
mkdir -p ${XDG_CACHE_HOME:-$HOME/.cache}/vim

# make sure vim has a data dir
mkdir -p ${XDG_DATA_HOME:-$HOME/.local/share}/vim

# make sure vim has a Python 3 virtualenv
if ! [[ -d ${VENV3} ]]; then
    python3 -m venv --system-site-packages --symlinks ${VENV3}
    source ${VENV3}/bin/activate
    pip install -U neovim jedi
fi

# make sure vim has a Python 2 (ugh) virtualenv
if ! [[ -d ${VENV2} ]]; then
    python2 -m virtualenv --system-site-packages --symlinks ${VENV2}
    source ${VENV2}/bin/activate
    pip install -U neovim jedi
fi
