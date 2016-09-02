#!/usr/bin/env bash

VIMDOTDIR="${XDG_CONFIG_HOME:-$HOME/.config}/vim"
VENV="${XDG_DATA_HOME:-$HOME/.local/share}/vim/venv"
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

# make sure vim has a Python virtualenv (try and use python3)
python3 -m venv ${VENV} || python -m venv ${VENV}
source ${VENV}/bin/activate
if ! pip install neovim; then
    echo "Failed to initialize a Vim virtualenv"
    return 1
fi
