#!/usr/bin/env bash

VIMDOTDIR="${XDG_CONFIG_HOME:-$HOME/.config}/vim"
PLUG_INIT="${VIMDOTDIR}/autoload/plug.vim"

if ! [[ -e $PLUG_INIT ]]; then
    curl -fLo $PLUG_INIT --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

    # install our plugins
    vim +PlugInstall +qall
fi

# let NeoVim work out of the same dir as vim
if ! [[ -e "$(dirname $VIMDOTDIR)"/nvim ]]; then
    ln -s $VIMDOTDIR "$(dirname $VIMDOTDIR)/nvim"
fi

# make sure vim has a cache dir
mkdir -p ${XDG_CACHE_HOME:-$HOME/.cache}/vim
