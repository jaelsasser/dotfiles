#!/usr/bin/env bash

VIMDOTDIR="${XDG_CONFIG_HOME:-"$HOME/.vim"}/vim"

PLUG_INIT="${VIMDOTDIR}/autoload/plug.vim"
if ! [[ -e $PLUG_INIT ]]; then
    curl -fLo $PLUG_INIT --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

    # install our plugins
    vim +PlugInstall +qall
fi

ln -svf $VIMDOTDIR "$(dirname $VIMDOTDIR)/nvim"
