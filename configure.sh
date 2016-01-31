#!/usr/bin/env bash
# determine os
if [[ "$OSTYPE" == darwin* ]]; then
  HAS_OSX=1
else
  HAS_LINUX=1
fi

# make sure all our submodules are initialized
#git submodule update --init --recursive

# ask for sudo command
sudo -v
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

dotfiles=( 
    # config files
    'all' 'home/.gitconfig'	 ''
    'deb' 'home/.i3'		 ''
    'osx' 'home/.tmux.conf'	 ''
    'all' 'home/.vim'    	 ''
    'all' 'home/.vim' 		 '.config/nvim'
    'all' 'home/.vim/init.vim'	 '.vimrc'
    'all' 'home/.zshenv'  	 ''
    'all' 'home/.zprezto' 	 ''
    'all' 'home/.zpreztorc' 	 ''
    'all' 'home/.zprofile' 	 '' 
    'all' 'home/.zshrc' 	 ''
)

for (( i=0; i < ${#dotfiles[@]}; (i+=3) )); do
    plat=${dotfiles[$i]}
    src=${dotfiles[$((i+1))]}
    tgt=${HOME}/${dotfiles[$((i+2))]}
        
    # check for platform-specific elements
    if [[ "$plat" != "all" ]]; then 
	    [[ -n "$HAS_OSX" && "$plat" != "osx" ]] && continue
        [[ -n "$HAS_DEB" && "$plat" != "deb" ]] && continue
    fi

    # make sure the parent dir exists
    if ! [[ -d "$(dirname ${tgt})" ]]; then
        mkdir -p $(dirname ${tgt})
    fi

    # link the file
    ln -svnf `pwd`/${src} ${tgt}
done    

