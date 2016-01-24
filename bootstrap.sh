#!/bin/sh
tgt=${HOME}
src=$(pwd)

# determine platform
if [[ `uname` == 'Darwin' ]]; then
  # MacOS X
  has_osx='1'
else
  # Linux
  has_linux='1'
fi

# defaults 
ln -sv ${src}/git/gitconfig ${tgt}/.gitconfig

# setup vim/neovim
mkdir -p ${tgt}/.config/
ln -sv ${src}/nvim/ ${tgt}/.config/nvim
ln -sv ${src}/nvim/init.vim ${tgt}/.vimrc
ln -sv ${src}/nvim/ ${tgt}/.vim

# setup zsh and zprezto
ln -sv ${src}/zsh/zprezto ${tgt}/.zprezto 
for rcfile in $(find ${src}/zsh/zprezto/runcoms -name z\*); do
  file=$(basename ${rcfile})
  if [ -f "${src}/zsh/${file}" ]; then
    ln -sv "${src}/zsh/${file}" "${tgt}/.${file}"
  else 
    ln -sv "$rcfile" "${tgt}/.${file}"
  fi
done
