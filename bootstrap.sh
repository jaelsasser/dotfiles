#!/bin/sh
tgt=${HOME}
src=$(pwd)
ln_flags=-svn

# determine platform
if [ "$(uname)" = 'Darwin' ]; then
  # MacOS X
  has_osx='1'
else
  # Linux
  has_linux='1'
  ln ${ln_flags} ${src}/i3 ${tgt}/.i3
fi

# link git config files 
ln ${ln_flags} ${src}/git/gitconfig ${tgt}/.gitconfig

# make sure all our submodules are initialized
git submodule update --init --recursive

# link vim and neovim config files 
mkdir -p ${tgt}/.config/
ln ${ln_flags} ${src}/nvim/ ${tgt}/.config/nvim
ln ${ln_flags} ${src}/nvim/init.vim ${tgt}/.vimrc
ln ${ln_flags} ${src}/nvim/ ${tgt}/.vim

# link zsh and zprezto
ln ${ln_flags} ${src}/zsh/zprezto ${tgt}/.zprezto 
for rcfile in $(find ${src}/zsh/zprezto/runcoms -name z\*); do
  file=$(basename ${rcfile})
  if [ -f "${src}/zsh/${file}" ]; then
    ln ${ln_flags} "${src}/zsh/${file}" "${tgt}/.${file}"
  else 
    ln ${ln_flags} "$rcfile" "${tgt}/.${file}"
  fi
done
