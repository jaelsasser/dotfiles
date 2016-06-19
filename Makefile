all: stow-all doctor

####################
# GNU Stow rules

STOW-ALL ?= \
	bin \
	git \
	neovim \
	tmux \
	vim \
	zsh

STOW-DEBIAN ?= \
	i3

STOW-MAC ?=

stow-%: MODULES=$(shell echo STOW-$* | tr 'a-z' 'A-Z')
stow-%:
	stow -R $($(MODULES))

####################
# general rules

doctor:
	git submodule update --init

vim-bootstrap:
	curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	vim -c "PlugInstall|qa!"

