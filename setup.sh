#!/usr/bin/env bash
set -e

if [[ -n $@ ]]; then
    TARGETS=$@
else
    TARGETS="git tmux vim zsh"
fi

[[ -z $XDG_CONFIG_HOME ]] && XDG_CONFIG_HOME=$HOME/.config

for TARGET in $TARGETS; do
	if [[ -e $TARGET/link.sh ]]; then
		$TARGET/link.sh
	else
		# default install: symlink into XDG_CONFIG_HOME
		DEST=$XDG_CONFIG_HOME/$TARGET
		[[ -d $DEST ]] || mkdir -p $DEST

		# stow our application
		stow -R $TARGET -t $DEST \
            --ignore="link.sh|configure.sh|README.md"

		# run a prepare script, if any
		[[ -e $TARGET/configure.sh ]] && $TARGET/configure.sh
	fi

done
