#!/usr/bin/env bash
set -e
if [[ "$#" -ge "1" ]]; then
    case $1 in
        -R|-D|-S)
            STOW_ACTION=$1
            ;;
        *)
            TARGETS="$TARGETS $1"
            ;;
    esac
    shift
fi

STOW_ACTION=${STOW_ACTION:-"-R"}
TARGETS=${TARGETS:-"bash git tmux vim zsh"}

for TARGET in $TARGETS; do
	if [[ -e $TARGET/link.sh ]]; then
        # allow dotfile bundles to override the linker
		$TARGET/link.sh
	else
		# default install: symlink into XDG_CONFIG_HOME
		DEST=${XDG_CONFIG_HOME:-$HOME/.config}/$TARGET
		[[ -d $DEST ]] || mkdir -p $DEST

		# stow our application
		stow $STOW_ACTION $TARGET -t $DEST \
            --no-folding --ignore="link.sh|configure.sh|README.md"

		# run a prepare script, if any, on stows/restows
        if [[ -e $TARGET/configure.sh ]] && ! [[ $STOW_ACTION = -D ]]; then
            $TARGET/configure.sh
        fi
	fi
done
