#!/usr/bin/env bash
cd "$(dirname "$0")" || exit 1
source sh/xdg.sh

while [[ "$#" -ge 1 ]]; do
    case $1 in
        -R|-D|-S)
            STOW_ACTION=$1
            ;;
        *)
            TARGETS="$TARGETS $1"
            ;;
    esac
    shift
done

STOW_ACTION=${STOW_ACTION:-"-R"}
TARGETS=${TARGETS:-"bash claude ghostty git sh tmux vim zsh"}

for TARGET in $TARGETS; do
    if [[ -e $TARGET/link.sh ]]; then
        # allow dotfile bundles to override the linker
        $TARGET/link.sh "$STOW_ACTION"
    else
        # default install: symlink into XDG_CONFIG_HOME
        DEST=${XDG_CONFIG_HOME:-$HOME/.config}/$TARGET
        [[ -d $DEST ]] || mkdir -p $DEST

        # stow our application
        stow -v $STOW_ACTION $TARGET -t $DEST \
             --ignore="configure.sh" --ignore="link.sh" --ignore "README.md"
    fi

    # run a prepare script, if any, on stows/restows
    if [[ -e $TARGET/configure.sh ]] && ! [[ $STOW_ACTION = -D ]]; then
        $TARGET/configure.sh
    fi
done
