#
# Defines environment variables for interactive shells
#

# ensure that XDG_CONFIG_HOME and friends are set
source ${XDG_CONFIG_HOME:-$HOME/.config}/sh/xdg.sh

# source the common sh/bash/zsh profile.sh
source ${XDG_CONFIG_HOME:-$HOME/.config}/sh/profile.sh

# tuck the zsh history file away in XDG_DATA_HOME
HISTFILE=${XDG_DATA_HOME:-$HOME/.local/share}/zsh/history
if ! [[ -d "$(dirname HISTFILE)" ]]; then
    mkdir -p "$(dirname ${HISTFILE})"
fi

# machine-specific configuration
[[ -f "$ZDOTDIR"/local.zsh ]] && source "$ZDOTDIR"/local.zsh

# bail out now if we're a boring shell
if ! [[ "$-" == *i* ]] || [[ "$TERM" == "dumb" ]]; then
    return
fi

#
# PRE-PLUGIN SUBMODULES
#
source "$ZDOTDIR"/completion.zsh
source "$ZDOTDIR"/editor.zsh
source "$ZDOTDIR"/options.zsh

#
# PLUGIN OPTIONS
#
export _Z_DATA=${XDG_DATA_HOME:-"$HOME/.local/share"}/zsh/z
export ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)

#
# ZPLUG: https://github.com/zplug/zplug
#
export ZPLUG_CACHE_DIR=${XDG_DATA_CACHE:-$HOME/.cache}/zsh/zplug
export ZPLUG_HOME=${XDG_DATA_HOME:-$HOME/.local/share}/zsh/zplug
export ZPLUG_LOADFILE="$ZDOTDIR"/plugins.zsh
zstyle :zplug:tag depth 1

source "$ZPLUG_HOME"/init.zsh
source "$ZDOTDIR"/prompt.zsh

zplug check || zplug install
zplug load
