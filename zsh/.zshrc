#
# Defines environment variables for interactive shells
#   Note: the XDG Base Directory Specification
#   enviornment vairables inherited from my bashrc,
#   which are sourced in zshenv
#

# ensure that XDG_CONFIG_HOME et al. are set
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
# ZPLUG: https://github.com/zplug/zplug
#
export ZPLUG_CACHE_FILE=${XDG_DATA_CACHE:-$HOME/.cache}/zsh/zplug
export ZPLUG_CLONE_DEPTH=1
export ZPLUG_HOME=${XDG_DATA_HOME:-$HOME/.local/share}/zsh/zplug
export ZPLUG_LOADFILE="$ZDOTDIR"/plugins.zsh

source "$ZPLUG_HOME"/init.zsh
source "$ZDOTDIR"/prompt.zsh

zplug check || zplug install
zplug load
