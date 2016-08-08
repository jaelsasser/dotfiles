#
# Defines environment variables for interactive shells
#   Note: the XDG Base Directory Specification
#   enviornment vairables inherited from my bashrc,
#   which are sourced in zshenv
#

if [[ "$TERM" == 'dumb' ]] ; then
    return
fi

#
# GENERAL OPTIONS
#
setopt NOTIFY               # report background job status immediately

setopt CHASE_DOTS           # don't take shortcuts resolving "foo/.."
setopt EXTENDED_GLOB        # '#', '~', and '^' as in filename patterns
setopt NOMATCH              # print error on non-matching pattern

setopt AUTO_PUSHD           # cd pushes to the dir stack on every call
setopt PUSHD_IGNORE_DUPS    # keep the directory stack clean
setopt PUSHD_MINUS          # swaps "+" and "-" when selecting from stack
setopt PUSHD_SILENT         # don't print the dir stack after pushd/popd
setopt PUSHD_TO_HOME        # pushd with no args -> pushd $HOME

#
# ENVIRONMENT
#
local COLORS=${COLORS:-$ZDOTDIR/colors.zsh}
[[ -e "$COLORS" ]] && source $COLORS

# source all zshrc.d file in interactive terminals
for f in "$ZDOTDIR"/zshrc.d/*.zsh; do
    source $f
done

# zplug
export ZPLUG_HOME=${XDG_DATA_HOME:-$HOME/.local/share}/zplug
export ZPLUG_CACHE_FILE=${XDG_DAT_CACHE:-$HOME/.cache}/zplug/cache
export ZPLUG_LOADFILE="$ZDOTDIR"/plugins.zsh

source "$ZPLUG_HOME"/init.zsh && zplug load
