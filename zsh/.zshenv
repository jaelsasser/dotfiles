#
# Defines environment variables in all zsh shells
#

# source the common sh/bash/zsh profile.sh
source ${XDG_CONFIG_HOME:-$HOME/.config}/sh/profile.sh

if [[ -d "$XDG_CONFIG_HOME"/zsh ]]; then
    export ZDOTDIR=${ZDOTDIR:-$XDG_CONFIG_HOME/zsh}
fi


if [[ ! -d "$TMPDIR" ]]; then
  export TMPDIR="/tmp/$LOGNAME"
  mkdir -p -m 700 "$TMPDIR"
fi
TMPPREFIX="${TMPDIR%/}/zsh"

# ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath manpath path
