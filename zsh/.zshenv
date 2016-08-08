#
# Defines environment variables in all zsh shells
#

# inherit from bashrc
local BASHRC="${XDG_CONFIG_HOME:-$HOME/.config}/bash/bashrc"
[[ -f "$BASHRC" ]] && emulate bash -c "source $BASHRC"

export ZDOTDIR=${ZDOTDIR:-$XDG_CONFIG_HOME/zsh}

export HISTFILE="${XDG_DATA_HOME:-$HOME/.local}/zsh/history"
[[ -f $HISTFILE ]] || mkdir -p "$(dirname ${HISTFILE})"

if [[ ! -d "$TMPDIR" ]]; then
  export TMPDIR="/tmp/$LOGNAME"
  mkdir -p -m 700 "$TMPDIR"
fi
TMPPREFIX="${TMPDIR%/}/zsh"

# ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path
