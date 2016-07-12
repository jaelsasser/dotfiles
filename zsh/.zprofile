# platform specific
if [[ "$OSTYPE" == darwin* ]]; then
  export HAS_OSX=1
  export PATH="/usr/local/bin:$PATH"
else
  export HAS_DEB=1
fi

# extra stuff
export PATH="$HOME/bin:$PATH"
FPATH="$HOME/.zsh/functions:$FPATH"

# set editors
if which nvim 2>&1 >/dev/null; then
    export EDITOR='nvim'
    export VISUAL='nvim'
else
    export EDITOR='vi'
    export VISUAL='vi'
fi
export PAGER='less'

# set language
[[ -z "$LC_ALL" ]] && export LC_ALL=en_US.UTF-8
[[ -z "$LANG" ]] && export LANG=en_US.UTF-8

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# temporary files
if [[ ! -d "$TMPDIR" ]]; then
  export TMPDIR="/tmp/$LOGNAME"
  mkdir -p -m 700 "$TMPDIR"
fi
TMPPREFIX="${TMPDIR%/}/zsh"

HISTSIZE=100000
SAVEHIST=100000
HISTFILE=~/.zsh/history
