# platform specific
if [[ "$OSTYPE" == darwin* ]]; then
  export HAS_OSX=1
  export path=( /usr/local/bin $path )
else
  export HAS_DEB=1
fi

# set editors
export EDITOR='vi'
export VISUAL='vi'
export PAGER='less'

# set language
[[ -z "$LANG" ]] && export LANG-'en_US.UTF-8' 

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
HISTFILE=~/.zsh/.history
