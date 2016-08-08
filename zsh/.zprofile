# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# override the bash history file
HISTFILE=${XDG_DATA_DIR}/zsh/history

# temporary files
if [[ ! -d "$TMPDIR" ]]; then
  export TMPDIR="/tmp/$LOGNAME"
  mkdir -p -m 700 "$TMPDIR"
fi

TMPPREFIX="${TMPDIR%/}/zsh"
