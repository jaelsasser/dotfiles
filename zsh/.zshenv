#
# Defines environment variables in all zsh shells
#

export ZDOTDIR=${ZDOTDIR:-$HOME/.config}

# inherit from bashrc
local BASHRC="${XDG_CONFIG_HOME:-$HOME/.config}/bash/bashrc"
[[ -f "$BASHRC" ]] && emulate bash -c "source $BASHRC"

# use our own HISTFILE
HISTFILE=${XDG_DATA_DIR:-$HOME/.local}/zsh/history
