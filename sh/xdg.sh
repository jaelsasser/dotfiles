#
# ENSURE XDG BASE DIRECTORY SPEC
#

# fallback to defaults
[[ -n "$XDG_CONFIG_HOME" ]] || \
    export XDG_CONFIG_HOME=${HOME}/.config
[[ -n "$XDG_DATA_HOME" ]] || \
    export XDG_DATA_HOME=${HOME}/.local/share
[[ -n "$XDG_CACHE_HOME" ]] || \
    export XDG_CACHE_HOME=${HOME}/.cache
[[ -n "$XDG_RUNTIME_DIR" ]] || \
    export XDG_RUNTIME_DIR=${TMPDIR:-"/tmp"}

# verify the runtime dir
if ! [[ -d "$XDG_RUNTIME_DIR" ]]; then
    mkdir -p -m 700 "$XDG_RUNTIME_DIR"
fi

#
# FORCE XDG BASE DIRECTORY COMPLIANCE WHERE POSSIBLE
#   see: https://wiki.archlinux.org/index.php/XDG_Base_Directory_support
#

## composer
export COMPOSER_HOME="$XDG_CONFIG_HOME"/composer
export COMPOSER_CACHE_DIR="$XDG_CACHE_HOME"/composer

## hg
export HGRCPATH=${XDG_CONFIG_HOME}/hg/hgrc

## gdb
alias gdb='gdb -nh -x "$XDG_CONFIG_HOME"/gdb/init'

## gpg
export GNUPGHOME="$XDG_CONFIG_HOME"/gnupg
alias gpg='gpg2 --homedir "$XDG_CONFIG_HOME"/gnupg'

## less
export LESSKEY="$XDG_CONFIG_HOME"/less/lesskey
export LESSHISTFILE="$XDG_CACHE_HOME"/less/history
mkdir -p "$XDG_CACHE_HOME"/less

## ncurses
export TERMINFO_DIRS="$XDG_DATA_HOME"/terminfo:/usr/share/terminfo

## irssi
alias irssi='irssi --home="$XDG_CONFIG_HOME"/irssi'

## readline
export INPUTRC="$XDG_CONFIG_HOME"/readline/inputrc

## tmux -- I've mostly given up on this one
# alias tmux='tmux -f "$XDG_CONFIG_HOME"/tmux/tmux.conf'
export TMUX_TMPDIR="$XDG_RUNTIME_DIR"/tmux && mkdir -m 700 -p $TMUX_TMPDIR

## urxvtd
export RXVT_SOCKET="$XDG_RUNTIME_DIR"/urxvt/urxvt-`hostname`

## various python
alias ptpython='ptpython --config-dir="$XDG_CONFIG_HOME"/ptpython'
export HTTPIE_CONFIG_DIR="$XDG_CONFIG_HOME"/httpie
export IPYTHONDIR="$XDG_CONFIG_HOME"/jupyter
export PYTHON_EGG_CACHE="$XDG_CACHE_HOME"/python-eggs

## vim
VIMDOTDIR="${XDG_CONFIG_HOME:-"$HOME/.config"}/vim"
export VIMINIT="let \$MYVIMRC='$VIMDOTDIR/vimrc' | source \$MYVIMRC"

## X11
export XAUTHORITY="$XDG_RUNTIME_DIR"/X11/xauthority
export XCOMPOSEFILE="$XDG_CONFIG_HOME"/X11/xcompose
export XINITRC="$XDG_CONFIG_HOME"/X11/xinitrc
