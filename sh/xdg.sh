#
# ENSURE XDG BASE DIRECTORY SPEC
#
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"

if ! [ -n "$XDG_RUNTIME_DIR" ]; then
    export XDG_RUNTIME_DIR="${TMPDIR:-/tmp/$USER}"
    ! [ -d "$XDG_RUNTIME_DIR" ] && mkdir -p -m 700 "$XDG_RUNTIME_DIR"
fi

#
# FORCE XDG BASE DIRECTORY COMPLIANCE WHERE POSSIBLE
#   see: https://wiki.archlinux.org/index.php/XDG_Base_Directory_support
#
alias gdb="gdb --nx --init-command='$XDG_CONFIG_HOME/gdb/init'"
alias irssi="irssi --home='$XDG_CONFIG_HOME'/irssi"
alias ptpython="ptpython --config-dir='$XDG_CONFIG_HOME'/ptpython"
alias tmux="tmux -f '$XDG_CONFIG_HOME'/tmux/tmux.conf"

export CARGO_HOME="$XDG_DATA_HOME"/cargo
export CCACHE_DIR="$XDG_CACHE_HOME"/ccache
export COMPOSER_CACHE_DIR="$XDG_CACHE_HOME"/composer
export COMPOSER_HOME="$XDG_CONFIG_HOME"/composer
export GOPATH="$XDG_DATA_HOME"/go
export PATH="$GOPATH/bin:$PATH" # expand PATH with go binaries
export PASSWORD_STORE_DIR="$XDG_CONFIG_HOME"/pass
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc
export HGRCPATH="$XDG_CONFIG_HOME"/hg/hgrc
export HTOPRC="$XDG_CONFIG_HOME"/htop/htoprc
export HTTPIE_CONFIG_DIR="$XDG_CONFIG_HOME"/httpie
export INPUTRC="$XDG_CONFIG_HOME"/readline/inputrc
export IPYTHONDIR="$XDG_CONFIG_HOME"/jupyter
export JUPYTER_CONFIG_DIR="$XDG_CONFIG_HOME"/jupyter
export LESSHISTFILE="$XDG_CACHE_HOME"/less/history
export LESSKEY="$XDG_CONFIG_HOME"/less/lesskey
export PYTHON_EGG_CACHE="$XDG_CACHE_HOME"/python-eggs
export RXVT_SOCKET="$XDG_RUNTIME_DIR"/urxvt/urxvt-`hostname`
export TERMINFO_DIRS="$XDG_DATA_HOME"/terminfo:/usr/share/terminfo  # ncurses
export TMUX_TMPDIR="$XDG_RUNTIME_DIR"/tmux && mkdir -m 700 -p $TMUX_TMPDIR
export VIMDOTDIR="$XDG_CONFIG_HOME"/vim  # still need to define VIMINIT
export VIMINIT="let \$MYVIMRC='$VIMDOTDIR/vimrc' | source \$MYVIMRC"
export WEECHAT_HOME="$XDG_CONFIG_HOME"/weechat
export XMONAD_CONFIG_HOME="$XDG_CONFIG_HOME"/xmonad
export XMONAD_DATA_HOME="$XDG_DATA_HOME"/xmonad
export XMONAD_CACHE_HOME="$XDG_CACHE_HOME"/xmonad
export XAUTHORITY="$XDG_CONFIG_HOME"/X11/xauthority
export XCOMPOSEFILE="$XDG_CONFIG_HOME"/X11/xcompose
export XINITRC="$XDG_CONFIG_HOME"/X11/xinitrc
