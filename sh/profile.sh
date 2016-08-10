# include homebrew in the PATH on macOS
if [[ "$OSTYPE" == darwin* ]]; then
    export PATH="/usr/local/sbin:/usr/local/bin:$PATH"

    # try to use the gnu coreutils if installed on macOS
    export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
    export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
fi

# fallback LANG
if [[ -z "$LANG" ]]; then
    export LANG=en_US.UTF-8
fi

# fallback LC_ALL
if [[ -z "$LC_ALL" ]]; then
    export LC_ALL=en_US.UTF-8
fi

# disk space is cheap; history is priceless
HISTSIZE=4096
SAVEHIST=4096

# bail out now if we're a boring shell
if ! [[ "$-" == *i* ]] || [[ "$TERM" == "dumb" ]]; then
    return
fi

# nvim > vim > vi, but always fall back to something vi-like
export PAGER='less'
if which nvim 2>&1 >/dev/null; then
    export EDITOR='nvim'
    export VISUAL='nvim'
elif which vim 2>&1 >/dev/null; then
    export EDITOR='vim'
    export VISUAL='vim'
else
    export EDITOR='vi'
    export VISUAL='vi'
fi

#
# ALIASES
#
alias df='df -kh'
alias du='du -kh'
alias l='ls -1A'
alias ll='ls -aGFhl'
alias ls='ls -GFh'

#
# LS_COLORS
#

if which dircolors 2>&1 >/dev/null; then
    LS_COLORS_SPEC=${XDG_CONFIG_HOME:-$HOME/.config}/sh/dircolors.solarized
    eval $(dircolors "$LS_COLORS_SPEC")

    alias ls="${aliases[ls]:-ls} --color=auto --group-directories-first"
else
    # BSD fallback
    alias ls="${aliases[ls]:-ls} -G"
    export LSCOLORS='gxfxbEaEBxxEhEhBaDaCaD'
    # for zsh autocomplete
    export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=36;"\
                "01:cd=33;01:su=31;40;07:sg=36;40;07:tw=32;40;07:ow=33;40;07:'
fi
