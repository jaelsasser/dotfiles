#!/usr/bin/env sh

# include local files in the PATH
export LD_LIBRARY_PATH="$HOME/.local/lib/:$LD_LIBRARY_PATH"
export PATH="$HOME/.local/bin:$PATH"
export MANPATH="$HOME/.local/share/man/:$MANPATH"

# ensure that XDG_CONFIG_HOME et al. are set
source ${XDG_CONFIG_HOME:-$HOME/.config}/sh/xdg.sh

# fallback LANG, LC_ALL
if [[ -z "$LANG" ]]; then
    export LANG=en_US.UTF-8
fi
if [[ -z "$LC_ALL" ]]; then
    export LC_ALL=en_US.UTF-8
fi

# bail out now if we're a boring shell
if ! [[ "$-" == *i* ]] || [ "$TERM" = "dumb" ]; then
    return
fi

if [ -e "$HOME"/.profile.local ]; then
    source "$HOME"/.profile.local
fi

# disk space is cheap; history is priceless
HISTSIZE=8192
SAVEHIST=8192

# never drop into nano
export PAGER='less'
export EDITOR='vi'
export VISUAL='vi'

#
# BASIC ALIASES
#
alias df='df -kh'
alias du='du -kh'
alias l='ls -1A'
alias ll='ls -aFhl'
alias ls='ls -Fh'

#
# LS_COLORS
#
if ! [[ "$OSTYPE" == *darwin* ]]; then # ls --version 2>/dev/null | grep -q 'coreutils'; then
    alias ls="${aliases[ls]:-"ls"} --color=auto --group-directories-first"
    LS_COLORS_SPEC=${XDG_CONFIG_HOME:-"$HOME/.config"}/sh/dircolors.solarized
    eval `dircolors "$LS_COLORS_SPEC"`

    alias ls="${aliases[ls]:-"ls"} --color=auto --group-directories-first"
else
    # BSD fallback
    alias ls="${aliases[ls]:-"ls"} -G"
    export LSCOLORS='gxfxbEaEBxxEhEhBaDaCaD'
    # for zsh autocomplete
    export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=36;"\
                "01:cd=33;01:su=31;40;07:sg=36;40;07:tw=32;40;07:ow=33;40;07:'
fi
