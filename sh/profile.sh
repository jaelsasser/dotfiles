#!/usr/bin/env sh

# include local files in the PATH
export PATH="$HOME/.local/bin:$PATH"
export MANPATH="$HOME/.local/share/man/:$MANPATH"

# ensure that XDG_CONFIG_HOME et al. are set
source ${XDG_CONFIG_HOME:-$HOME/.config}/sh/xdg.sh

# include homebrew in the PATH on macOS
if [[ "$OSTYPE" == *darwin* ]]; then
    export PATH="/usr/local/sbin:/usr/local/bin:$PATH"

    # try to use the gnu coreutils if installed on macOS
    export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
    export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"

    # NVIDIA CUDA binaries and tools
    export PATH=/Developer/NVIDIA/CUDA-8.0/bin${PATH:+:${PATH}}
    export DYLD_LIBRARY_PATH=/Developer/NVIDIA/CUDA-8.0/lib\ ${DYLD_LIBRARY_PATH:+:${DYLD_LIBRARY_PATH}}
fi 

# fallback LANG, LC_ALL
if [[ -z "$LANG" ]]; then
    export LANG=en_US.UTF-8
fi
if [[ -z "$LC_ALL" ]]; then
    export LC_ALL=en_US.UTF-8
fi

# bail out now if we're a boring shell
if ! [[ "$-" == *i* ]] || [[ "$TERM" == "dumb" ]]; then
    return
fi

# disk space is cheap; history is priceless
HISTSIZE=4096
SAVEHIST=4096

# never drop into nano
export PAGER='less'
export EDITOR='vi'
export VISUAL='vi'

#
# FIX GPG >= 2.1
#
export GPG_TTY=$(tty)

#
# BASIC ALIASES
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
