#!/usr/bin/env zsh

ZPLUG_HOME="${ZDOTDIR}/zplug.d"
source $ZPLUG_HOME/init.zsh

zplug "zsh-users/zsh-history-substring-search"

zplug "junegunn/fzf-bin", \
    from:gh-r, \
    as:command, \
    rename-to:fzf, \
    use:"*darwin*amd64*"

# pure theme requires zsh-async
zplug "mafredri/zsh-async"
zplug "sindresorhus/pure"

# alternative theme: purity, one-line pure
# zplug "therealklanni/purity"

# load syntax highlighting last
zplug "zsh-users/zsh-syntax-highlighting", nice:10

zplug load
