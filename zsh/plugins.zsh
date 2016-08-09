zplug "junegunn/fzf-bin", \
    from:gh-r, \
    as:command, \
    rename-to:fzf, \
    use:"*darwin*amd64*"

zplug "mafredri/zsh-async"
zplug "sindresorhus/pure"

# zplug "lib/git", from:oh-my-zsh, ignore:oh-my-zsh.sh, \
#     on:"therealklanni/purity", nice:10
# zplug "lib/theme-and-appearance", from:oh-my-zsh, ignore:oh-my-zsh.sh, \
#     on:"therealklanni/purity"
# zplug "therealklanni/purity"

if [[ $OSTYPE == *darwin* ]]; then
    zplug "plugins/brew-cask", from:oh-my-zsh, ignore:oh-my-zsh.sh, \
        nice:10
fi

zplug "zsh-users/zsh-completions", nice:10

# load syntax highlighting second-last
zplug "zsh-users/zsh-syntax-highlighting", nice:11

# load substring search last
zplug "zsh-users/zsh-history-substring-search", nice:15, \
	hook-load:\
    'for mode in emacs viins vicmd; do' \
        'bindkey -M $mode "^P" history-substring-search-up;' \
        'bindkey -M $mode "^N" history-substring-search-down;' \
    'done'
