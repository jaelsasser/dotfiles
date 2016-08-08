zplug "junegunn/fzf-bin", \
    from:gh-r, \
    as:command, \
    rename-to:fzf, \
    use:"*darwin*amd64*"

zplug "mafredri/zsh-async", \
    on:"sindresorhus/pure"
zplug "sindresorhus/pure"

# zplug "lib/git", from:oh-my-zsh, ignore:oh-my-zsh.sh, \
#     on:"therealklanni/purity", nice:10
# zplug "lib/theme-and-appearance", from:oh-my-zsh, ignore:oh-my-zsh.sh, \
#     on:"therealklanni/purity"
# zplug "therealklanni/purity"

zplug "zsh-users/zsh-completions", nice:10

# load syntax highlighting second-last
zplug "zsh-users/zsh-syntax-highlighting", nice:13

# load substring search last
zplug "zsh-users/zsh-history-substring-search", nice:15, \
	hook-load:"bindkey -M emacs '^P' history-substring-search-up", \
	hook-load:"bindkey -M emacs '^N' history-substring-search-down"
