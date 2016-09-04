zplug "junegunn/fzf-bin", \
    from:gh-r, \
    as:command, \
    rename-to:fzf, \
    use:"*darwin*amd64*"

# macOS plugins
if [[ $OSTYPE == *darwin* ]]; then
    zplug "plugins/brew-cask", from:oh-my-zsh, ignore:oh-my-zsh.sh, \
        nice:10
    zplug "unixorn/tumult.plugin.zsh", nice:11
fi

# nice:10 is defined as the completion point in zplug
zplug "zsh-users/zsh-completions", nice:10

# load syntax highlighting second-last
zplug "zsh-users/zsh-syntax-highlighting", nice:11, \
    hook-load:\
    'export ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)'

# load substring search last
zplug "zsh-users/zsh-history-substring-search", nice:15, \
    hook-load:'for mode in emacs viins vicmd; do' \
        'bindkey -M $mode "^P" history-substring-search-up;' \
        'bindkey -M $mode "^N" history-substring-search-down;' \
    'done'
