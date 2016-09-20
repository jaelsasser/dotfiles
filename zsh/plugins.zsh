zplug "zsh-users/zsh-completions"

# load syntax highlighting second-last
zplug "zsh-users/zsh-syntax-highlighting", nice:10, \
    hook-load:'export ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)'

# load substring search last
zplug "zsh-users/zsh-history-substring-search", nice:15, \
    hook-load:'for mode in emacs viins vicmd; do' \
        'bindkey -M $mode "^P" history-substring-search-up;' \
        'bindkey -M $mode "^N" history-substring-search-down;' \
    'done'
