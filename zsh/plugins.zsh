zplug "rupa/z", use:z.sh
zplug "zsh-users/zsh-completions"

# load syntax highlighting second-last
zplug "zsh-users/zsh-syntax-highlighting", defer:2
export ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)
function __zsh_history_substring_search_hook_load {
    for mode in emacs viins vicmd; do \
        bindkey -M $mode "^P" history-substring-search-up; \
        bindkey -M $mode "^N" history-substring-search-down; \
    done
}
# load substring search last
zplug "zsh-users/zsh-history-substring-search", defer:3, \
    hook-load:'__zsh_history_substring_search_hook_load'
