zplug "rupa/z", use:z.sh
zplug "zsh-users/zsh-completions"

# load syntax highlighting second-last
zplug "zsh-users/zsh-syntax-highlighting", defer:2

# load substring search last
function __zsh_history_substring_search_hook_load {
    for mode in emacs viins vicmd; do \
        bindkey -M $mode "^P" history-substring-search-up; \
        bindkey -M $mode "^N" history-substring-search-down; \
    done
}
zplug "zsh-users/zsh-history-substring-search", defer:3, \
    hook-load:'__zsh_history_substring_search_hook_load'
