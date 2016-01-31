source ${HOME}/.zsh/history/zsh-history-substring-search.zsh

# keybinds for substring search
if zmodload zsh/terminfo; then
    ## bind UP and DOWN arrow keys
    bindkey "$terminfo[kcuu1]" history-substring-search-up
    bindkey "$terminfo[kcud1]" history-substring-search-down
else
    # bind UP and DOWN arrow keys (compatibility fallback
    ## for Ubuntu 12.04, Fedora 21, and MacOSX 10.9 users)
    bindkey '^[[A' history-substring-search-up
    bindkey '^[[B' history-substring-search-down
fi

## bind P and N for EMACS mode
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

## bind k and j for VI mode
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down
