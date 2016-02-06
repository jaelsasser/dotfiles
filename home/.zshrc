# source extra local setup functions 
[[ -s ~/.zshlocal ]]   && source ~/.zshlocal

# aliases
alias vi='nvim'
alias ls='ls -GFh'
alias ll='ls -GFhl'

################################
if [[ "$TERM" == 'dumb' ]]; then
    return
fi
################################

# emacs bindings for line editor
bindkey -e


# set zsh options
setopt autopushd \
    chasedots \
    pushdignoredups \
    extendedglob \
    nomatch \
    appendhistory \
    sharehistory \
    histignorealldups \
    sharehistory \
    notify

# extra zsh functions
fpath=( "$HOME/.zfunctions" $fpath )

# complex module loading
source ${HOME}/.zsh/alias.zsh
source ${HOME}/.zsh/color.zsh
source ${HOME}/.zsh/completion.zsh
source ${HOME}/.zsh/substring-search.zsh

# prompt theme for zshell
if autoload -U promptinit && promptinit; then
     prompt pure
fi

# portable color scheme
BASE16_SHELL="$HOME/.dotfiles/resources/base16-eighties.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

# fix for nvim colors on GNU terminal
if [ -z "$HAS_SSH" ] && [ -n "$HAS_DEB" ]; then
    #export NVIM_TUI_ENABLE_TRUE_COLOR=1
    export TERM="xterm-256color"
fi
