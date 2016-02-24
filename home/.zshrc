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
setopt \
    autopushd \
    chasedots \
    pushdignoredups \
    extendedglob \
    nomatch \
    notify

# extra zsh functions
fpath=( "$HOME/.zfunctions" $fpath )
autoload zmv

# complex module loading
source ${HOME}/.zsh/alias.zsh
source ${HOME}/.zsh/completion.zsh
source ${HOME}/.zsh/color.zsh
source ${HOME}/.zsh/substring-search.zsh

# prompt theme for zshell
if autoload -U promptinit && promptinit; then
     prompt pure
     #prompt minimal
fi

# portable color scheme
BASE16_SHELL="${HOME}/.dotfiles/res/scripts/base16-eighties.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

# fix for nvim colors on GNU terminal
if [ -z "$HAS_SSH" ] && [ -z "$TMUX" ] && [ -n "$HAS_DEB" ]; then
    export TERM="xterm-256color"
fi

# tmuxify
if [[ -z "$TMUX" && && -z "$SSH" -z "$EMACS" && -z "$VIM" ]]; then
    source ${HOME}/.zsh/tmux.zsh
fi

