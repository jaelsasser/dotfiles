# source extra local setup functions
[[ -s ~/.zshlocal ]] && source ~/.zshlocal

# must-have, always-on-hand aliases
alias ls='ls -GFh'
alias ll='ls -GFhl'

# sometimes I wonder why I switched to tmuxp
alias mux='tmuxp load'

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

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
    pushdignoredups \
    pushdminus \
    pushdsilent \
    pushdtohome \
    chasedots \
    extendedglob \
    nomatch \
    notify

# complex module loading
source ${HOME}/.zsh/alias.zsh
source ${HOME}/.zsh/completion.zsh
source ${HOME}/.zsh/color.zsh
source ${HOME}/.zsh/substring-search.zsh

# prompt theme for zshell
if autoload -U promptinit && promptinit; then
     export PURE_PROMPT_SYMBOL='»'
     prompt pure

     #prompt minimal

     #prompt elspure
     #trap 'prompt_elspure_setprompts' WINCH
fi

# portable color scheme
BASE16_SHELL="${HOME}/.dotfiles/misc/base16-eighties.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

# fix for nvim colors on GNU terminal
if [ -z "$HAS_SSH" ] && [ -z "$TMUX" ] && [ -n "$HAS_DEB" ]; then
    export TERM="xterm-256color"
fi

# fzf everywhere
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
_fzf_compgen_path() {
    ag -g "" "$1"
}

# iTerm2 shell integrations on macOS
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
