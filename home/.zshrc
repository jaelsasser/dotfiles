# prezto setup 
#if ! [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
#  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
#fi

# source local setup functions 
[[ -s ~/.zshlocal ]]   && source ~/.zshlocal
[[ -s ~/.zshprivate ]] && source ~/.zshprivate

# general opts
bindkey -e
setopt autocd correct extendedglob histignorealldups nomatch notify sharehistory

# aliases
alias vi='nvim'
alias ls='ls -GFh'
alias ll='ls -GFhl'

################################
if [[ "$TERM" == 'dumb' ]]; then
    return
fi
################################

fpath=( "$HOME/.zfunctions" $fpath )

# complex module loading
source ${HOME}/.zsh/completion.zsh
source ${HOME}/.zsh/color.zsh
source ${HOME}/.zsh/history.zsh

# prompt theme for zshell
if autoload -U promptinit && promptinit; then
     prompt pure
fi

# portable color scheme
BASE16_SHELL="$HOME/.dotfiles/resources/base16-eighties.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

# fix for nvim colors on GNU terminal
#if [ -z "$HAS_SSH" ] && [ -n "$HAS_DEB" ]; then
#    export NVIM_TUI_ENABLE_TRUE_COLOR=1
#fi
