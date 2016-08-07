#!/usr/bin/env zsh
#
# df / du
alias df='df -kh'
alias du='du -kh'

# ls
alias l='ls -1A'
alias ls='ls -GFh'
alias ll='ls -aGFhl'

# prefer neovim over vim
if which nvim 2>&1 > /dev/null; then
    alias vim=nvim
    alias vi=nvim
fi

# grep
alias grep="${aliases[grep]:-grep} --color=auto"

# point tmux at XDG_CONFIG_HOME
alias tmux='tmux -f ${XDG_CONFIG_HOME}/tmux/tmux.conf'

# Expands .... to ../..
function expand-dot-to-parent-directory-path {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+='/..'
  else
    LBUFFER+='.'
  fi
}
zle -N expand-dot-to-parent-directory-path

# Do not expand .... to ../.. during incremental search.
bindkey -M isearch . self-insert 2> /dev/null
