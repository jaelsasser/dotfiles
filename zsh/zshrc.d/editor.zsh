# emacs bindings for line editor
bindkey -e

# allow command line editing in an external editor.
# autoload -Uz edit-command-line
# zle -N edit-command-line

# expands .... to ../..
function expand-dot-to-parent-directory-path {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+='/..'
  else
    LBUFFER+='.'
  fi
}

zle -N expand-dot-to-parent-directory-path
bindkey -M emacs "." expand-dot-to-parent-directory-path

# do not expand .... to ../.. during incremental search.
bindkey -M isearch . self-insert 2> /dev/null
