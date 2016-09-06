# vi editing mode
bindkey -e

# 10ms for key sequences
KEYTIMEOUT=1

# Treat these characters as part of a word.
WORDCHARS='*?_-.[]~&;!#$%^(){}<>'

# Shift-Tab to reverse complete
bindkey -M emacs "^[[Z" reverse-menu-complete

# better incremental search (shamelessly stolen from prezto)
if (( $+widgets[history-incremental-pattern-search-backward] )); then
    bindkey -M emacs "?" history-incremental-pattern-search-backward
    bindkey -M emacs "/" history-incremental-pattern-search-forward
else
    bindkey -M emacs "?" history-incremental-search-backward
    bindkey -M emacs "/" history-incremental-search-forward
fi

# allow command line editing in vicmd (^X^V)
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -M vicmd 'v' edit-command-line

# expands .... to ../.. in viins
function expand-dot-to-parent-directory-path {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+='/..'
  else
    LBUFFER+='.'
  fi
}
# map to zle
zle -N expand-dot-to-parent-directory-path
bindkey -M viins "." expand-dot-to-parent-directory-path
# do not expand .... to ../.. during incremental search.
bindkey -M isearch . self-insert 2> /dev/null
