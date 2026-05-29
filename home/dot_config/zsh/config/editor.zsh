# emacs-like editing mode
bindkey -e

# 10ms for key sequences
KEYTIMEOUT=1

# Treat these characters as part of a word.
WORDCHARS='*?_-.[]~&;!#$%^(){}<>'

# Shift-Tab to reverse complete
bindkey -M emacs "^[[Z" reverse-menu-complete

# better incremental search (shamelessly stolen from prezto)
if (( $+widgets[history-incremental-pattern-search-backward] )); then
    bindkey -M emacs "^R" history-incremental-pattern-search-backward
    bindkey -M emacs "^S" history-incremental-pattern-search-forward
else
    bindkey -M emacs "^R" history-incremental-search-backward
    bindkey -M emacs "^S" history-incremental-search-forward
fi

# expands .... to ../.. in viins
function expand-dot-to-parent-directory-path {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+='/..'
  else
    LBUFFER+='.'
  fi
}
# define our function as a zle callback
zle -N expand-dot-to-parent-directory-path
# mask the '.' key with our callback
for mode in emacs viins; do
    bindkey -M $mode "." expand-dot-to-parent-directory-path
done
# do not expand .... to ../.. during incremental search.
bindkey -M isearch . self-insert 2> /dev/null
