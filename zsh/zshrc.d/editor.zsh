# vi editing mode
bindkey -v

# Treat these characters as part of a word.
WORDCHARS='*?_-.[]~&;!#$%^(){}<>'

# allow command line editing in an external editor.
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -M vicmd 'v' edit-command-line

# sets some emacs-like movement commands in viins and vicmd
for mode in viins vicmd; do
    bindkey -M $mode "\C-l" clear-screen

    bindkey -M $mode "\eb" backward-word
    bindkey -M $mode "\ef" forward-word

    bindkey -M $mode "\C-b" backward-char
    bindkey -M $mode "\C-f" forward-char

    bindkey -M $mode "\C-a" beginning-of-line
    bindkey -M $mode "\C-e" end-of-line
done

# sets some vi-like kill commands in viins
bindkey -M viins "\C-d" delete-char
bindkey -M viins "\e-d" kill-word
bindkey -M viins "\C-k" kill-line
bindkey -M viins "\C-u" kill-whole-line
bindkey -M viins "\C-w" backward-kill-word

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
