# autload the promptinit module, even if we won't use it
autoload promptinit && promptinit

# Enable zsh's built-in highlighters
# possible values: main brackets pattern curosr
autoload colors && colors
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)

if [[ $OSTYPE == *darwin* ]]; then
    # colors for BSD ls
    export GREP_COLOR='37;45'
    # colors for BSD ls
    export LSCOLORS='exfxcxdxbxGxDxabagacad'
    # colors for completion
    export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=36;01:cd=33;01:su=31;40;07:sg=36;40;07:tw=32;40;07:ow=33;40;07:'

    alias ls="${aliases[ls]:-ls} -G"
else
    export GREP_COLORS="mt=$GREP_COLOR" # GNU.

    eval "$(dircolors --sh)"

    alias ls="${aliases[ls]:-ls} --color=auto --group-directories-first"
fi
