autoload colors && colors

# possible values: main brackets pattern curosr
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)
source ${HOME}/.zsh/color/zsh-syntax-highlighting.zsh

# grep colors 
export GREP_COLOR='37;45'           # BSD.
export GREP_COLORS="mt=$GREP_COLOR" # GNU.
alias grep="${aliases[grep]:-grep} --color=auto"

if [ -n "$HAS_OSX" ]; then
    # colors for BSD ls
    export LSCOLORS='exfxcxdxbxGxDxabagacad'
    # colors for completion
    export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=36;01:cd=33;01:su=31;40;07:sg=36;40;07:tw=32;40;07:ow=33;40;07:'

    alias ls="${aliases[ls]:-ls} -G"
else
    eval "$(dircolors --sh)"
    alias ls="${aliases[ls]:-ls} --color=auto --group-directories-first"
fi
