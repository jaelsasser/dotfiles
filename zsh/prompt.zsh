#
# SET ZSH PROMPT
#   Run before zplug load
#

zplug "mafredri/zsh-async", defer:0
zplug "sindresorhus/pure", defer:1

PROMPT='%(?.%F{magenta}$.%F{red}$)%f '
