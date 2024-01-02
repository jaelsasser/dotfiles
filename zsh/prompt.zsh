#
# SET ZSH PROMPT
#   Run before zplug load
#
zplug "mafredri/zsh-async", from:github, defer:0
zplug "sindresorhus/pure", from:github, use:pure.zsh as:theme, defer:1

zstyle ':prompt:pure:git:stash' show no
zstyle ':prompt:pure:git:fetch' only_upstream yes
PURE_GIT_PULL=0

zstyle ':prompt:pure:prompt:success' color magenta
zstyle ':prompt:pure:prompt:error' color red
