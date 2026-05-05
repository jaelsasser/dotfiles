#
# Pure prompt configuration
#
readonly PURE_PROMPT_SYMBOL='$'

zstyle ':prompt:pure:git:stash' show no
zstyle ':prompt:pure:git:fetch' only_upstream yes
readonly PURE_GIT_PULL=0

zstyle ':prompt:pure:prompt:success' color magenta
zstyle ':prompt:pure:prompt:error' color red
