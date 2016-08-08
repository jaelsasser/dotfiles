#
# Executes commands at login post-zshrc.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

{
  # Compile the completion dump to increase startup speed.
  zcompdump="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/completion_cache"
  if [[ -s "$zcompdump" && (! -s "${zcompdump}.zwc" || "$zcompdump" -nt "${zcompdump}.zwc") ]]; then
    zcompile "$zcompdump"
  fi
} &!

