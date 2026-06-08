# mise: hook the runtime manager into the shell, but only where it's installed —
# this snippet rides to every host, mise doesn't.
(( $+commands[mise] )) && eval "$(mise activate zsh)"
