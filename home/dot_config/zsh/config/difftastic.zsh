# difftastic: structural `git diff` where the binary rides; log/show/magit
# default --no-ext-diff and stay textual.
(( $+commands[difft] )) && export GIT_EXTERNAL_DIFF=difft
