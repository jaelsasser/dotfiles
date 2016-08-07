# source all zshrc.d file in interactive terminals
if ! [[ "$TERM" == 'dumb' ]]; then
    for f in $ZDOTDIR/zshrc.d/*.zsh; do
        source $f || echo "Failed to source $f"
    done
fi
