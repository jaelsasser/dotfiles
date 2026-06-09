# mise: hook the runtime manager into the shell, but only where it's installed —
# this snippet rides to every host, mise doesn't.
if (( $+commands[mise] )); then
    eval "$(mise activate zsh)"

    # A global config makes mise's tools live everywhere, so leave its stock
    # per-prompt hook alone. Absent one, gate the `hook-env` fork to the trees
    # that actually carry a mise.toml.
    _mise_dir=${XDG_CONFIG_HOME:-$HOME/.config}/mise
    _mise_global=( $_mise_dir/(config|mise).toml(N) $_mise_dir/conf.d/*.toml(N) )
    if (( ! $#_mise_global )); then
        # `inside || was` fires it once more on the way out, to tear the env down.
        _mise_scoped() {
            local inside=0
            case $PWD/ in
                $HOME/Repos/*|$HOME/Upstream/*|${XDG_DATA_HOME:-$HOME/.local/share}/chezmoi/*) inside=1 ;;
            esac
            (( inside || ${_mise_was_inside:-0} )) && "$@"
            _mise_was_inside=$inside
        }
        if (( $+functions[_mise_hook_precmd] )); then
            functions[_mise_precmd]=$functions[_mise_hook_precmd]
            _mise_hook_precmd() { _mise_scoped _mise_precmd }
        fi
        if (( $+functions[_mise_hook_chpwd] )); then
            functions[_mise_chpwd]=$functions[_mise_hook_chpwd]
            _mise_hook_chpwd() { _mise_scoped _mise_chpwd }
        fi
    fi
    unset _mise_dir _mise_global
fi
