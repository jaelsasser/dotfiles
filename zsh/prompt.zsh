#
# SET ZSH PROMPT
#   Run before zplug load
#

# store the prompt pure symbol in psvar
function __prompt_pure_set_symbol {
    case ${KEYMAP} in
      (vicmd)      psvar[1]='❮'   ;;
      (*)          psvar[1]='❯'   ;;
    esac
}

function __prompt_pure_dynamic_symbol {
    # register callbacks on exit/enter vicmd, redraw prompt
    function zle-keymap-select {
        local __old=${psvar[1]:-'no'}
        __prompt_pure_set_symbol
        if [[ "$__old" != "$psvar[1]" ]]; then
            zle reset-prompt
        fi
    }
    zle -N zle-keymap-select

    # register callback on prompt precmd to redraw symbol
    autoload -Uz add-zsh-hook
    add-zsh-hook precmd __prompt_pure_set_symbol

    psvar[1]=❯
    export PROMPT=${PROMPT/❯/%v}
}

zplug "mafredri/zsh-async", defer:0
zplug "sindresorhus/pure", defer:1, \
    hook-load:'__prompt_pure_dynamic_symbol'
