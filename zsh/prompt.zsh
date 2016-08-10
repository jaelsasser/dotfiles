#
# SET ZSH PROMPT
#   Run before zplug load
#

# store the prompt pure symbol in psvar
function prompt_pure_set_symbol {
    case ${KEYMAP} in
      (vicmd)      export psvar[1]='❮'   ;;
      (main|viins) export psvar[1]='❯'   ;;
      (*)          export psvar[1]='❯'   ;;
    esac
}

function prompt_pure_dynamic_symbol {
    # register callbacks on viins <-> vicmd, redraw prompt
    function zle-keymap-select {
        local __old=${psvar[1]:-'no'}
        prompt_pure_set_symbol
        if [ "$__old" != "$psvar[2]" ]; then
            zle reset-prompt
        fi
    }
    zle -N zle-keymap-select

    # register callback on prompt precmd to redraw symbol
    autoload -Uz add-zsh-hook
    add-zsh-hook precmd prompt_pure_set_symbol

    export psvar[1]=${PROMPT_PURE_SYMBOL:-❯}
    export PROMPT=${PROMPT/${PROMPT_PURE_SYMBOL:-❯}/%v}
}

zplug "mafredri/zsh-async", nice:5
zplug "sindresorhus/pure", nice:7, \
    hook-load:'prompt_pure_dynamic_symbol'

