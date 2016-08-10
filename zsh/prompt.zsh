#
# SET ZSH PROMPT
#   Run before zplug load
#

# store the prompt pure symbol in psvar
function prompt_pure_set_symbol {
    case ${KEYMAP} in
      (vicmd)      psvar[1]='❮'   ;;
      (main|viins) psvar[1]='❯'   ;;
      (*)          psvar[1]='❯'   ;;
    esac
}

function prompt_pure_dynamic_symbol {
    # register callbacks on viins <-> vicmd, redraw prompt
    function zle-keymap-select {
        local __old=${psvar[1]:-'no'}
        prompt_pure_set_symbol
        if [ "$__old" != "$psvar[1]" ]; then
            zle reset-prompt
        fi
    }
    zle -N zle-keymap-select

    # register callback on prompt precmd to redraw symbol
    autoload -Uz add-zsh-hook
    add-zsh-hook precmd prompt_pure_set_symbol

    psvar[1]=❯
    export PROMPT=${PROMPT/❯/%v}
}

zplug "mafredri/zsh-async", nice:5
zplug "sindresorhus/pure", nice:7, \
    hook-load:'prompt_pure_dynamic_symbol'

