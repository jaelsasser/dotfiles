# elspure.zsh
# Minimal version of the pure theme:
#   https://github.com/sindresorhus/pure
#
# Authors:
#   Josh Elsasser 
#

# turns seconds into human readable time
# 165392 => 1d 21h 56m 32s
# https://github.com/sindresorhus/pretty-time-zsh
prompt_pure_human_time_to_var() {
	local human=" " total_seconds=$1 var=$2
	local days=$(( total_seconds / 60 / 60 / 24 ))
	local hours=$(( total_seconds / 60 / 60 % 24 ))
	local minutes=$(( total_seconds / 60 % 60 ))
	local seconds=$(( total_seconds % 60 ))
	(( days > 0 )) && human+="${days}d "
	(( hours > 0 )) && human+="${hours}h "
	(( minutes > 0 )) && human+="${minutes}m "
	human+="${seconds}s"

	# store human readable time in variable as specified by caller
	typeset -g "${var}"="${human}"
}

# stores (into prompt_elspure_cmd_exec_time) the exec time of the last command if set threshold was exceeded
prompt_elspure_check_cmd_exec_time() {
	integer elapsed
	(( elapsed = EPOCHSECONDS - ${prompt_elspure_cmd_timestamp:-$EPOCHSECONDS} ))
	prompt_elspure_cmd_exec_time=
	(( elapsed > ${PURE_CMD_MAX_EXEC_TIME:=5} )) && {
		prompt_pure_human_time_to_var $elapsed "prompt_elspure_cmd_exec_time"
	}
}

prompt_elspure_set_title() {
	print -n '\e]0;'
    case $1 in
        expand-prompt)
            print -Pn $2;;
        ignore-escape)
            print -rn $2;;
    esac
    # show hostname if connected through ssh
    [[ -n $SSH_CONNECTION ]] && print -Pn ' (%m)'
	print -n '\a'
}

prompt_elspure_preexec() {
    # save timestamp
	prompt_elspure_cmd_timestamp=$EPOCHSECONDS
	# shows the current dir and executed command in the title while a process is active
	prompt_elspure_set_title 'ignore-escape' "[$2]"
}

prompt_elspure_dirinfo() {
    local git_dir
    if [[ -n "${vcs_info_msg_1_}" ]]; then
        # strip the annoying '.'
        git_dir="${vcs_info_msg_2_##*.}"
        [[ -n "${git_dir}" ]] && vcs_info_msg_1_+="/${git_dir}"
        
        # override with git dir if in repo
        git_dir="${vcs_info_msg_1_}"
    fi
    prompt_elspure_dir="%(4~,${git_dir},%4~)"
	prompt_elspure_set_title 'expand-prompt' "${prompt_elsterm_dir}"
}

prompt_elspure_precmd() {
	# check exec time and store it in a variable
	prompt_elspure_check_cmd_exec_time
	prompt_elspure_cmd_timestamp=
   

	# get vcs info, dir info
	vcs_info
    prompt_elspure_dirinfo

    # pad out the prompt lines (not rendered on ^L)
    print -P "${PPROMPT}"

	# remove the prompt_elspure_cmd_timestamp, indicating that precmd has completed
	unset prompt_elspure_cmd_timestamp
}

prompt_elspure_setprompts() {
    # show prompt symbol; red if last comand failed 
    PROMPT='%(?.%F{white}.%F{red})»%f '

    # execution time
	local prompt_time='%F{yellow}${prompt_elspure_cmd_exec_time}%f'
	# git info
	local prompt_git='%F{242}${vcs_info_msg_0_}%f'
    # path info
    local prompt_path='%F{blue}${prompt_elspure_dir}%f'

	# show username@host if logged in through SSH
	[[ -n "$SSH_CONNECTION" ]] && prompt_elspure_username=' %F{242}%n@%m%f'
	# show username@host if root, with username in white
	[[ $UID -eq 0 ]] && prompt_elspure_username=' %F{white}%n%f%F{242}@%m%f'
    # set username / machine info if applicable
    local prompt_user='$prompt_elspure_username'
    
    # RPROMPT="${prompt_time} ${prompt_user} ${prompt_git} ${prompt_path}"
    # PPROMPT=
        
    # reverse order of prompt when using the preprompt
    RPROMPT=
    PPROMPT="\n${prompt_path} ${prompt_git} ${prompt_user} ${prompt_time}"
}

prompt_elspure_setup() {
	# prevent percentage showing up
	# if output doesn't end with a newline
	export PROMPT_EOL_MARK=''
    
    # right-prompt at edge
    #export ZLE_RPROMPT_INDENT=0

	prompt_opts=(subst percent)

	zmodload zsh/datetime
	autoload -Uz add-zsh-hook
	autoload -Uz vcs_info

	add-zsh-hook precmd prompt_elspure_precmd
	add-zsh-hook preexec prompt_elspure_preexec
    
    # Set vcs_info parameters.
    zstyle ':vcs_info:*' enable git
	zstyle ':vcs_info:*' use-simple true
    # use the longer, potentially slower check-for-changes for accuracy
    zstyle ':vcs_info:*' check-for-changes true
    zstyle ':vcs_info:*' stagedstr '#'
    zstyle ':vcs_info:*' unstagedstr '*' 
    # only export two msg variables from vcs_info
	zstyle ':vcs_info:*' max-exports 3
	zstyle ':vcs_info:git*' formats '%b%c%u' '%r' '%S'
	zstyle ':vcs_info:git*' actionformats '%b|%a' '%r' '%S'
    zstyle ':vcs_info:git*+set-message:*' hooks git_status

    # export pre-prompt string
    export PPROMPT=
    # set the initial prompts
    prompt_elspure_setprompts
}

prompt_elspure_setup "$@"
