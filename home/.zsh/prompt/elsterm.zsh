# elsterm.zsh
# Based on:
#   https://github.com/sindresorhus/pure
#   prezto's 'minimal' theme
#
# Authors:
#   Josh Elsasser 
#

# For my own and others sanity (via pure.zsh)
# git:
# %b => current branch
# %a => current action (rebase/merge)
# prompt:
# %F => color dict
# %f => reset color
# %~ => current path
# %* => time
# %n => username
# %m => shortname host
# %(?..) => prompt conditional - %(condition.true.false)
# terminal codes:
# \e7   => save cursor position
# \e[2A => move cursor 2 lines up
# \e[1G => go to position 1 in terminal
# \e8   => restore cursor position
# \e[K  => clears everything after the cursor on the current line
# \e[2K => clear everything on the current line


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

# stores (into prompt_elsterm_cmd_exec_time) the exec time of the last command if set threshold was exceeded
prompt_elsterm_check_cmd_exec_time() {
	integer elapsed
	(( elapsed = EPOCHSECONDS - ${prompt_elsterm_cmd_timestamp:-$EPOCHSECONDS} ))
	prompt_elsterm_cmd_exec_time=
	(( elapsed > ${PURE_CMD_MAX_EXEC_TIME:=5} )) && {
		prompt_pure_human_time_to_var $elapsed "prompt_elsterm_cmd_exec_time"
	}
}

prompt_elsterm_set_title() {
	print -n '\e]0;'
    # show hostname if connected through ssh
    case $1 in
        expand-prompt)
            print -Pn $2;;
        ignore-escape)
            print -rn $2;;
    esac
    [[ -n $SSH_CONNECTION ]] && print -Pn ' (%m)'
	print -n '\a'
}

prompt_elsterm_preexec() {
	prompt_elsterm_cmd_timestamp=$EPOCHSECONDS

	# shows the current dir and executed command in the title while a process is active
	prompt_elsterm_set_title 'ignore-escape' "[$2]"
}

prompt_elsterm_dir() {
    if [[ -n "${vcs_info_msg_1_}" ]]; then
        # strip the annoying '.'
        local tmp="${vcs_info_msg_2_##*.}"
        [[ -n "${tmp}" ]] && vcs_info_msg_1_+="/${tmp}"
        
        # override with git dir if in repo
        prompt_elsterm_dir_="${vcs_info_msg_1_}"
    else
        #
        prompt_elsterm_dir_="%3~"
    fi
}

prompt_elsterm_precmd() {
	# check exec time and store it in a variable
	prompt_elsterm_check_cmd_exec_time
	prompt_elsterm_cmd_timestamp=

	# shows the full path in the title
	prompt_elsterm_set_title 'expand-prompt' '%5~'

	# get vcs info
	vcs_info

    # generate the path
    prompt_elsterm_dir
    # pad out the prompt lines (not rendered on ^L)
    print -Pn "\n"

	# remove the prompt_elsterm_cmd_timestamp, indicating that precmd has completed
	unset prompt_elsterm_cmd_timestamp
}

prompt_elsterm_setprompts() {
	# show username@host if logged in through SSH
	[[ -n "$SSH_CONNECTION" ]] && prompt_elsterm_username=' %F{242}%n@%m%f'
	# show username@host if root, with username in white
	[[ $UID -eq 0 ]] && prompt_elsterm_username=' %F{white}%n%f%F{242}@%m%f'
    
    # show prompt symbol; red if last comand failed 
    PROMPT=' %(?.%F{white}.%F{red})»%f '
   
    RPROMPT=''
	# execution time
	RPROMPT+='%F{yellow}${prompt_elsterm_cmd_exec_time}%f'
	# username and machine if applicable
	RPROMPT+=' $prompt_elsterm_username'
	# git info
	RPROMPT+=' %F{242}${vcs_info_msg_0_}%f'
    # path info
    RPROMPT+=' %F{blue}${prompt_elsterm_dir_}%f'
}

prompt_elsterm_setup() {
	# prevent percentage showing up
	# if output doesn't end with a newline
	export PROMPT_EOL_MARK=''
    
    # right-prompt at edge
    #export ZLE_RPROMPT_INDENT=0

	prompt_opts=(subst percent)

	zmodload zsh/datetime
	autoload -Uz add-zsh-hook
	autoload -Uz vcs_info

	add-zsh-hook precmd prompt_elsterm_precmd
	add-zsh-hook preexec prompt_elsterm_preexec
    
    # Set vcs_info parameters.
    zstyle ':vcs_info:*' enable git
	zstyle ':vcs_info:*' use-simple true
    # use the longer, potentially slower check-for-changes for accuracy
    zstyle ':vcs_info:*' check-for-changes true
    zstyle ':vcs_info:*' stagedstr '*'
    # only export two msg variables from vcs_info
	zstyle ':vcs_info:*' max-exports 3
	zstyle ':vcs_info:git*' formats '%b%c' '%r' '%S'
	zstyle ':vcs_info:git*' actionformats '%b|%a' '%r' '%S'
    zstyle ':vcs_info:git*+set-message:*' hooks git_status

    # resize the prompt display whenver a window change is detected 
    # functions[TRAPWINCH]="${functions[TRAPWINCH]//prompt_elsterm_refresh} 
    #     prompt_elsterm_setprompts"
    
    prompt_elsterm_setprompts
}

prompt_elsterm_setup "$@"
