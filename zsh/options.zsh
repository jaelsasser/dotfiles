#
# OPTIONS for zsh
#
setopt NOTIFY               # report background job status immediately
unsetopt LIST_BEEP          # don't beep on ambiguous completion

setopt APPEND_HISTORY       # each session appends to the same hist file
setopt SHARE_HISTORY        # each session reads from the live hist file

setopt HIST_EXPIRE_DUPS_FIRST # delete dups before uniques from history
setopt HIST_FIND_NO_DUPS    # don't dispaly dups of previously found cmds
setopt HIST_IGNORE_DUPS     # don't repeatedly add the same cmd to hist
setopt HIST_VERIFY          # run hist commands through editor before exec

setopt CHASE_DOTS           # don't take shortcuts resolving "foo/.."
setopt EXTENDED_GLOB        # '#', '~', and '^' as in filename patterns
setopt NOMATCH              # print error on non-matching pattern

setopt AUTO_PUSHD           # cd pushes to the dir stack on every call
setopt PUSHD_IGNORE_DUPS    # keep the directory stack clean
setopt PUSHD_MINUS          # swaps "+" and "-" when selecting from stack
setopt PUSHD_SILENT         # don't print the dir stack after pushd/popd
setopt PUSHD_TO_HOME        # pushd with no args -> pushd $HOME
