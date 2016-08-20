if !has('nvim')
	set directory=$XDG_CACHE_HOME/vim,~/,/tmp
	set backupdir=$XDG_CACHE_HOME/vim,~/,/tmp
	set viminfo+=n$XDG_CACHE_HOME/vim/viminfo
	set runtimepath=$XDG_CONFIG_HOME/vim,$XDG_CONFIG_HOME/vim/after,$VIMRUNTIME

	filetype plugin on
	filetype indent on
	syntax enable
endif

source $XDG_CONFIG_HOME/vim/init.vim
