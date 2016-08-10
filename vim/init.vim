if !has('nvim')
    set directory=$XDG_CACHE_HOME/vim,~/,/tmp
    set backupdir=$XDG_CACHE_HOME/vim,~/,/tmp
    set viminfo+=n$XDG_CACHE_HOME/vim/viminfo
    set runtimepath=$XDG_CONFIG_HOME/vim,$XDG_CONFIG_HOME/vim/after,$VIMRUNTIME

    filetype plugin on
    filetype indent on
    syntax enable
endif

call plug#begin()

Plug 'junegunn/vim-peekaboo'
Plug 'justinmk/vim-dirvish'
Plug 'justinmk/vim-sneak'
Plug 'kana/vim-textobj-user'
Plug 'ntpeters/vim-better-whitespace'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'

" Navigation
Plug 'bogado/file-line'
Plug 'justinmk/vim-dirvish'

" fzf
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'junegunn/fzf.vim'

" Languages
Plug 'mphe/grayout.vim', { 'for': ['c', 'cpp'] }
Plug 'octol/vim-cpp-enhanced-highlight', { 'for': ['c', 'cpp'] }
Plug 'bps/vim-textobj-python', { 'for': 'python' }

" Markdown
Plug 'junegunn/goyo.vim', { 'for': 'markdown' }
Plug 'junegunn/limelight.vim', { 'for': 'markdown' }
Plug 'reedes/vim-lexical', { 'for': 'markdown' }
Plug 'reedes/vim-litecorrect', { 'for': 'markdown' }
Plug 'reedes/vim-pencil', { 'for': 'markdown' }
Plug 'reedes/vim-textobj-sentence', { 'for': 'markdown' }
Plug 'tpope/vim-markdown', { 'for': 'markdown' }

" Appearance
Plug 'airblade/vim-gitgutter'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Themes
Plug 'altercation/vim-colors-solarized'


Plug 'Valloric/YouCompleteMe', {
        \ 'do': 'python3 install.py --clang-completer',
        \ 'on': 'Heavyweight',
    \ }

" never loaded -- only to pull down scripts
Plug 'rdnetto/YCM-Generator', { 'branch': 'stable', 'on': [] }

" will be loaded one day
" Plug 'Shougo/deoplete.nvim', {
"         \ 'on': 'Heavyweight',
"     \ }
" Plug 'zchee/deoplete-jedi', {
"         \ 'for': 'python',
"         \ 'on': 'Heavyweight',
"     \ }

call plug#end()

command! Heavyweight call plug#load('YouCompleteMe')
                  \| call youcompleteme#Enable()

augroup filetypes
    autocmd FileType c set tabstop=8 shiftwidth=8 noexpandtab
    autocmd FileType cpp set tabstop=2 shiftwidth=2 expandtab
augroup END

"" spaces > tabs
set tabstop=4
set shiftwidth=4
set expandtab
"" I've never opened a swapfile either
set noswapfile
set hidden
"" anything to make scrolling smoother
set lazyredraw
set updatetime=750

"
" solarized
"" default to the dark background
set background=dark
colorscheme solarized
"" source ToggleBG function from solarized
call togglebg#map("")

"
"" basics
set number
set colorcolumn=80
set nowrap
"" keep a bit of a buffer at the bottom of the screen
set scrolloff=8
"" only show the filename as the title
set title titlestring=%F
"" these options can make scrolling in vsplits slow
" set relativenumber
" set cursorline

"
" keybindings
"" sensible mapleader
let mapleader = "\<Space>"
"" disable mouse
set mouse=
" trim clutter from grep-like tools
set wildignore+=*.pyc,*.bak,*/tmp/*,*.so,*.swp,*.zip
" use cscope + ctags, search cscope first
set cscopetag csto=1
nn <leader>jd :YcmCompleter GoTo<CR>

"
" plugin: fzf (ctrl-p on steroids)
"" leader keys for shortcuts
nmap <leader>ff :Files<cr>
nmap <leader>fg :GitFiles<cr>
nmap <leader>fb :Buffers<cr>
nmap <leader>fT :Tags<cr>
nmap <leader>ft :BTags<cr>
nmap <leader>fa :Ag<Space>

"
" plugin: ag.vim
"" search from the git project root
let g:ag_working_path_mode="r"

"
" plugin: gitgutter
"" the keybindings are super useful
let g:gitgutter_map_keys = 1

"
" plugin: YouCompleteMe
let g:ycm_python_binary_path = 'python3'
let g:ycm_show_diagnostics_ui = 0
"" <C-Space> only completion
let g:ycm_auto_trigger = 0
"" close preview window when done insert
let g:ycm_autoclose_preview_window_after_insertion = 1
"" autoload anything in my git projects dir, build output dir
let g:ycm_extra_conf_globlist = ['~/git/*', '/data/build/*']
"" extra sources
let g:ycm_seed_identifiers_with_syntax = 0
let g:ycm_collect_identifiers_from_tags_files = 1

"
" plugin: grayout.vim
"" autoload
let g:grayout_confirm = 0

"
" plugin: vim-sneak
"
let g:sneak#streak = 1

"
" plugin: projectionist.vim
"" default heuristic
let g:projectionist_heuristics = {
      \   "cpp/src/": {
      \     "*.cpp": {
      \        "alternate": "{}.h",
      \     },
      \     "*.h": {
      \        "alternate": "{}.cpp",
      \     }
      \   }
      \ }

"
" plugin: airline
"" appearance
let g:airline_powerline_fonts = 1
let g:airline_left_sep=''
let g:airline_right_alt_sep=''
let g:airline_right_sep=''
let g:airline_right_alt_sep=''
"" sections
let g:airline#extensions#whitespace#enabled = 0
let g:airline#extensions#hunks#non_zero_only = 1
let g:airline#extensions#tagbar#enabled = 1
"" tabline -- I don't like the arrow symbols
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_min_count = 2
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#left_sep = ''
let g:airline#extensions#tabline#left_alt_sep = ''
let g:airline#extensions#tabline#right_sep = ''
let g:airline#extensions#tabline#right_alt_sep = ''

"
" shameless copy-paste-from-random-internet-sources
"" local chdir relative to the current file (not for /tmp/)
aug local_chdir
	au BufEnter * if expand("%:p:h") !~ '^/tmp' | silent! lcd %:p:h | endif
aug END

"" save cursor position (but not for gitcommit files)
aug cursor_memory
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") && &filetype != "gitcommit" |
            \ exe "normal! g'\"" |
        \ endif
aug END

"" use ag instead of grep
aug use_ag
    if executable('ag')
        set grepprg=ag\ --nogroup\ --nocolor\ --column
        set grepformat=%f:%l:%c%m
    endif
aug END
