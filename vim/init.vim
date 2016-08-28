"
" ENVIRONMENT BOOTSTRAP
"
if !has('nvim') && !empty($XDG_CONFIG_HOME)
    "shift as much vim cruft out of $HOME
    set directory=$XDG_CACHE_HOME/vim,/tmp
    set backupdir=$XDG_CACHE_HOME/vim,/tmp
    set viminfo+=n$XDG_CACHE_HOME/vim/viminfo
    set runtimepath=$XDG_CONFIG_HOME/vim,$XDG_CONFIG_HOME/vim/after,$VIMRUNTIME
endif
if !empty($XDG_DATA_HOME)
    "work out of XDG_DATA_HOME, if possible
    let g:python_host_prog = expand("$XDG_DATA_HOME/vim/venv/bin/python")
    let g:python3_host_prog = expand("$XDG_DATA_HOME/vim/venv/bin/python")
    let g:plug_home = expand("$XDG_DATA_HOME/vim/plugged")
endif

"
" VIM-PLUG
"
call plug#begin()

Plug 'airblade/vim-gitgutter'           "track file changes
Plug 'justinmk/vim-dirvish'             "slick wrapper on netrw
Plug 'justinmk/vim-sneak'               "lightweight easymotion
Plug 'ludovicchabant/vim-gutentags'     "ctags/cscope manager
Plug 'romainl/vim-qf'                   "quickfix window enhancements
Plug 'romainl/vim-qlist'                "use quickfix as much as possible
Plug 'tommcdo/vim-lion'                 "align text by char with 'gl', 'gL'
Plug 'tpope/vim-abolish'                "useful in way nobody can describe
Plug 'tpope/vim-characterize'           "more useful 'ga'
Plug 'tpope/vim-commentary'             "quick comment/uncomment
Plug 'tpope/vim-eunuch'                 "vim sugar on common unix commands
Plug 'tpope/vim-fugitive'               "vim sugar on some git commands
Plug 'tpope/vim-repeat'                 "extends default 'repeat action'
Plug 'tpope/vim-rsi'                    "love me some emacs keybindings
Plug 'tpope/vim-surround'               "adds a 's' (surround) textobj
Plug 'tpope/vim-unimpaired'             "great set of complimentary remaps
Plug 'wellle/targets.vim'               "beautifully extends default textobj

Plug 'lifepillar/vim-solarized8'        "solarized, some bullshit
Plug 'romainl/flattened'                "solarized, no bullshit

Plug 'kana/vim-textobj-user'            "depend for lots of textobjs
Plug 'bps/vim-textobj-python'           "objs -- f:function, c:class
Plug 'reedes/vim-textobj-sentence', {
            \ 'for': 'markdown',
            \ }

Plug 'ntpeters/vim-better-whitespace'   "TODO: reduce dep with :set list
Plug 'tpope/vim-obsession'              "TODO: use more || delete
Plug 'tpope/vim-projectionist'          "TODO: use more || delete

Plug 'junegunn/fzf', {
            \ 'do': './install --bin',
            \ 'dir': empty($XDG_DATA_HOME) ? '~/.fzf' :
            \   expand('$XDG_DATA_HOME/fzf')
            \ } | Plug 'junegunn/fzf.vim'

Plug 'reedes/vim-lexical'               "keymaps, sensible spellcheck
Plug 'reedes/vim-litecorrect'           "lightweight autocorrect
Plug 'octol/vim-cpp-enhanced-highlight' "tried to live without this, couldn't

"only-for-some-filetype jail
Plug 'mphe/grayout.vim', { 'for': ['c', 'cpp'] }
Plug 'junegunn/goyo.vim', { 'for': 'markdown' }
Plug 'junegunn/limelight.vim', { 'for': 'markdown' }
Plug 'reedes/vim-pencil', { 'for': 'markdown' }
Plug 'tpope/vim-markdown'

call plug#end()

"
" THEMING
"
set background=dark
let g:solarized_term_italics = 1
let g:solarized_statusline = 1
let g:solarized_visibility = 'low'
colorscheme solarized8_dark_high

"
" DEFAULTS
"   based off of tpope/vim-sensible
"
filetype plugin indent on
syntax enable

set autoindent
set smartindent
set backspace=indent,eol,start  "backspace eats through tab characters
set complete-=i
set smarttab

set nrformats-=octal            "don't show octal with ga

set ttimeout                    "wait for keycodes to complete
set ttimeoutlen=100             "timeout for keycodes

set formatoptions+=j            "delete comment chars when joining lines
set autoread                    "reload changed files when possible
set sessionoptions-=options     "don't save options in sessions
set noswapfile                  "never saw the point of .swp

set number                      "line numbers
set expandtab                   "spaces > tabs, whenever possible
set shiftwidth=4                "4 spaces per indent level
set tabstop=4                   "tab = 4 spaces (mostly for Makefiles)

set colorcolumn=79              "wrapped lines are ugly
set nowrap                      "explicitly wrap with :wrap
set scrolloff=8                 "small offset at the screen bottom
set scrolloff=5                 "small offset at the screen right
set title titlestring=%F        "show the filepath as the title

let mapleader="\<Space>"        "sensible mapleader
set mouse=                      "disable mouse
set cscopetag                   "use cscope tags in addition to ctags
set csto=1                      "search ctags before cscope

set incsearch                   "show matches while searching
if maparg('<C-L>', 'n') ==# ''  "<C-L> to clear incsearch hl
    nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
endif

"trim clutter from grep-like tools
set wildignore+=*.pyc,*.bak,*/tmp/*,*.so,*.swp,*.zip
"show trailing whitespace, tabs with ':set list'
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+

"
" PERFORMANCE TWEAKS
"
set lazyredraw                  "no screen redraw during macros

"
" CUSTOM STATUSLINE
"
set statusline=                             "rhs statusline
set stl+=\ %t%m\                            "relative file path
set stl+=%(\ %{fugitive#head(1)}%)\        "current git branch
set statusline+=%=                          "lhs statusline
set stl+=%(%{&filetype}\ %)                 "filetype
set stl+=%([%W%R]\ %)                       "flags
set stl+=%10(\ %4l:%4L☰\ :%3v\ %)          "cursor/line position, %age

"
" KEYMAPS
"
nnoremap <leader>ff :Files<cr>
nnoremap <leader>fg :GitFiles<cr>
nnoremap <leader>fb :Buffers<cr>
nnoremap <leader>fT :Tags<cr>
nnoremap <leader>ft :BTags<cr>
nnoremap <leader>fa :Ag<Space>

"
" PLUGIN-SPECIFIC SETTINGS
"
let g:gitgutter_map_keys = 1    "use the gitgutter keybinds
let g:grayout_confirm = 0       "autoload grayout.vim config files
let g:sneak#streak = 1          "vim-sneak smart streak mode
"try and match <file>.{cpp,h} together
let g:projectionist_heuristics = {
      \   "cpp/": {
      \     "*.cpp": {
      \        "alternate": "{}.h",
      \     },
      \     "*.h": {
      \        "alternate": "{}.cpp",
      \     }
      \   }
      \ }

"
" CUSTOM FUNCTIONS
"   mostly stolen from various dotfile repos
"
aug local_chdir                 "not for the /tmp dir
    au BufEnter * if expand("%:p:h") !~ '^/tmp' | silent! lcd %:p:h | endif
aug END
aug save_cursor_position        "skip gitcommits, etc
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") && &filetype != "gitcommit" |
            \ exe "normal! g'\"" |
        \ endif
aug END
aug use_ag                      "ag is faster than grep, use if installed
    if executable('ag')
        set grepprg=ag\ --nogroup\ --nocolor\ --column
        set grepformat=%f:%l:%c%m
    endif
aug END
