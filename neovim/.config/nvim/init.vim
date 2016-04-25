call plug#begin('~/.vim/plugged')
Plug 'airblade/vim-gitgutter'
Plug 'bogado/file-line'
Plug 'christoomey/vim-sort-motion'
Plug 'JCLiang/vim-cscope-utils'
Plug 'kana/vim-textobj-user'
Plug 'rking/ag.vim'
Plug 'ntpeters/vim-better-whitespace'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'vim-airline/vim-airline'
Plug 'xolox/vim-misc'| Plug 'xolox/vim-session'

" manual deferred load
Plug 'benekastah/neomake', { 'on' : [] }
Plug 'majutsushi/tagbar', { 'on' : [] }
Plug 'Valloric/YouCompleteMe', { 'do': './install.py --clang-completer', 'on' : [] }
Plug 'rdnetto/YCM-Generator', { 'branch': 'stable', 'on' : [] }

" fzf
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Markdown
Plug 'junegunn/goyo.vim', { 'for' : 'markdown' }
Plug 'junegunn/limelight.vim', { 'for' : 'markdown' }
Plug 'reedes/vim-lexical', { 'for' : 'markdown' }
Plug 'reedes/vim-litecorrect', { 'for' : 'markdown' }
Plug 'reedes/vim-pencil', { 'for' : 'markdown' }
Plug 'reedes/vim-textobj-sentence', { 'for' : 'markdown' }
Plug 'tpope/vim-markdown', { 'for' : 'markdown' }

" Python
Plug 'bps/vim-textobj-python', { 'for' : 'python' }
"Plug 'python-rope/ropevim', { 'for' : 'python', 'on': 'RopeOpenProject' }

" C/C++
Plug 'octol/vim-cpp-enhanced-highlight', { 'for' : 'cpp' }
Plug 'vim-scripts/ifdef-highlighting', { 'for' : [ 'c', 'cpp' ] }
Plug 'vivien/vim-linux-coding-style', { 'for' : 'c' }

" Themes
Plug 'chriskempson/base16-vim'
Plug 'vim-airline/vim-airline-themes'
call plug#end()

command! Heavyweight silent echo 'Loading expensive plugins...'
                  \| call plug#load('YouCompleteMe', 'tagbar', 'neomake', 'YCM-Generator')
                  \| call youcompleteme#Enable()

"
" housekeeping
"
filetype plugin on
filetype indent on
set tabstop=4 shiftwidth=4 expandtab
set colorcolumn=80
set relativenumber

set noswapfile
set nobackup
set hidden

" anything to make scrolling smoother
set lazyredraw
set updatetime=750
"
" appearance
"" sane defaults
set number "relativenumber
set cursorline
set scrolloff=8
set title titlestring=%F
"" base16 eighties
set background=dark
let base16colorspace=256
colorscheme base16-eighties
"
" keybindings
"" sensible mapleader
let mapleader = "\<Space>"
"" disable mouse
set mouse=
" trim clutter from grep-like tools
set wildignore+=*.pyc,*.bak,*/tmp/*,*.so,*.swp,*.zip
" use cscope + ctags, search ctags first
set cscopetag csto=1
nn <leader>jd :YcmCompleter GoTo<CR>
nn <Leader>t :TagbarToggle<CR>
" custom setup for markdown files
autocmd FileType markdown,mkd so $HOME/.vim/writing.vim
"
" plugin: vim-session
"
let g:session_autoload = 'no'
let g:session_autosave = 'no'
"
" plugin: fzf (ctrl-p on steroids)
"" leader keys for shortcuts
nmap <leader>p  :GitFiles<cr>
nmap <leader>bb :Buffers<cr>
nmap <leader>f  :Ag<Space>
"
" plugin: ag.vim
"" search from the git project root
"let g:ag_working_path_mode="r"
"
" plugin: gitgutter
"
let g:gitgutter_map_keys = 0
"
" plugin: airline
"" appearance
let g:airline_theme='base16'
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

""
" shameless copy-paste-from-random-internet-sources
"" save cursor position (but not for gitcommit files)
aug cursor_memory
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") && &filetype != "gitcommit" |
            \ exe "normal! g'\"" |
        \ endif
aug END

"" handle the 'crap-I-forgot-sudo' edge case
aug sudo_hack
    cmap w!! w !sudo tee % >/dev/null
aug END

"" use ag
aug use_ag
    if executable('ag')
        set grepprg=ag\ --nogroup\ --nocolor\ --column
        set grepformat=%f:%l:%c%m
    endif
aug END
