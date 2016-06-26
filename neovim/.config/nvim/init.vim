call plug#begin('~/.vim/plugged')
Plug 'justinmk/vim-dirvish'
Plug 'ntpeters/vim-better-whitespace'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'

" Navigation
Plug 'bogado/file-line'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'justinmk/vim-dirvish'

" C/C++
Plug 'mphe/grayout.vim', { 'for': ['c', 'cpp'] }
Plug 'octol/vim-cpp-enhanced-highlight', { 'for': ['c', 'cpp'] }

" Markdown
Plug 'junegunn/goyo.vim', { 'for': 'markdown' }
Plug 'junegunn/limelight.vim', { 'for': 'markdown' }
Plug 'reedes/vim-lexical', { 'for': 'markdown' }
Plug 'reedes/vim-litecorrect', { 'for': 'markdown' }
Plug 'reedes/vim-pencil', { 'for': 'markdown' }
Plug 'reedes/vim-textobj-sentence', { 'for': 'markdown' }
Plug 'tpope/vim-markdown', { 'for': 'markdown' }

" Themes and Appearance
Plug 'airblade/vim-gitgutter'
Plug 'majutsushi/tagbar'
Plug 'chriskempson/base16-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" deferred load
Plug 'Valloric/YouCompleteMe', { 'do': 'sudo ./install.py --clang-completer', 'on': 'Heavyweight' }
" never loaded -- only to pull down scripts
Plug 'rdnetto/YCM-Generator', { 'branch': 'stable', 'on': [] }
call plug#end()

command! Heavyweight call plug#load('YouCompleteMe')
                  \| call youcompleteme#Enable()
                  \| nmap <leader>gr :GrayoutUpdate<cr>
                  \| nmap <leader>gR :GrayoutReloadConfig<cr>
                  \| nmap <leader>gc :GrayoutClear<cr>
                  \| set nocursorline norelativenumber
                  \| set noexpandtab tabstop=8 shiftwidth=8

"" sane defaults
filetype plugin on
filetype indent on
set tabstop=4 shiftwidth=4 expandtab
set noswapfile nobackup hidden
"" anything to make scrolling smoother
set lazyredraw
set updatetime=750

"
" appearance
"" basics
set number relativenumber
set cursorline
set colorcolumn=80
set scrolloff=8
"" only show the filename as the title
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
" use cscope + ctags, search cscope first
set cscopetag csto=0
nn <leader>jd :YcmCompleter GoTo<CR>

"
" custom setup for markdown files
"
autocmd FileType markdown,mkd so $HOME/.vim/writing.vim

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
"
let g:gitgutter_map_keys = 1

"
" plugin: YouCompleteMe
"" <C-Space> only completion
let g:ycm_auto_trigger = 0
"" close preview window when done insert
let g:ycm_autoclose_preview_window_after_insertion = 1
"" autoload anything in my git projects dir
let g:ycm_extra_conf_globlist = ['~/git/*', '/data/build/*']
"" extra sources
let g:ycm_seed_identifiers_with_syntax = 0
let g:ycm_collect_identifiers_from_tags_files = 1

"
" plugin: grayout.vim
"" autoload
let g:grayout_confirm = 0

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
