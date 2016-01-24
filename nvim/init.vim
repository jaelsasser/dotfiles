" vim-plug
call plug#begin('~/.vim/plugged')
Plug 'airblade/vim-gitgutter'
Plug 'chriskempson/base16-vim'
Plug 'kien/ctrlp.vim'
Plug 'reedes/vim-lexical', { 'for' : 'markdown' }
Plug 'reedes/vim-litecorrect', { 'for' : 'markdown' }
Plug 'reedes/vim-pencil', { 'for' : 'markdown' }
Plug 'reedes/vim-textobj-sentence', { 'for' : 'markdown' }
Plug 'tpope/vim-markdown', { 'for' : 'markdown' }
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-sensible'
Plug 'vim-airline/vim-airline'
Plug 'junegunn/limelight.vim', { 'for' : 'markdown' }
Plug 'junegunn/goyo.vim', { 'for' : 'markdown' }
Plug 'kana/vim-textobj-user', { 'for' : 'markdown' }
call plug#end()

" housekeeping
filetype plugin on
filetype indent on
set tabstop=2 shiftwidth=2 expandtab
set noswapfile

" appearance
set number relativenumber
set background=dark
set scrolloff=8

let base16colorspace=256
colorscheme base16-eighties

" keybindings
inoremap jk <ESC>
let mapleader = "\<Space>"
"" rage-inducing changes
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>

" filetype: markdown 
autocmd FileType markdown,mkd call pencil#init()
                          \ | call lexical#init()
                          \ | call litecorrect#init()
                          \ | call textobj#sentence#init()
                          \ | nnoremap <Leader>[ :Goyo<CR>
                          \ | nnoremap <Leader>] :Limelight!!<CR>

" plugin: pencil
let g:pencil#wrapModeDefault = 'soft'

" plugin: goyo
let g:goyo_width = '80'
let g:goyo_height = '100%'

" plugin: airline
"" appreance
let g:airline_powerline_fonts = 1
let g:airline_left_sep=''
let g:airline_right_alt_sep=''
let g:airline_right_sep=''
let g:airline_right_alt_sep=''
"" sections
let g:airline#extensions#whitespace#enabled = 0
let g:airline#extensions#hunks#non_zero_only = 1
"let g:airline#extensions#tabline#enabled = 1
"let g:airline#extensions#tabline#buffer_min_count = 2

" save cursor position (but not for gitcommit files)
aug cursor_memory
    au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") && &filetype != "gitcommit" | exe "normal! g'\"" | endif
aug END

" system-specific configuration
let s:uname = system("echo -n \"$(uname)\"")
if !v:shell_error
    if s:uname == "Darwin"
        " Yank text to the OS X clipboard
        noremap <leader>y "*y
        noremap <leader>yy "*Y
        " Preserve indentation while pasting text from the OS X clipboard
        noremap <leader>p :set paste<CR>:put  *<CR>:set nopaste<CR>
    else
    
    endif
endif
