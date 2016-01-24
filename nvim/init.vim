" vim-plug
call plug#begin('~/.vim/plugged')
Plug 'airblade/vim-gitgutter'
Plug 'chriskempson/base16-vim'
Plug 'kien/ctrlp.vim'
Plug 'reedes/vim-pencil'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-sensible'
Plug 'vim-airline/vim-airline'
call plug#end()

" housekeeping
filetype plugin on
set tabstop=2 softtabstop=0 expandtab shiftwidth=4 smarttab

" appearance
set number relativenumber
set background=dark
colorscheme base16-eighties

" keybindings
inoremap jk <ESC>
let mapleader = "\<Space>"
"" rage-inducing changes
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>

" plugin: pencil
augroup pencil
  autocmd!
  autocmd FileType markdown,mkd,md call pencil#init()
  autocmd FileType text 	   call pencil#init()
augroup END

" plugin: airline
"" appreance
let g:airline_powerline_fonts = 1
let g:airline_left_sep=''
let g:airline_right_sep=''
"" sections
let g:airline#extensions#whitespace#enabled = 0
let g:airline#extensions#hunks#non_zero_only = 1

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
