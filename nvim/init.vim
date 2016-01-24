" remaps
inoremap jk <ESC>
let mapleader = "\<Space>"

" vim-plug
call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-rsi'

Plug 'chriskempson/base16-vim'

Plug 'vim-airline/vim-airline'
Plug 'airblade/vim-gitgutter'
call plug#end()

" theme
set background=dark
colorscheme base16-eighties

" airline
let g:airline_powerline_fonts = 1
