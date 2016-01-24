" defaults
filetype plugin on
filetype indent on

syntax on
set encoding=utf-8

set number relativenumber
set mouse=

" remaps
inoremap jk <ESC>
let mapleader = "\<Space>"

" vim-plug
call plug#begin('~/.vim/plugged')

Plug 'chriskempson/base16-vim'
Plug 'altercation/vim-colors-solarized'

Plug 'tpope/vim-rsi'

Plug 'vim-airline/vim-airline'
Plug 'airblade/vim-gitgutter'

call plug#end()

" theme
set background=dark
colorscheme base16-eighties

" airline
let g:airline_powerline_fonts = 1
