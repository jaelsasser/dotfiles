" vim-plug
call plug#begin('~/.vim/plugged')
Plug 'airblade/vim-gitgutter'
Plug 'chriskempson/base16-vim'
Plug 'joe-skb7/cscope-maps', { 'for' : 'c' }
Plug 'junegunn/goyo.vim', { 'for' : 'markdown' }
Plug 'junegunn/limelight.vim', { 'for' : 'markdown' }
Plug 'kana/vim-textobj-user', { 'for' : 'markdown' }
Plug 'kien/ctrlp.vim'
Plug 'majutsushi/tagbar', { 'for' : 'c' }
Plug 'reedes/vim-lexical', { 'for' : 'markdown' }
Plug 'reedes/vim-litecorrect', { 'for' : 'markdown' }
Plug 'reedes/vim-pencil', { 'for' : 'markdown' }
Plug 'reedes/vim-textobj-sentence', { 'for' : 'markdown' }
Plug 'tpope/vim-markdown', { 'for' : 'markdown' }
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-sensible'
Plug 'vim-airline/vim-airline'
Plug 'vim-scripts/ifdef-highlighting', { 'for' : 'c' }
Plug 'vivien/vim-linux-coding-style', { 'for' : 'c' }
call plug#end()

"
" housekeeping
"
filetype plugin on
filetype indent on
set tabstop=4 shiftwidth=4 expandtab
set noswapfile
set nobackup

"
" appearance
"" sane defaults 
set number relativenumber
set cursorline
set scrolloff=8
set title
"" base16 eighties
set background=dark
let base16colorspace=256
colorscheme base16-eighties

"
" keybindings
"" sensible mapleader 
let mapleader = "\<Space>"
"" disable mouse, arrow keys 
set mouse=
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>

" 
" filetype: markdown (grouped under vim-pencil)
"
augroup pencil
    autocmd FileType markdown,mkd call pencil#init()
                              \ | call lexical#init()
                              \ | call litecorrect#init()
                              \ | call textobj#sentence#init()
                              \ | nnoremap <Leader>[ :Goyo<CR>
                              \ | nnoremap <Leader>] :Limelight!!<CR>
augroup end

"
" plugin: ctrlp
"" custom ignores via https://joshldavis.com/2014/04/05/vim-tab-madness-buffers-vs-tabs 
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/](\.(git|hg|svn)|\_site)$',
  \ 'file': '\v\.(exe|so|dll|class|png|jpg|jpeg)$',
\}
"" nearest .git directory
let g:ctrlp_working_path_mode = 'r'
"" leader keys for shortcuts
nmap <leader>p  :CtrlP<cr>
nmap <leader>bb :CtrlPBuffer<cr>
nmap <leader>bm :CtrlPMixed<cr>
nmap <leader>bs :CtrlPMRU<cr>

"
" plugin: pencil
"
let g:pencil#wrapModeDefault = 'soft'

"
" plugin: goyo
"
let g:goyo_width = '80'
let g:goyo_height = '100%'

"
" plugin: airline
"" appearance
let g:airline_powerline_fonts = 1
let g:airline_left_sep=''
let g:airline_right_alt_sep=''
let g:airline_right_sep=''
let g:airline_right_alt_sep=''
"" sections
let g:airline#extensions#whitespace#enabled = 1
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
" buffer remapping
"
set hidden
nmap <leader>T :enew<cr>
nmap <leader>l :bnext<CR>
nmap <leader>h :bprevious<CR>
nmap <leader>bq :bp <BAR> bd #<CR>
nmap <leader>bl :ls<CR>

"
" misc remapping
"" save cursor position (but not for gitcommit files)
aug cursor_memory
    au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") && &filetype != "gitcommit" | exe "normal! g'\"" | endif
aug END


"" handle the 'crap-I-forgot-sudo' edge case
cmap w!! w !sudo tee % >/dev/null

"" system-specific configuration
if executable('ag')
    set grepprg=ag\ --nogroup\ --nocolor\ --column
    set grepformat=%f:%l:%c%m

    " plugin: ctrlp + ag
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
    let g_ctrlp_use_caching = 0
endif

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
