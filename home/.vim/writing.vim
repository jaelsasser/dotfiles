" initialize helper plugins
call pencil#init()
call lexical#init()
call litecorrect#init()
call textobj#sentence#init()

" set up mapleader keys for plugins
nnoremap <Leader>[ :Goyo<CR>
nnoremap <Leader>] :Limelight!!<CR>

" highlight strings in markdown
syn region mdString start=+"+ end=+"+ end=/$/
hi def link mdString String

" highlight comments in markdown
syn region mdComment start="//" skip="\\$" end="$"
hi def link mdComment Comment

"
" plugin: pencil
"
let g:pencil#wrapModeDefault = 'soft'

"
" plugin: goyo
"
let g:goyo_width = '80'
let g:goyo_height = '100%'
