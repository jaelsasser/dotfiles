" initialize helpers
call pencil#init()
call lexical#init()
call litecorrect#init()
call textobj#sentence#init()

" don't lose progress
set autowriteall

" highlight strings in markdown
"syn region mdString start=+"+ end=+"+ end=/\n\w*\n/
"hi def link mdString String

" highlight comments in markdown
"syn region mdComment start="//" skip="\\$" end="$"
"hi def link mdComment Comment

"
" plugin: pencil
"
let g:pencil#wrapModeDefault = 'soft'

"
" plugin: goyo
"
let g:goyo_width = '80'
let g:goyo_height = '100%'
"" via goyo.vim's wiki
function! s:goyo_enter()
  let b:quitting = 0
  let b:quitting_bang = 0
  autocmd QuitPre <buffer> let b:quitting = 1
  cabbrev <buffer> q! let b:quitting_bang = 1 <bar> q!
endfunction
"" also via goyo.vim's wiki
function! s:goyo_leave()
  " Quit Vim if this is the only remaining buffer
  if b:quitting && len(filter(range(1, bufnr('$')), 'buflisted(v:val)')) == 1
    if b:quitting_bang
      qa!
    else
      qa
    endif
  endif
endfunction
"" link the two above commands
autocmd! User GoyoEnter call <SID>goyo_enter()
autocmd! User GoyoLeave call <SID>goyo_leave()
