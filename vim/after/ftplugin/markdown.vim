"
" SETTINGS
"
set autowriteall                      "don't lose progress in .md files

"
" PLUGINS
"
call pencil#init()                    "vim-pencil is weird
call textobj#sentence#init()          "vim-textobj-sentence is weird
let g:pencil#wrapModeDefault='soft'   "softwrap on markdown files
let g:goyo_width='80'                 "centred 80px text edit view
let g:goyo_height='95%'               "pad out the top/bottom of the screen

"
" EXTRA SYNTAX HIGHLIGHTING
"
let g:markdown_fenced_languages=['html', 'python', 'bash=sh', 'c', 'cpp']
syn region mdString start=+"+ end=+"+ end=/\n\w*\n/
hi def link mdString String
syn region mdComment start="//" skip="\\$" end="$"
hi def link mdComment Comment

"
" AUTOCOMMANDS 
"   make :q work with goyo.vim
"
function! s:goyo_enter()
  let b:quitting = 0
  let b:quitting_bang = 0
  autocmd QuitPre <buffer> let b:quitting = 1
  cabbrev <buffer> q! let b:quitting_bang = 1 <bar> q!
endfunction
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
autocmd! User GoyoEnter call <SID>goyo_enter()
autocmd! User GoyoLeave call <SID>goyo_leave()
