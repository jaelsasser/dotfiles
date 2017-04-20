"
" AUTOCOMPLETE SETTINGS
" python competion via jedi-vim
autocmd Filetype py set omnifunc=python3complete#Complete"
let g:jedi#force_py_version=3
let deoplete#sources#jedi#show_docstring=1
