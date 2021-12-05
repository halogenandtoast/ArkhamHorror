augroup tags
au BufWritePost *.hs            silent !init-tags %
au BufWritePost *.hsc           silent !init-tags %
augroup END

let g:ale_haskell_ormolu_executable='fourmolu'

let g:ale_fixers = {
  \ 'haskell': ['ormolu', 'hlint', 'stylish-haskell'],
  \ 'javascript': ['eslint', 'prettier'],
  \ 'ruby': ['rubocop'],
  \ 'sh': ['shfmt'],
  \ }

let g:ale_linters = {
  \ 'haskell': ['hlint'],
  \ 'ruby': ['rubocop'],
  \ 'javascript': []
  \ }

let g:ale_fix_on_save = 1
let g:ale_haskell_brittany_executable='stack'
let g:ale_haskell_hlint_executable='stack'
let g:ale_haskell_stylish_haskell_executable='stack'
let g:ale_sh_shfmt_options = '-i 2 -ci'
