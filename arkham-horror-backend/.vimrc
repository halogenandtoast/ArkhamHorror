augroup tags
au BufWritePost *.hs            silent !init-tags %
au BufWritePost *.hsc           silent !init-tags %
augroup END
