vim.api.nvim_create_autocmd({'BufNewFile', 'BufRead'}, {
  pattern = {'*.txt'},
  callback = function()
    vim.cmd('iabbr <buffer> stop ⊘')
    vim.b.unfo_ftplugin = 'unabbr stop'
  end
})
vim.cmd('doautocmd BufRead')
