abbrevs = {
  you='✥',
  stop='⊘',
  push='↦',
  win='✓',
  sink='≉',
  defeat='⩍',
  hot='⌇',
  melt='⌢',
  move='→',
  shut='⨶',
  open='⧜',
  float='⚲',
  weak='_',
  tele='*',
  pull='↣',
  shift='^',
  swap='↔',

  up='⇧',
  down='⇩',
  right='⇨',
  left='⇦',

  is='=',
  ["not"]='¬',
  ["and"]='&',
  has='~',
  text='@',
  empty='?',
}
vim.api.nvim_create_autocmd({'BufNewFile', 'BufRead'}, {
  pattern = {'*.txt'},
  callback = function()
    for k,v in pairs(abbrevs) do
      vim.cmd('iabbr <buffer> ' .. k .. ' ' .. v)
    end
  end
})
vim.cmd('doautocmd BufRead')
