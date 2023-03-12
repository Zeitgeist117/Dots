
vim.g.vim_markdown_folding_disabled = 1
vim.g.markdown_syntax_conceal = 0
vim.g.vimwiki_list = {{
	path = '~/Notes/wiki',
	syntax = 'markdown',
	ext = '.md'
}}

vim.g.vimwiki_filetypes = {'markdown'}
vim.g.vimwiki_global_ext = 0

vim.api.nvim_create_autocmd(
    { "BufRead", "BufNewFile" },
    { pattern = {"*.md", "*.wiki" }, command = "set filetype=markdown" }
)

vim.api.nvim_create_autocmd(
    { "BufRead", "BufNewFile" },
    { pattern = {"*.md", "*.wiki" }, command = "set spell" }
)

-- vim.api.nvim_create_autocmd(
--     { "BufRead", "BufNewFile" },
--     { pattern = {"*.md", "*.wiki" }, command = "GoyoEnter" }
-- )

vim.api.nvim_create_autocmd('User', {
  pattern = 'GoyoEnter nested call',
  desc = 'Settings md',
  callback = function(event)
	  vim.cmd('set filetype=markdown')
	  vim.cmd('Pencil')

  end
})

vim.api.nvim_create_autocmd('User', {
  pattern = 'GoyoLeave nested call',
  desc = 'Restore settings',
  callback = function(event)
	  vim.cmd('set filetype=markdown')
	  vim.cmd('PencilOff')
  end
})

