vim.g.vim_markdown_folding_disabled = 1
vim.g.markdown_syntax_conceal = 0
vim.g.vimwiki_filetypes = {'markdown.pandoc'}
vim.g.vimwiki_global_ext = 0

vim.api.nvim_create_autocmd(
    { "BufRead", "BufNewFile" },
    { pattern = {"*.md", "*.wiki" }, 
		command = "set spell",
		command = "Pencil",

	}
)
