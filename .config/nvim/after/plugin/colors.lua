function Coloring()
	color = color or "gruvbox"
	vim.cmd.colo(color)

	vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
	vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
end
Coloring()
