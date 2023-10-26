require("mason").setup({
    ui = {
        icons = {
            package_installed = "✓",
            package_pending = "➜",
            package_uninstalled = "✗"
        }
    }
})

require("mason-lspconfig").setup{
	ensure_installed = {
		"html",
		"cssls",
		"lua_ls",
		"pyright",
	},
	automatic_installation = true,
}
