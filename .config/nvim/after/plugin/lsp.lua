local lsp = require('lsp-zero')

lsp.preset('recommended')

lsp.ensure_installed({
	'lua_ls',
	'pyright',
	'dhall_lsp_server',
	'ltex',
	'rust_analyzer',
})
require'lspconfig'.gdscript.setup{}

local cmp = require('cmp')
local cmp_select = {behavior = cmp.SelectBehavior.Select}
local cmp_mappings = lsp.defaults.cmp_mappings({
	['<C-p>'] = cmp.mapping.select_prev_item(cmp.select),
	['<C-n>'] = cmp.mapping.select_next_item(cmp.select),
	['<C-y>'] = cmp.mapping.confirm({ select = true }),
	['<C-Space>'] = cmp.mapping.complete(),
})

lsp.set_preferences({
	sign_icons = {}
})
lsp.setup()
