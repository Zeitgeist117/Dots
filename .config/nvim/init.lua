----     _   _ _____ _____     _____ __  __
----    | \ | | ____/ _ \ \   / /_ _|  \/  |
----    |  \| |  _|| | | \ \ / / | || |\/| |
----    | |\  | |__| |_| |\ V /  | || |  | |
----    |_| \_|_____\___/  \_/  |___|_|  |_|

----Basic-Options--------------------------------------
vim.o.termguicolors = true
vim.o.number = true
vim.o.relativenumber = true
vim.o.autoindent = true
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.softtabstop = 4
vim.o.mouse='a'
vim.opt.clipboard = 'unnamedplus'
vim.o.syntax = true
vim.o.hidden = true
vim.o.ve = all
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.scrolloff = 8
vim.cmd("filetype plugin on")
vim.cmd("colo dracula")

----Key-Bindings----------------------------------------
vim.g.mapleader = " "
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")


----Packer---------------------------------------------
vim.cmd [[packadd packer.nvim]]

require('packer').startup(function(use)
	use 'wbthomason/packer.nvim'

	use 'Mofiqul/dracula.nvim'
	use {
		"FeiyouG/command_center.nvim",
		requires = { "nvim-telescope/telescope.nvim" }
	}
	use {'nvim-telescope/telescope.nvim', tag = '0.1.0', requires = { {'nvim-lua/plenary.nvim'} } }
	use('nvim-treesitter/nvim-treesitter', {run = ':TSUpdate'})
	use('nvim-treesitter/playground')
	use('ThePrimeagen/harpoon')
	use('mbbill/undotree')
	use('tpope/vim-fugitive')
	use {
		'VonHeikemen/lsp-zero.nvim',
		branch = 'v1.x',
		requires = {
			-- LSP Support
			{'neovim/nvim-lspconfig'},             -- Required
			{'williamboman/mason.nvim'},           -- Optional
			{'williamboman/mason-lspconfig.nvim'}, -- Optional

			-- Autocompletion
			{'hrsh7th/nvim-cmp'},         -- Required
			{'hrsh7th/cmp-nvim-lsp'},     -- Required
			{'hrsh7th/cmp-buffer'},       -- Optional
			{'hrsh7th/cmp-path'},         -- Optional
			{'saadparwaiz1/cmp_luasnip'}, -- Optional
			{'hrsh7th/cmp-nvim-lua'},     -- Optional

			-- Snippets
			{'L3MON4D3/LuaSnip'},             -- Required
			{'rafamadriz/friendly-snippets'}, -- Optional
		}
	}
	use {
		"folke/which-key.nvim",
		config = function()
			require("which-key").setup {
				-- your configuration comes here
				-- or leave it empty to use the default settings
				-- refer to the configuration section below
			}
		end
	}
	use {
		'goolord/alpha-nvim',
		config = function ()
			-- require'alpha'.setup(require'alpha.themes.dashboard'.config)
		end
	}
	use 'windwp/windline.nvim'
	use {'akinsho/bufferline.nvim', tag = "v3.*", requires = 'nvim-tree/nvim-web-devicons'}
	use 'junegunn/goyo.vim'
	use 'junegunn/limelight.vim'
	use 'preservim/vim-pencil'
	use 'preservim/vim-markdown'
	use {
		'vimwiki/vimwiki',
		config = function()
			vim.g.vimwiki_list = {
				{
					path = '/home/nightwing/Notes/wiki/',
					syntax = 'markdown',
					ext = '.md',
				}
			}
		end
	}
	-- use 'preservim/vim-markdown'
	use 'jiangmiao/auto-pairs'
	use 'tpope/vim-commentary'
	use 'habamax/vim-godot'
	use {'wfxr/minimap.vim', as = 'minimap'}
	use 'nvim-tree/nvim-tree.lua'
	use 'nvim-tree/nvim-web-devicons'

end)

----Dashboard-------------------------------------------

local alpha = require("alpha")
local dashboard = require("alpha.themes.dashboard")

dashboard.section.header.val = {
	 [[                               ]],
     [[⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⣠⣤⣤⣴⣦⣤⣤⣄⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ ]],
     [[⠀⠀⠀⠀⠀⠀⢀⣤⣾⣿⣿⣿⣿⠿⠿⠿⠿⣿⣿⣿⣿⣶⣤⡀⠀⠀⠀⠀⠀⠀ ]],
     [[⠀⠀⠀⠀⣠⣾⣿⣿⡿⠛⠉⠀⠀⠀⠀⠀⠀⠀⠀⠉⠛⢿⣿⣿⣶⡀⠀⠀⠀⠀ ]],
     [[⠀⠀⠀⣴⣿⣿⠟⠁⠀⠀⠀⣶⣶⣶⣶⡆⠀⠀⠀⠀⠀⠀⠈⠻⣿⣿⣦⠀⠀⠀ ]],
     [[⠀⠀⣼⣿⣿⠋⠀⠀⠀⠀⠀⠛⠛⢻⣿⣿⡀⠀⠀⠀⠀⠀⠀⠀⠙⣿⣿⣧⠀⠀ ]],
     [[⠀⢸⣿⣿⠃⠀⠀⠀⠀⠀⠀⠀⠀⢀⣿⣿⣷⠀⠀⠀⠀⠀⠀⠀⠀⠸⣿⣿⡇⠀ ]],
     [[⠀⣿⣿⡿⠀⠀⠀⠀⠀⠀⠀⠀⢀⣾⣿⣿⣿⣇⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⠀ ]],
     [[⠀⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⢠⣿⣿⡟⢹⣿⣿⡆⠀⠀⠀⠀⠀⠀⠀⣹⣿⣿⠀ ]],
     [[⠀⣿⣿⣷⠀⠀⠀⠀⠀⠀⣰⣿⣿⠏⠀⠀⢻⣿⣿⡄⠀⠀⠀⠀⠀⠀⣿⣿⡿⠀ ]],
     [[⠀⢸⣿⣿⡆⠀⠀⠀⠀⣴⣿⡿⠃⠀⠀⠀⠈⢿⣿⣷⣤⣤⡆⠀⠀⣰⣿⣿⠇⠀ ]],
     [[⠀⠀⢻⣿⣿⣄⠀⠀⠾⠿⠿⠁⠀⠀⠀⠀⠀⠘⣿⣿⡿⠿⠛⠀⣰⣿⣿⡟⠀⠀ ]],
     [[⠀⠀⠀⠻⣿⣿⣧⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣾⣿⣿⠏⠀⠀⠀ ]],
     [[⠀⠀⠀⠀⠈⠻⣿⣿⣷⣤⣄⡀⠀⠀⠀⠀⠀⠀⢀⣠⣴⣾⣿⣿⠟⠁⠀⠀⠀⠀ ]],
     [[⠀⠀⠀⠀⠀⠀⠈⠛⠿⣿⣿⣿⣿⣿⣶⣶⣿⣿⣿⣿⣿⠿⠋⠁⠀⠀⠀⠀⠀⠀ ]],
     [[⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠉⠛⠛⠛⠛⠛⠛⠉⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ ]],
}
dashboard.section.header.opts.hl = "Title"
dashboard.section.footer.val = require('alpha.fortune')
require'alpha.themes.dashboard'.section.footer.val = require'alpha.fortune'()
dashboard.section.buttons.val = {
	dashboard.button( "e", "  New file" , ":ene <BAR> start <CR>"),
	dashboard.button("SPC p f", "  Find file", ":Telescope find_files hidden=true no_ignore=true<CR>"),
	dashboard.button("SPC f h", "  Recently opened files", "<cmd>Telescope oldfiles<CR>"),
	dashboard.button( "q", "  Quit" , ":qa<CR>"),
}
alpha.setup(dashboard.opts)

----Fugitive--------------------------------------------
vim.keymap.set("n", "<leader>gs", vim.cmd.Git);

----Goyo------------------------------------------------
vim.keymap.set("n", "<leader>g", '<CMD>Goyo<CR>')
vim.keymap.set("n", "<leader>l", '<CMD>Limelight<CR>')
vim.keymap.set("n", "<leader>o", '<CMD>Limelight!<CR>')
vim.g.goyo_width = 200

----Harpoon---------------------------------------------
local mark = require("harpoon.mark")
local ui = require("harpoon.ui")

vim.keymap.set("n", "<leader>a", mark.add_file)
vim.keymap.set("n", "<C-e>", ui.toggle_quick_menu)

vim.keymap.set("n", "<C-h>", function () ui.nav_file(1) end)
vim.keymap.set("n", "<C-t>", function () ui.nav_file(2) end)
vim.keymap.set("n", "<C-n>", function () ui.nav_file(3) end)
vim.keymap.set("n", "<C-s>", function () ui.nav_file(4) end)


----Limelight-------------------------------------------
vim.g.limelight_conceal = 'gray'
vim.g.limelight_conceal_guifg = '#777777'
vim.g.limelight_default_coefficient = 1



----Status-Bar------------------------------------------
require('wlsample.bubble')


----Buffer-Line-----------------------------------------
vim.opt.termguicolors = true
require("bufferline").setup{}

vim.keymap.set("n", "<A-j>", '<CMD>BufferLineCycleNext<CR>')
vim.keymap.set("n", "<A-k>", '<CMD>BufferLineCyclePrev<CR>')
vim.keymap.set("n", "<A-w>", ':bw<CR>')



----Telescope-------------------------------------------
local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>pf', builtin.find_files, {})
vim.keymap.set('n', '<C-g>', builtin.git_files, {})
vim.keymap.set('n', '<leader>ps', function()
	builtin.grep_string({ search = vim.fn.input("Grep > ") });
end)

----Treesitter------------------------------------------

require'nvim-treesitter.configs'.setup {
  -- A list of parser names, or "all" (the five listed parsers should always be installed)
  ensure_installed = { "typescript", "python", "javascript", "c", "lua", "vim", "help", "query" },

  -- Install parsers synchronously (only applied to `ensure_installed`)
  sync_install = false,

  -- Automatically install missing parsers when entering buffer
  -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
  auto_install = true,

  highlight = {
    enable = true,

  },
}

----Undotree--------------------------------------------
vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle)


----Vim-Wiki--------------------------------------------
vim.g.vim_markdown_folding_disabled = 1
vim.g.markdown_syntax_conceal = 0
vim.g.vimwiki_filetypes = {'markdown.pandoc'}
vim.g.vimwiki_global_ext = 0

vim.g.markdown_syntax_conceal = 0
-- vim.api.nvim_create_autocmd(
--     { "BufRead", "BufNewFile" },
--     { pattern = {"*.md", "*.wiki" }, command = "set filetype=markdown" }
-- )

vim.api.nvim_create_autocmd(
    { "BufRead", "BufNewFile" },
    { pattern = {"*.md", "*.wiki" }, 
		command = "set spell",
		command = "Pencil",
		command = "MinimapClose"

	}
)
-- vim.api.nvim_create_autocmd(
--     { "BufRead", "BufNewFile" },
--     { pattern = {"*.md", "*.wiki" }, command = "Pencil" }
-- )
-- vim.api.nvim_create_autocmd(
--     { "BufRead", "BufNewFile" },
--     { pattern = {"*.md", "*.wiki" }, command = "MinimapClose" }
-- )

-- vim.api.nvim_create_autocmd(
--     { "BufRead", "BufNewFile" },
--     { pattern = {"*.md", "*.wiki" }, command = "GoyoEnter" }
-- )

-- vim.api.nvim_create_autocmd('User', {
--   pattern = 'GoyoEnter nested call',
--   desc = 'Settings md',
--   callback = function(event)
-- 	  vim.cmd('set filetype=markdown')
-- 	  vim.cmd('Pencil')

--   end
-- })

-- vim.api.nvim_create_autocmd('User', {
--   pattern = 'GoyoLeave nested call',
--   desc = 'Restore settings',
--   callback = function(event)
-- 	  vim.cmd('set filetype=markdown')
-- 	  vim.cmd('PencilOff')
--   end
-- })

----LSP---------------------------------------------

local lsp = require('lsp-zero')

lsp.preset('recommended')

lsp.ensure_installed({
	'lua_ls',
	'bashls',
	'pyright',
	'dhall_lsp_server',
	'ltex',
	'rust_analyzer',
	'clangd',
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

--------------------------------------------------------------------

-- vim.g.minimap_auto_start = 1
-- vim.g.minimap_width = 10
-- vim.g.minimap_auto_start_win_enter = 1
-- vim.g.minimap_left = 0
-- vim.g.minimap_block_filetypes = {'fugitive', 'nvim-tree', 'tagbar', 'fzf', 'telescope', 'NvimTree', 'markdown', 'vimwiki'}
-- vim.g.minimap_block_buftypes = {'nofile', 'nowrite', 'quickfix', 'terminal', 'prompt', 'NvimTree', 'markdown', 'vimwiki'}
-- vim.g.minimap_close_filetypes = {'startify', 'alpha', 'netrw', 'packer', 'NvimTree', 'markdown'}
-- vim.g.minimap_highlight_range = 1
-- vim.g.minimap_git_colors = 1
-- vim.g.minimap_highlight_search = 1
--------------------------------------------------------------------

vim.keymap.set("n", "<leader>e", ':NvimTreeToggle<CR>')
vim.g.loaded = 1
vim.g.loaded_netrwPlugin = 1
require("nvim-tree").setup({

})

