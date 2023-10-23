return {

	{"dracula/vim", 
	name = "dracula",
	priority = 1000,
	config = function()
		vim.cmd("colo dracula")
	end},


	{"numToStr/Comment.nvim",
	config = function()
		require("Comment").setup()
	end},


	-- Lualine
	{"nvim-lualine/lualine.nvim",
	dependencies = "nvim-tree/nvim-web-devicons",
	config = function()
		require("lualine").setup({
			icons_enabled = true,
		})
	end},



	"folke/which-key.nvim",
	{'nvim-telescope/telescope.nvim', tag = '0.1.4',
	dependencies = { 'nvim-lua/plenary.nvim' }},


	{"nvim-treesitter/nvim-treesitter",
	build = ":TSUpdate"},

	"nvim-treesitter/playground",
	"theprimeagen/harpoon",
	"mbbill/undotree",
	"tpope/vim-fugitive",
	{'VonHeikemen/lsp-zero.nvim', branch = 'v3.x'},
	{'neovim/nvim-lspconfig'},
	{'hrsh7th/cmp-nvim-lsp'},
	{'hrsh7th/nvim-cmp'},
	{'L3MON4D3/LuaSnip'},
}
