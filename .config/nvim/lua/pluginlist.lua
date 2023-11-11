return {

	{"dracula/vim", 
	name = "dracula",
	priority = 1000,
	},
	"morhetz/gruvbox",

	"masroof-maindak/swamp.nvim",

	{"numToStr/Comment.nvim",
	config = function()
		require("Comment").setup()
	end},


	{"nvim-lualine/lualine.nvim",
	dependencies = "nvim-tree/nvim-web-devicons",
	},
	"folke/which-key.nvim",
	{'nvim-telescope/telescope.nvim', tag = '0.1.4',
	dependencies = { 'nvim-lua/plenary.nvim' }},


	{"nvim-treesitter/nvim-treesitter",
	build = ":TSUpdate"},

	"nvim-treesitter/playground",
	"theprimeagen/harpoon",
	"mbbill/undotree",
	"tpope/vim-fugitive",

	{"hrsh7th/nvim-cmp",
	event = "InsertEnter",
	},
	"hrsh7th/cmp-buffer",
	"hrsh7th/cmp-path",
	"hrsh7th/cmp-nvim-lsp",
	"hrsh7th/cmp-cmdline",
	"L3MON4D3/LuaSnip",
	"saadparwaiz1/cmp_luasnip",
	"rafamadriz/friendly-snippets",
	"neovim/nvim-lspconfig",
	"williamboman/mason.nvim",
	"williamboman/mason-lspconfig.nvim",
	"neovim/nvim-lspconfig",
	{'goolord/alpha-nvim',
	dependencies = { 'nvim-tree/nvim-web-devicons' },
	},
	{'akinsho/bufferline.nvim', 
	version = "*", 
	dependencies = 'nvim-tree/nvim-web-devicons'},

	{"nvim-tree/nvim-tree.lua",
	dependencies = 'nvim-tree/nvim-web-devicons'
	},

	{"windwp/nvim-autopairs",
	event = "InsertEnter",
	opts = {}
	},
}
