return{

	"folke/which-key.nvim",

	{"dracula/vim", 
	name = "dracula",
	priority = 1000,
	config = function()
		vim.cmd("colo dracula")
	end
	},

    {
        "numToStr/Comment.nvim",
        config = function()
            require("Comment").setup()
        end
    },

    {
        'nvim-lualine/lualine.nvim',
        dependencies = {
            "nvim-tree/nvim-web-devicons"
        },
        config = function()
            require("lualine").setup({
                icons_enabled = true,
            })
        end,
    }
}
