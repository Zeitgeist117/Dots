----     _   _ _____ _____     _____ __  __
----    | \ | | ____/ _ \ \   / /_ _|  \/  |
----    |  \| |  _|| | | \ \ / / | || |\/| |
----    | |\  | |__| |_| |\ V /  | || |  | |
----    |_| \_|_____\___/  \_/  |___|_|  |_|


----Variables-------------------------------------------
local g   = vim.g
local o   = vim.o
local opt = vim.opt
local A   = vim.api

----Basic-Options--------------------------------------

o.termguicolors = true
o.number = true
o.relativenumber = true
o.autoindent = true
o.tabstop = 4
o.shiftwidth = 4
o.softtabstop = 4
o.mouse='a'
opt.clipboard = 'unnamedplus'
o.syntax = true
o.hidden = true
o.ve = all


----Plugins---------------------------------------------
require('packer').startup(function(use)
	use 'wbthomason/packer.nvim'
-- Auto-Complete

	use 'neovim/nvim-lspconfig'
	use 'hrsh7th/cmp-nvim-lsp'
	use 'hrsh7th/cmp-buffer'
	use 'hrsh7th/cmp-path'
	use 'hrsh7th/cmp-cmdline'
	use 'hrsh7th/nvim-cmp'
	use 'hrsh7th/cmp-vsnip'
	use 'hrsh7th/vim-vsnip'

	use 'nvim-treesitter/nvim-treesitter'
	use 'dracula/vim'
	use 'windwp/windline.nvim'
	use {
	  'goolord/alpha-nvim',
	  config = function ()
        -- require'alpha'.setup(require'alpha.themes.dashboard'.config)
	  end
	 }

	-- use {
	--   "folke/which-key.nvim",
	--   config = function()
      -- require("which-key").setup {
      -- -- your configuration comes here
      -- -- or leave it empty to use the default settings
      -- -- refer to the configuration section below
     -- }
	-- end
	-- }

	use 'junegunn/goyo.vim'
	use 'junegunn/fzf.vim'
	use 'junegunn/limelight.vim'
	use 'preservim/vim-pencil'
    use 'vimwiki/vimwiki'
	use 'godlygeek/tabular'
	use 'preservim/vim-markdown'
	use 'preservim/nerdtree'
	use 'ryanoasis/vim-devicons'

	use 'jiangmiao/auto-pairs'
	use {'nvim-telescope/telescope.nvim', tag = '0.1.0', requires = { {'nvim-lua/plenary.nvim'} } }
	use 'nvim-telescope/telescope-project.nvim'
	use "nvim-lua/plenary.nvim"
	use "nvim-telescope/telescope-file-browser.nvim"
	use 'tpope/vim-commentary'
	use 'rrethy/vim-hexokinase'
	use 'vim-pandoc/vim-pandoc'
	use 'vim-pandoc/vim-pandoc-syntax'

end)

require'telescope'.load_extension('project')

g.hexokinase_highlighters = "backgroundfull"


----Key-Bindings----------------------------------------

g.mapleader = " "
g.maplocalleader = " "

local function map(m, k, v)
   vim.keymap.set(m, k, v, { silent = true })
end


-- split
map('n', '<leader>wv', '<CMD>vertical split<CR>')


-- Goyo
map('n', '<leader>g', '<CMD>Goyo<CR>')
map('n', '<leader>l', '<CMD>Limelight<CR>')
map('n', '<leader>o', '<CMD>Limelight!<CR>')

-- nerdtree
map('n', '<leader>i', '<CMD>NERDTreeToggle<CR>')

-- Telescope
map('n', '<leader>sl', '<CMD>SessionLoad<CR>')
map('n', '<leader>fh', '<CMD>Telescope oldfiles<CR>')
map('n', '<leader>ff', '<CMD>Telescope find_files<CR>')
map('n', '<leader>.', '<CMD>Telescope file_browser<CR>')
map('n', '<leader>fw', '<CMD>Telescope live_grep<CR>')

map('n', '<leader>qq', '<CMD>NvimagerToggle<CR>')



----Dashboard-------------------------------------------

local alpha = require("alpha")
local dashboard = require("alpha.themes.dashboard")
local fortune = require("alpha.fortune")

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
dashboard.section.buttons.opts.hl = "Debug"
dashboard.section.footer.opts.hl = "Conceal"
dashboard.config.opts.noautocmd = true
alpha.setup(dashboard.opts)
  
----Colorscheme-----------------------------------------
vim.cmd[[colorscheme dracula]]

----Status-Bar------------------------------------------

require('wlsample.bubble')

----Vim-Wiki--------------------------------------------
g.vimwiki_list = {{
	path = '~/Notes/wiki',
	syntax = 'markdown',
	ext = '.md'
}}

g.vimwiki_filetypes = {'markdown'}
g.vimwiki_global_ext = 0


----Telescope-------------------------------------------
-- You don't need to set any of these options.
-- IMPORTANT!: this is only a showcase of how you can set default options!
require("telescope").setup {
  extensions = {
    file_browser = {
      theme = "ivy",
      -- disables netrw and use telescope-file-browser in its place
      hijack_netrw = true,
      mappings = {
        ["i"] = {
          -- your custom insert mode mappings
        },
        ["n"] = {
          -- your custom normal mode mappings
        },
      },
    },
  },
}
-- To get telescope-file-browser loaded and working with telescope,
-- you need to call load_extension, somewhere after setup function:
require("telescope").load_extension "file_browser"

----LimeLight&Goyo-------------------------------------------

g.limelight_conceal = 'gray'
g.limelight_conceal_guifg = '#777777'
g.limelight_default_coefficient = 1
g.goyo_width = 150

g.vim_markdown_folding_disabled = 1
g.markdown_syntax_conceal = 0


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



----Auto-Complete--------------------------------------------

  -- Set up nvim-cmp.
  local cmp = require'cmp'

  cmp.setup({
    snippet = {
      -- REQUIRED - you must specify a snippet engine
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
        -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
        -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
        -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
      end,
    },
    window = {
      -- completion = cmp.config.window.bordered(),
      -- documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert({
	  ['<Tab>'] = cmp.mapping.select_next_item(),
      ['<C-b>'] = cmp.mapping.scroll_docs(-4),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<C-Space>'] = cmp.mapping.complete(),
      ['<C-e>'] = cmp.mapping.abort(),
      ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    }),
    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      { name = 'vsnip' }, -- For vsnip users.
      -- { name = 'luasnip' }, -- For luasnip users.
      -- { name = 'ultisnips' }, -- For ultisnips users.
      -- { name = 'snippy' }, -- For snippy users.
    }, {
      { name = 'buffer' },
    })
  })

  -- Set configuration for specific filetype.
  cmp.setup.filetype('gitcommit', {
    sources = cmp.config.sources({
      { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
    }, {
      { name = 'buffer' },
    })
  })

  -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline({ '/', '?' }, {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
      { name = 'buffer' }
    }
  })

  -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
      { name = 'path' }
    }, {
      { name = 'cmdline' }
    })
  })

  -- Set up lspconfig.
  local capabilities = require('cmp_nvim_lsp').default_capabilities()
  -- Replace <YOUR_LSP_SERVER> with each lsp server you've enabled.
  -- require('lspconfig')['<YOUR_LSP_SERVER>'].setup {
  --   capabilities = capabilities
  -- }




vim.api.nvim_create_autocmd(
  { "BufRead", "BufNewFile" },
  { pattern = {"*.cfg"}, command = "set filetype=xml" }
)

