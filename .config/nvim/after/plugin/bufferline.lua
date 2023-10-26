vim.opt.termguicolors = true
require("bufferline").setup{}

vim.keymap.set("n", "<A-j>", '<CMD>BufferLineCycleNext<CR>')
vim.keymap.set("n", "<A-k>", '<CMD>BufferLineCyclePrev<CR>')
vim.keymap.set("n", "<A-w>", ':bw<CR>')
