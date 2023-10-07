
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
vim.cmd[[ iabbrev todo • ]]

----Key-Bindings----------------------------------------
vim.g.mapleader = " "
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

--LAZY-------------------------------------------------
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
      
})
