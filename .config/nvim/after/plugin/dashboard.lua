local alpha = require("alpha")
local dashboard = require("alpha.themes.dashboard")

dashboard.section.header.val = {

[[      ________________________________           ]],
[[     /                                "-_        ]],
[[    /      .  |  .                       \       ]],
[[   /      : \ | / :                       \      ]],
[[  /        '-___-'                         \     ]],
[[ /_________________________________________ \    ]],
[[      _______| |________________________--""-L   ]],
[[     /       F J                              \  ]],
[[    /       F   J                              L ]],
[[   /      :'     ':                            F ]],
[[  /        '-___-'                            /  ]],
[[ /_________________________________________--"   ]],
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
