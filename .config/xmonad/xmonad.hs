--Base Imports

import XMonad


import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Util.SpawnOnce
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks

import XMonad.Layout.Spacing

import XMonad.Actions.CycleWS

import XMonad.Util.NamedScratchpad

-- Autostart
myStartupHook :: X ()
myStartupHook = do -- | TRIED REMOVING @exec@ FROM THE FOLLOWING COMMANDS, MODIFIED TRAYER'S COMMAND SLIGHTLY; SEE IF THIS WORKS...
    spawnOnce "picom --experimental-backends &"
    spawnOnce "/usr/bin/emacs --daemon &"
    spawnOnce "~/.fehbg"
    -- spawn "sleep 1 && trayer -l --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 5 --tint 0x292d3e --height 22 &"
    spawnOnce "nm-applet"
    -- spawnOnce "syncthing &"
    spawnOnce "mpd"
    spawnOnce "easyeffects --gapplication-service"
    setWMName "LG3D"

-- Variables
myTerminal, myBrowser, myExplorer :: String
myTerminal = "kitty" :: String
myBrowser = "brave" :: String
myExplorer = "pcmanfm" :: String



-- Key Binds

main :: IO ()
main = xmonad $ ewmhFullscreen $ ewmh $ myConfig
  { layoutHook = spacingWithEdge 5 $ Tall 1 (3/100) (1/2) ||| Full  -- leave gaps at the top and right
}

myConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig = def
    { modMask    = mod4Mask  -- Rebind Mod to the Super key
    , startupHook = myStartupHook
    , manageHook = manageHook def <+> manageDocks
    , focusedBorderColor = "#f8f8f2"
    , normalBorderColor = "#282A36"
    , borderWidth = 3
    , workspaces = ["1","2","3","4","5","6","7","8","9"] 
    }
  `removeKeysP`
    [("M-p")
    ,("M-<Space>")
    ]
  `additionalKeysP`
    [("M-<Space>", spawn "dmenu_run -c -l 20"	)
    ,("M-v"  , spawn myBrowser                  ) -- Launches Web Browser
    ,("M-e"  , spawn myExplorer                 ) -- Launches File Explorer
    ,("M-<Return>"  , spawn myTerminal          ) -- Lauches Terminal
    ,("M-'"  , spawn "emacsclient -c"           )
    ,("M-w"  , kill			        )
    ,("M-S-h"  , prevWS		        )
    ,("M-S-l"  , nextWS		        )
    ]

-- myScratchPads = [ NS "music" "st -n music -g 100x30 -e ncmpcpp" (title =? "music")
--                 ]
