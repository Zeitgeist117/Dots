--Base Imports

import XMonad


import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Util.SpawnOnce
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks

-- Autostart
myStartupHook :: X ()
myStartupHook = do -- | TRIED REMOVING @exec@ FROM THE FOLLOWING COMMANDS, MODIFIED TRAYER'S COMMAND SLIGHTLY; SEE IF THIS WORKS...
    spawnOnce "picom --experimental-backends &"
    spawnOnce "~/.fehbg &"
    spawn "sleep 1 && trayer -l --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 5 --tint 0x292d3e --height 22 &"
    spawnOnce "nm-applet &"
    -- spawnOnce "syncthing &"
    spawnOnce "mpd &"
    setWMName "LG3D"

-- Variables
myTerminal, myBrowser, myExplorer :: String
myTerminal = "kitty" :: String
myBrowser = "brave" :: String
myExplorer = "pcmanfm" :: String

--Layouts
myLayout :: Choose Tall (Choose (Mirror Tall) Full) a
myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes

-- Key Binds

main :: IO ()
main = xmonad . ewmh =<< xmobar myConfig

myConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig = def
    { modMask    = mod4Mask  -- Rebind Mod to the Super key
    , startupHook = myStartupHook
    , layoutHook = myLayout
    , manageHook = manageHook def <+> manageDocks
    , workspaces = ["1","2","3","4","5","6","7","8","9"] 
    }
  `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    ,("M-C-s", unGrab *> spawn "scrot -s"       )
    ,("M-q"  , spawn myBrowser                  ) -- Launches Web Browser
    ,("M-e"  , spawn myExplorer                 ) -- Launches File Explorer
    ,("M-<Return>"  , spawn myTerminal          ) -- Lauches Terminal

    ]
