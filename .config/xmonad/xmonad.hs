import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

import XMonad.Util.Run
import XMonad.Util.Hacks
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad

import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns

import Data.Maybe (fromJust, isJust)
import XMonad.Actions.CycleWS
import XMonad.StackSet as W

myStartupHook :: X ()
myStartupHook = do 
    spawnOnce "picom &"
    spawnOnce "xset r rate 160 35"
    -- spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --margin 550 &" 
    spawnOnce "/usr/bin/emacs --daemon &"
    spawnOnce "dunst"
    -- spawnOnce "eww daemon"
    -- spawnOnce "eww open xmonad-bar"
    spawnOnce "nm-applet --indicator"
    spawnOnce "xclip &"
    spawnOnce "~/.fehbg"
    spawnOnce "xrdb .Xresources"
    spawnOnce "syncthing &"
    spawnOnce "mpDris2"
    spawnOnce "corectrl &"
    setWMName "XMonad"

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh =<< xmobar myConfig 
-- main = xmonad . ewmhFullscreen . ewmh . xmobarProp $ myConfig 
myConfig = def
    { modMask    = mod4Mask      -- Rebind Mod to the Super key
    , layoutHook = myLayout      -- Use custom layouts
    , startupHook = myStartupHook
    , manageHook = myManageHook  -- Match on certain windows
    , focusedBorderColor = "#ebdbb2"
    , normalBorderColor = "#282828"
    , borderWidth = 2 
    }`additionalKeysP` myKeymap

myTerminal, myBrowser, myExplorer :: String
myTerminal = "alacritty" :: String
myBrowser = "firefox" :: String
myExplorer = "pcmanfm" :: String

myLayout = tiled ||| ngtiled ||| monocle ||| fullsc
  where
    tiled = spacingWithEdge 5 (Tall 1 (3/100) (1/2))
    ngtiled = spacingWithEdge 0 (Tall 1 (3/100) (1/2))
    monocle = spacingWithEdge 5 (Full)
    fullsc = spacingWithEdge 0 (avoidStruts(smartBorders(Full)))

-- myWorkspaces = ["dev","term","www","gam","cht","6","7","8","9"]
myWorkspaces = [" dev ", " code ", " www ", " game ", " chat ", " vid ", " wrk "]

myManageHook = manageDocks <+> composeAll
    [ isFullscreen --> doFullFloat
    , manageDocks
    , namedScratchpadManageHook scratchpads
    , className =? "mpv" --> doFloat
    ]

scratchpads = [ NS "pulsemixer" "alacritty --class pulsemixer -e pulsemixer" (className =? "pulsemixer") centerFloating
              , NS "btop" "alacritty --class btop -e btop" (className =? "btop") centerFloating
              , NS "music" "alacritty --class music -e kew" (className =? "music") centerOther
              , NS "fum" "alacritty --class fum -e fum" (className =? "fum") centerSmall
              , NS "term" "alacritty --class term" (className =? "term") centerFloating
              , NS "scratch" "alacritty --class scratch" (className =? "scratch") centerFloating
              ]where

    centerFloating = customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2)
    centerOther = customFloating $ W.RationalRect 0.3 0.25 0.4 0.65
    centerSmall = customFloating $ W.RationalRect 0.37 0.45 0.25 0.20

nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))

myKeymap =
    [("M-<Space>", spawn "dmenu_run -c -l 15") --Launches DMenu a suckless application launcher
    -- [("M-<Space>", spawn "rofi -show run") -- Launches Rofi application launcher
    ,("M-S-<Space>"  , sendMessage NextLayout) -- Cycles through layouts
    ,("M-S-<Backspace>"  , spawn "slock") -- Launches slock the suckless lock screen
    ,("M-q"  , spawn "passmenu -c -l 20 -p pass") -- Launches pass menu, a built in dmenu wrapper for the pass gpg password manager
    ,("M-S-e"  , spawn "xmonad --recompile && xmonad --restart") -- Restart Xmonad
    ,("M-v"  , spawn myBrowser) -- Launches Web Browser
    ,("M-e"  , spawn myExplorer) -- Launches File Explorer
    ,("M-<Return>"  , spawn myTerminal) -- Lauches Terminal
    ,("M-'"  , spawn "emacsclient -c") -- Launches Emacs Client
    ,("M-w"  , kill) -- Kills Window
    ,("M-h"  , sendMessage Shrink) -- Makes window smaller
    ,("M-l"  , sendMessage Expand) -- Makes it Bigger
    ,("M-S-h"  , moveTo Prev nonNSP) -- Move to previous workspace (ie from 2 to 1)
    ,("M-S-l"  , moveTo Next nonNSP) -- Move to next workspace (ie from 1 to 2)
    ,("M-j"  , windows W.focusDown) -- change window focus
    ,("M-k"  , windows W.focusUp) -- same thing different direction
    ,("M-S-j"  , windows W.swapDown) -- move window in layout/stack
    ,("M-S-k"  , windows W.swapUp) -- move in the other direction

    ,("M-p"  , namedScratchpadAction scratchpads "pulsemixer") -- Launches scratchpad of pulsemixer to make quick and easy audio changes
    ,("M-n"  , namedScratchpadAction scratchpads "music") -- Launches scratchpad of pulsemixer to make quick and easy audio changes
    ,("M-m"  , namedScratchpadAction scratchpads "fum") -- Launches scratchpad of pulsemixer to make quick and easy audio changes
    ,("M-g"  , namedScratchpadAction scratchpads "btop") -- Launches scratchpad of btop to quickly see whats happening and kill processess
    ,("M-i"  , namedScratchpadAction scratchpads "scratch") -- Launches scratchpad of an empty terminal to do quick stuff
    ,("M-o"  , namedScratchpadAction scratchpads "term") -- Launches scratchpad of an empty terminal to do quick stuff
    
    ,("<XF86AudioPlay>",  spawn "playerctl -p kew,fooyin,DeaDBeeF,Feishin play-pause") -- toggle play/pause mpd
    ,("<XF86AudioPrev>",  spawn "playerctl -p kew,fooyin,DeaDBeeF,Feishin previous") -- skip to previous song mpd
    ,("<XF86AudioNext>",  spawn "playerctl -p kew,fooyin,DeaDBeeF,Feishin next") -- skip to next song mpd

    ,("<XF86AudioMute>",  spawn "pamixer -t && getvol") -- toggle mute
    ,("<XF86AudioLowerVolume>", spawn "pamixer -d 5 && getvol") -- decrease volume by 5%
    ,("<XF86AudioRaiseVolume>", spawn "pamixer -i 5 && getvol") -- increase volume by 5%

    ,("M-s",  spawn "scr select") -- screenshot selection with scrot script
    ,("M-S-s",  spawn "scr") -- screenshot of whole screen with scrot script
    ]
