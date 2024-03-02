import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
--import XMonad.Util.UnGrab
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
    -- spawnOnce "xrandr --output 'eDP-1' --off"
    spawnOnce "compfy &"
    spawnOnce "xset r rate 160 35"
    spawnOnce "/usr/bin/emacs --daemon &"
    spawnOnce "xclip &"
    spawnOnce "~/.fehbg"
    spawnOnce "xrdb .Xresources"
    spawnOnce "eww --daemon"
    spawnOnce "eww open bar"
    spawnOnce "syncthing &"
    spawnOnce "mpDris2"
    spawnOnce "mpd"
    -- spawnOnce "easyeffects --gapplication-service"

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . xmobarProp $ myConfig 
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
myTerminal = "st" :: String
myBrowser = "firefox" :: String
myExplorer = "thunar" :: String

myLayout = tiled ||| ngtiled ||| monocle ||| fullsc
  where
	monocle = spacingWithEdge 5 (Full)
	tiled = spacingWithEdge 5 (Tall 1 (3/100) (1/2))
	ngtiled = spacingWithEdge 0 (smartBorders(Tall 1 (3/100) (1/2)))
	fullsc = spacingWithEdge 0 (avoidStruts(smartBorders(Full)))

-- myWorkspaces = ["dev","term","www","gam","cht","6","7","8","9"]
myWorkspaces = [" dev ", " code ", " www ", " game ", " chat ", " vid ", " wrk "]

myManageHook = manageDocks <+> composeAll
    [ isFullscreen --> doFullFloat
    , manageDocks
    , namedScratchpadManageHook scratchpads
    , className =? "mpv" --> doFloat
    ]

scratchpads :: [NamedScratchpad]
scratchpads = [ NS "ncmpcpp" "st -c ncmpcpp -T 'ncmpcpp' -e ncmpcpp" (className =? "ncmpcpp") centerFloating
              , NS "pulsemixer" "st -c pulsemixer -T pulsemixer -e pulsemixer" (className =? "pulsemixer") centerFloating
              , NS "btop" "st -c btop -T btop -e btop" (className =? "btop") centerFloating
              , NS "scratch" "st -c scratch -T scratch" (className =? "scratch") centerFloating
              , NS "term" "st -c term -T term" (className =? "term") centerFloating
              ]where
    centerFloating = customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2)

nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))

myKeymap =
    [("M-<Space>", spawn "dmenu_run -c -l 15"                 ) --Launches DMenu a suckless application launcher
    -- [("M-<Space>", spawn "rofi -show run"                     ) -- Launches Rofi application launcher
    ,("M-S-<Space>"  , sendMessage NextLayout                 ) -- Cycles through layouts
    ,("M-S-<Backspace>"  , spawn "slock"                      ) -- Launches slock the suckless lock screen
    ,("M-S-p"  , spawn "passmenu -c -l 20 -p pass"	          ) -- Launches pass menu, a built in dmenu wrapper for the pass gpg password manager
    ,("M-q"  , spawn "xmonad --recompile && xmonad --restart" ) -- Restart Xmonad
    ,("M-v"  , spawn myBrowser                                ) -- Launches Web Browser
    ,("M-e"  , spawn myExplorer                               ) -- Launches File Explorer
    ,("M-<Return>"  , spawn myTerminal                        ) -- Lauches Terminal
    ,("M-'"  , spawn "emacsclient -c"                         ) -- Launches Emacs Client
    ,("M-w"  , kill			                                  ) -- Kills Window
    ,("M-h"  , sendMessage Shrink		                      ) -- Makes window smaller
    ,("M-l"  , sendMessage Expand		                      ) -- Makes it Bigger
    ,("M-S-h"  , moveTo Prev nonNSP                           ) -- Move to previous workspace (ie from 2 to 1)
    ,("M-S-l"  , moveTo Next nonNSP                           ) -- Move to next workspace (ie from 1 to 2)
    ,("M-j"  , windows W.focusDown		                      ) -- change window focus
    ,("M-k"  , windows W.focusUp		                      ) -- same thing different direction
    ,("M-S-j"  , windows W.swapDown		                      ) -- move window in layout/stack
    ,("M-S-k"  , windows W.swapUp		                      ) -- move in the other direction
    ,("M-n"  , namedScratchpadAction scratchpads "ncmpcpp"    ) -- Launches a scratchpad of my favourite music player N Curses Music Player Client ++
    ,("M-p"  , namedScratchpadAction scratchpads "pulsemixer" ) -- Launches scratchpad of pulsemixer to make quick and easy audio changes
    ,("M-g"  , namedScratchpadAction scratchpads "btop"       ) -- Launches scratchpad of btop to quickly see whats happening and kill processess
    ,("M-i"  , namedScratchpadAction scratchpads "scratch"    ) -- Launches scratchpad of an empty terminal to do quick stuff
    ,("M-o"  , namedScratchpadAction scratchpads "term"    ) -- Launches scratchpad of an empty terminal to do quick stuff
    ,("<XF86AudioPlay>",  spawn "mpc toggle"                  ) -- toggle play/pause mpd
    ,("<XF86AudioPrev>",  spawn "mpc prev"                    ) -- skip to previous song mpd
    ,("<XF86AudioNext>",  spawn "mpc next"                    ) -- skip to next song mpd
    ,("<XF86AudioMute>",  spawn "pamixer -t && getvol"        ) -- toggle mute
    ,("<XF86AudioLowerVolume>", spawn "pamixer -d 5 && getvol") -- decrease volume by 5%
    ,("<XF86AudioRaiseVolume>", spawn "pamixer -i 5 && getvol") -- increase volume by 5%
    ,("M-s",  spawn "scr select"                              ) -- screenshot selection with scrot script
    ,("M-S-s",  spawn "scr"                                   ) -- screenshot of whole screen with scrot script
    ,("M-y"  , spawn "ywatch"                                 ) -- if my clipboard has a youtube link it is launched in mpv
    ]
