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
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.ManageHook
import XMonad.Actions.Submap
import XMonad.Util.NamedActions
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders

myStartupHook :: X ()
myStartupHook = do 
    setWMName "XMonad"
    spawnOnce "picom --experimental-backends &"
    spawnOnce "/usr/bin/emacs --daemon &"
    spawnOnce "xclip &"
    spawnOnce "~/.fehbg"
    spawnOnce "xset r rate 190 35"
    spawnOnce "syncthing &"
    spawnOnce "mpd"
    spawnOnce "easyeffects --gapplication-service"

myTerminal, myBrowser, myExplorer :: String
myTerminal = "kitty" :: String
myBrowser = "firefox" :: String
myExplorer = "pcmanfm" :: String

main :: IO ()
main = xmonad $ myConfig
  { layoutHook = spacingWithEdge 5 (Tall 1 (3/100) (1/2)) ||| Full ||| spacingWithEdge 0 (avoidStruts(smartBorders(Full)))
}

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

-- myManageHook :: ManageHook
myManageHook = manageDocks <+> composeAll
    [ isFullscreen --> doFullFloat
    , manageDocks
    , namedScratchpadManageHook scratchpads
    ]

myConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig = ewmh def
    { modMask    = mod4Mask  -- Rebind Mod to the Super key
    , startupHook = myStartupHook
    , manageHook = myManageHook
    , XMonad.workspaces = myWorkspaces
    , focusedBorderColor = "#f8f8f2"
    , normalBorderColor = "#282A36"
    , borderWidth = 3
    }`additionalKeysP` myKeymap  --calls the keymap without getting rid of the defaults cause i don't wont to reconfigure everything

myKeymap =
    [("M-<Space>", spawn "dmenu_run -c -l 20"                 )
    ,("M-S-<Space>"  , sendMessage NextLayout                 )
    ,("M-q"  , spawn "xmonad --recompile && xmonad --restart" ) -- Restart Xmonad
    ,("M-v"  , spawn myBrowser                                ) -- Launches Web Browser
    ,("M-e"  , spawn myExplorer                               ) -- Launches File Explorer
    ,("M-<Return>"  , spawn myTerminal                        ) -- Lauches Terminal
    ,("M-'"  , spawn "emacsclient -c"                         ) -- Launches Emacs Client
    ,("M-w"  , kill			                                  ) -- Kills Window
    ,("M-h"  , sendMessage Shrink		                      ) -- Makes window smaller
    ,("M-l"  , sendMessage Expand		                      ) -- Makes it Bigger
    ,("M-S-h"  , prevWS		                                  ) -- Move to previous workspace (ie from 2 to 1)
    ,("M-S-l"  , nextWS		                                  ) -- Move to previous workspace (ie from 2 to 1)
    ,("M-j"  , windows W.focusDown		                      ) -- change window focus
    ,("M-k"  , windows W.focusUp		                      ) -- same thing different direction
    ,("M-S-j"  , windows W.swapDown		                      ) -- move window in layout/stack
    ,("M-S-k"  , windows W.swapUp		                      ) -- move in the other direction
    ,("M-n"  , namedScratchpadAction scratchpads "ncmpcpp"    ) -- Launches a scratchpad of my favourite music player N Curses Music Player Client ++
    ,("M-p"  , namedScratchpadAction scratchpads "pulsemixer" ) -- Launches scratchpad of pulsemixer to make quick and easy audio changes
    ,("M-g"  , namedScratchpadAction scratchpads "btop"       ) -- Launches scratchpad of btop to quickly see whats happening and kill processess
    ,("<XF86AudioPlay>",  spawn "mpc toggle"                  ) -- toggle play/pause mpd
    ,("<XF86AudioPrev>",  spawn "mpc prev"                    ) -- skip to previous song mpd
    ,("<XF86AudioNext>",  spawn "mpc next"                    ) -- skip to next song mpd
    ,("<XF86AudioMute>",  spawn "pamixer -t && getvol"        ) -- toggle mute
    ,("<XF86AudioLowerVolume>", spawn "pamixer -d 5 && getvol") -- decrease volume by 5%
    ,("<XF86AudioRaiseVolume>", spawn "pamixer -i 5 && getvol") -- increase volume by 5%
    ,("M-s",  spawn "scr select"                          ) --screenshot selection with scrot script
    ,("M-S-s",  spawn "scr"                               ) --screenshot of whole screen with scrot script
    ,("M-y"  , spawn "ywatch" ) -- Restart Xmonad
    ]

scratchpads :: [NamedScratchpad]
scratchpads = [ NS "ncmpcpp" "st -n ncmpcpp -g 100x30 -e ncmpcpp" (title =? "ncmpcpp") centerFloating
              , NS "pulsemixer" "st -n pulsemixer -g 100x30 -e pulsemixer" (title =? "pulsemixer") centerFloating
              , NS "btop" "st -n btop -g 100x30 -e btop" (title =? "btop") centerFloating
              ]where
    centerFloating = customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2)
