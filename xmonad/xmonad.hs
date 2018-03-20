-- Imports.
import Data.Monoid
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig(additionalKeys)

-- The main function.
main = xmonad =<< statusBar myStatusBar myPP toggleStrutsKey myConfig

myModMask           = mod4Mask
myTerminal          = "alacritty"
myAltTerminal       = "xterm"
myBrowser           = "google-chrome"
myStatusBar         = "xmobar"
myLauncher          = "rofi -show run"
--myLauncher          = "rofi -matching fuzzy -show run -lines 5 -eh 2 -width 100 -padding 500 -opacity \"80\" -bw 0 -bc \"#1f222d\" -bg \"#252936\" -fg \"#ffffff\" -hlbg \"#d1d4e0\" -hlfg \"#9575cd\" -font \"Source Code Pro 18\" -show combi -combi-modi run,drun"


-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main configuration, override the defaults to your liking.
myConfig = defaultConfig
        { modMask = myModMask -- Use Super instead of Alt
        , terminal = myTerminal
        }
	`additionalKeys` myAdditionalKeys

myAdditionalKeys = [
		((mod4Mask .|. shiftMask, xK_p), spawn myLauncher)
	,	((mod4Mask .|. shiftMask, xK_z), spawn myAltTerminal)
	]

