import System.IO
import Data.Monoid
-- Imports.
import XMonad
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run
import XMonad.Util.Run(spawnPipe)
--
-- not sure if these do anything yet
import XMonad.Layout.PerScreen              -- Check screen width & adjust layouts
import XMonad.Layout.PerWorkspace           -- Configure layouts on a per-workspace 
import XMonad.Util.WorkspaceCompare

-- The main function.
main = do
        xmproc <- spawnPipe myStatusBar

        xmonad 
            $dynamicProjects projects
	    $myConfig xmproc

-- My Config Params
myAltTerminal       = "gnome-terminal"
myBrowser           = "google-chrome"
myLauncher          = "rofi -show run"
myLockScreen        = "i3lock"
myModMask           = mod4Mask
myStartupScript     = "/home/andrewwright/.xmonad/startup.sh"
myStatusBar         = "xmobar"
myTerminal          = "alacritty"

-- Colors
xmobarTitleColor = "#429942"
xmobarCurrentWorkspaceColor = "#429942"

active      = blue
activeWarn  = red
inactive    = base02
focusColor  = blue
unfocusColor = base02

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green       = "#859900"

-- Main configuration, override the defaults to your liking.
myConfig p = def
        { modMask            = myModMask
        , terminal           = myTerminal
        , workspaces         = myWorkspaces
	, handleEventHook    = handleEventHook defaultConfig <+> docksEventHook
	, manageHook         = manageDocks <+> manageHook defaultConfig
        , layoutHook         = avoidStruts  $ layoutHook defaultConfig
        , logHook            = myLogHook p
        }
	`additionalKeys` myAdditionalKeys


------------------------------------------------------------------------}}}
-- Keybindings                                                           {{{
---------------------------------------------------------------------------

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myAdditionalKeys = [
		((mod4Mask .|. shiftMask, xK_p), spawn myLauncher)
	,	((mod4Mask .|. shiftMask, xK_z), spawn myAltTerminal)
	,	((mod4Mask .|. shiftMask, xK_s), spawn myStartupScript)
	,	((mod4Mask .|. shiftMask, xK_l), spawn myLockScreen)
	]

------------------------------------------------------------------------}}}
-- Workspaces                                                           {{{
---------------------------------------------------------------------------

wsAV     = "AV"
wsBSA    = "BSA"
wsCHAT   = "CHAT"
wsCOM    = "COM"
wsDOM    = "DOM"
wsGCC    = "GCC"
wsGEN    = "GEN"
wsGGC    = "GGC"
wsMON    = "MON"
wsOSS    = "OSS"
wsRAD    = "RAD"
wsRW     = "RW"
wsTMP    = "TMP"
wsWRKB   = "WRKB"
wsWRKT   = "WRKT"

-- myWorkspaces = map show [1..9]
myWorkspaces = [wsGEN, wsWRKB, wsWRKT, wsCHAT, wsRW, wsTMP]

projects :: [Project]
projects =

    [ Project   { projectName       = wsGEN
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsGEN myTerminal
                }

    , Project   { projectName       = wsWRKT
                , projectDirectory  = "~/freelancer-dev"
                , projectStartHook  = Just $ do spawnOn wsWRKT myTerminal
                }

    , Project   { projectName       = wsWRKB
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsWRKB myBrowser
                }

    , Project   { projectName       = wsCHAT
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsCHAT myBrowser
                }

    , Project   { projectName       = wsMON
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsMON myTerminal
                }

    , Project   { projectName       = wsRAD
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawn myBrowser
                }

    , Project   { projectName       = wsTMP
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawn myBrowser
                }
    ]

-- Custom PP, determines what is being written to xmobar.
myLogHook h = dynamicLogWithPP $ def 

        { ppCurrent             = xmobarColor active "" . wrap "[" "]"
        , ppTitle               = xmobarColor active "" . shorten 50
        , ppVisible             = xmobarColor base0  "" . wrap "(" ")"
        , ppUrgent              = xmobarColor red    "" . wrap " " " "
        --, ppHidden              = check
        , ppHiddenNoWindows     = const ""
        , ppSep                 = xmobarColor red blue "  :  "
        , ppWsSep               = " "
        , ppLayout              = xmobarColor yellow ""
        , ppOrder               = id
        , ppOutput              = hPutStrLn h  
	}
