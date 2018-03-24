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

-- My Applicatoins
myAltTerminal       = "gnome-terminal"
myBrowser           = "google-chrome"
myPersonalBrowser   = myBrowser ++ " --profile-directory=Default"
myTerminal          = "alacritty"
myWorkBrowser       = myBrowser ++ " --profile-directory='Profile 1'"
myWorkChat          = myWorkBrowser ++ " --app=https://chat.tools.flnltd.com/home"
myWorkMonitoring    = myWorkBrowser ++ " https://grafana.fln.flnltd.com/"

-- My Config Params
myLauncher          = "rofi -show run"
myLockScreen        = "i3lock"
myModMask           = mod4Mask
myStartupScript     = "/home/andrewwright/.xmonad/startup.sh"
myStatusBar         = "xmobar"

-- Colors
xmobarTitleColor = "#429942"
xmobarCurrentWorkspaceColor = "#429942"

active       = blue
activeWarn   = red
inactive     = base02
focusColor   = blue
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
green   = "#859900"

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
wsCHAT   = "4: CHAT"
wsCOM    = "COM"
wsDOM    = "DOM"
wsGCC    = "GCC"
wsGEN    = "1: GEN"
wsGGC    = "GGC"
wsMON    = "5: MON"
wsOSS    = "OSS"
wsRAD    = "RAD"
wsPB     = "6: PB"
wsTMP    = "7: TMP"
wsWRKB   = "3: WRKB"
wsWRKT   = "2: WRKT"

-- myWorkspaces = map show [1..9]
myWorkspaces = [wsGEN, wsWRKT, wsWRKB, wsCHAT, wsMON, wsPB, wsTMP]

projects :: [Project]
projects =

    [ Project   { projectName       = wsGEN
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsGEN myTerminal
		                                spawnOn wsGEN myBrowser
                }

    , Project   { projectName       = wsWRKT
                , projectDirectory  = "~/freelancer-dev"
                , projectStartHook  = Just $ do spawnOn wsWRKT myTerminal
                }

    , Project   { projectName       = wsWRKB
                , projectDirectory  = "~/freelancer-dev"
                , projectStartHook  = Just $ do spawnOn wsWRKB myWorkBrowser
                }

    , Project   { projectName       = wsCHAT
                , projectDirectory  = "~/freelancer-dev"
                , projectStartHook  = Just $ do spawnOn wsCHAT myWorkChat
                }

    , Project   { projectName       = wsMON
                , projectDirectory  = "~/freelancer-dev"
                , projectStartHook  = Just $ do spawnOn wsMON myWorkMonitoring
                }

    , Project   { projectName       = wsPB
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsPB myPersonalBrowser
                }

    , Project   { projectName       = wsTMP
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsTMP myPersonalBrowser
                }
    ]

------------------------------------------------------------------------}}}
-- Status bar                                                           {{{
---------------------------------------------------------------------------

-- Custom PP, determines what is being written to xmobar.
myLogHook h = dynamicLogWithPP $ def 

        { ppCurrent             = xmobarColor active "" . wrap "[" "]"
        , ppTitle               = xmobarColor active "" . shorten 50
        , ppVisible             = xmobarColor base0  "" . wrap "(" ")"
        , ppUrgent              = xmobarColor red    "" . wrap " " " "
        , ppHiddenNoWindows     = const ""
        , ppSep                 = xmobarColor red blue "  :  "
        , ppWsSep               = " | "
        , ppLayout              = xmobarColor yellow ""
        , ppOrder               = id
        , ppOutput              = hPutStrLn h  
	}

-- vim: ft=haskell:foldmethod=marker:expandtab:ts=4:shiftwidth=4
