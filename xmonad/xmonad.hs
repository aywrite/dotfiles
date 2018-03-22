import System.IO
-- Imports.
import Data.Monoid
import XMonad
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.PerScreen              -- Check screen width & adjust layouts
import XMonad.Layout.PerWorkspace           -- Configure layouts on a per-workspace 
-- The main function.
main = do
        xmproc <- spawnPipe myStatusBar

        xmonad
            $dynamicProjects projects
            $myConfig

myAltTerminal       = "gnome-terminal"
myBrowser           = "google-chrome"
myLauncher          = "rofi -show run"
myLockScreen        = "i3lock"
myModMask           = mod4Mask
myStartupScript     = "/home/andrewwright/.xmonad/startup.sh"
myStatusBar         = "xmobar"
myTerminal          = "alacritty"

xmobarTitleColor = "#429942"
xmobarCurrentWorkspaceColor = "#429942"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
--myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main configuration, override the defaults to your liking.
myConfig = defaultConfig
        { modMask            = myModMask
        , terminal           = myTerminal
        , workspaces         = myWorkspaces
        , layoutHook         = avoidStruts  $  layoutHook defaultConfig
        , logHook            = dynamicLogWithPP $ xmobarPP {
	 	ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
          	, ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
          	, ppSep = "   "
      		}
        }
	`additionalKeys` myAdditionalKeys


myAdditionalKeys = [
		((mod4Mask .|. shiftMask, xK_p), spawn myLauncher)
	,	((mod4Mask .|. shiftMask, xK_z), spawn myAltTerminal)
	,	((mod4Mask .|. shiftMask, xK_s), spawn myStartupScript)
	,	((mod4Mask .|. shiftMask, xK_l), spawn myLockScreen)
	]

------------------------------------------------------------------------}}}
-- Workspaces                                                           {{{
---------------------------------------------------------------------------

wsAV    = "AV"
wsBSA   = "BSA"
wsCOM   = "COM"
wsDOM   = "DOM"
wsWRKB   = "WRKB"
wsGEN   = "GEN"
wsGCC   = "GCC"
wsMON   = "MON"
wsOSS   = "OSS"
wsRAD   = "RAD"
wsRW    = "RW"
wsWRKT  = "WRKT"
wsTMP   = "TMP"
wsVIX   = "VIX"
wsGGC   = "GGC"

-- myWorkspaces = map show [1..9]
myWorkspaces = [wsGEN, wsWRKB, wsWRKT, wsMON, wsRW, wsTMP]

projects :: [Project]
projects =

    [ Project   { projectName       = wsGEN
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsGEN myTerminal
                }

    , Project   { projectName       = wsWRKT
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsWRKT myTerminal
                }

    , Project   { projectName       = wsWRKB
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsWRKB myBrowser
                }

    , Project   { projectName       = wsVIX
                , projectDirectory  = "~/.xmonad"
                , projectStartHook  = Just $ do spawnOn wsVIX myTerminal
                                                spawnOn wsVIX myTerminal
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
