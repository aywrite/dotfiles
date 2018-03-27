------------------------------------------------------------------------}}}
-- Modules                                                              {{{
---------------------------------------------------------------------------
import Data.Monoid
import System.Exit
import System.IO
-- Imports.
import XMonad
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.SpawnOn               -- Spawn windows on a specific workspace
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty         
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.DecorationMadness      -- testing alternative accordion styles
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.ShowWName
import XMonad.Layout.SimplestFloat
import XMonad.Layout.WindowNavigation       -- Navigate directionally between windows
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt  
import XMonad.Util.EZConfig
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W       -- myManageHookShift
--
-- not sure if these do anything yet
import XMonad.Layout.PerScreen              -- Check screen width & adjust layouts
import XMonad.Layout.PerWorkspace           -- Configure layouts on a per-workspace 
import XMonad.Util.WorkspaceCompare

------------------------------------------------------------------------}}}
-- Main                                                                 {{{
---------------------------------------------------------------------------

main = do
        xmproc <- spawnPipe myStatusBar

        xmonad 
            $ dynamicProjects projects
            $ withUrgencyHook LibNotifyUrgencyHook
            $ ewmh
            $ myConfig xmproc


myConfig p = def
        { modMask            = myModMask
        , borderWidth        = border
        , focusedBorderColor = active
        , terminal           = myTerminal
        , workspaces         = myWorkspaces
        , handleEventHook    = myHandleEventHook
        , manageHook         = myManageHook
        , layoutHook         = myLayoutHook
        , logHook            = myLogHook p
        , startupHook        = myStartupHook
        }
        `additionalKeysP` myAdditionalKeys


------------------------------------------------------------------------}}}
-- Applications                                                         {{{
---------------------------------------------------------------------------

myAltTerminal       = "alacritty"
myBrowser           = "google-chrome"
myPersonalBrowser   = myBrowser ++ " --profile-directory=Default"
myTerminal          = "gnome-terminal"
myWorkBrowser       = myBrowser ++ " --profile-directory='Profile 1'"
myWorkChat          = myWorkBrowser ++ " --app=https://chat.tools.flnltd.com/home"
myWorkMonitoring    = myWorkBrowser ++ " --new-window https://grafana.fln.flnltd.com/"
myLauncher          = "rofi -show run"
myLockScreen        = "i3lock"
myStartupScript     = "/home/andrewwright/.xmonad/startup.sh"
myStatusBar         = "xmobar"

------------------------------------------------------------------------}}}
-- Themes                                                               {{{
---------------------------------------------------------------------------

-- Colors
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

-- Fonts
myWideFont   = "-*-terminus-medium-*-*-*-*-180-*-*-*-*-*-*"
myFont       = "-*-terminus-medium-*-*-*-*-160-*-*-*-*-*-*"

-- Size/Layout
gap         = 10
topbar      = 10
border      = 0
prompt      = 20
status      = 20

-- Themes
myShowWNameTheme = def
    { swn_font              = myWideFont
    , swn_fade              = 0.5
    , swn_bgcolor           = "#000000"
    , swn_color             = "#FFFFFF"
    }

topBarTheme = def
    { fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

myPromptTheme = def
    { font                  = myFont
    , bgColor               = base03
    , fgColor               = active
    , fgHLight              = base03
    , bgHLight              = active
    , borderColor           = base03
    , promptBorderWidth     = 0
    , height                = prompt
    , position              = Top
    }

warmPromptTheme = myPromptTheme
    { bgColor               = yellow
    , fgColor               = base03
    , position              = Top
    }

hotPromptTheme = myPromptTheme
    { bgColor               = red
    , fgColor               = base3
    , position              = Top
    }

------------------------------------------------------------------------}}}
-- Hooks                                                                {{{
---------------------------------------------------------------------------

myManageHook =
    manageSpecific
    <+> manageDocks
    <+> manageHook defaultConfig
    <+> namedScratchpadManageHook scratchpads
    <+> fullscreenManageHook
    <+> manageSpawn
    where
        manageSpecific = composeOne
            [ isChat -?> forceCenterFloat
            , isDialog -?> doCenterFloat
            , isFullscreen -?> doFullFloat
            , isRole =? "pop-up" -?> doCenterFloat
            , isRole =? "popup" -?> forceCenterFloat
            , isRole =? "chat_notif" -?> forceCenterFloat
            , isInProperty "_NET_WM_WINDOW_TYPE"
                           "_NET_WM_WINDOW_TYPE_NOTIFICATION" -?> forceCenterFloat
            ]
        isRole = stringProperty "WM_WINDOW_ROLE"

myLayoutHook = showWorkspaceName
        $ onWorkspace wsFLOAT simplestFloat
        $ avoidStruts
        $ addTopBar
        $ layoutHook defaultConfig
    where
        showWorkspaceName   = showWName' myShowWNameTheme
        addTopBar           = noFrillsDeco shrinkText topBarTheme

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]

---------------------------------------------------------------------------
-- X Event Actions
---------------------------------------------------------------------------

-- for reference, the following line is the same as dynamicTitle myDynHook
-- <+> dynamicPropertyChange "WM_NAME" myDynHook

myHandleEventHook = docksEventHook
                <+> dynamicTitle myDynHook
                <+> handleEventHook def
                <+> XMonad.Layout.Fullscreen.fullscreenEventHook
    where
        myDynHook = composeAll
            [ isChat --> forceCenterFloat
            ]


------------------------------------------------------------------------}}}
-- Keybindings                                                           {{{
---------------------------------------------------------------------------

myModMask           = mod4Mask

myAdditionalKeys = [
      ("M-S-p", spawn myLauncher)
    , ("M-S-z", spawn myAltTerminal)
    , ("M-S-s", spawn myStartupScript)
    , ("M-S-l", spawn myLockScreen)
    , ("M-S-t", namedScratchpadAction scratchpads "chatwork")
    , ("M-S-q", confirmPrompt hotPromptTheme "Quit XMonad" $ io (exitWith ExitSuccess))
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
wsFLOAT  = "FLT"

-- myWorkspaces = map show [1..9]
-- TODO auto number these with map
myWorkspaces = [wsGEN, wsWRKT, wsWRKB, wsCHAT, wsMON, wsPB, wsTMP, wsFLOAT]

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

-- Scratch pads

scratchpads =
    [   (NS "chatwork"  myWorkChat isChat defaultFloating)
    ] 

chatWorkResource = "chat.tools.flnltd.com__home"
isChat = (resource =? chatWorkResource)

------------------------------------------------------------------------}}}
-- Status bar                                                           {{{
---------------------------------------------------------------------------

-- Custom PP, determines what is being written to xmobar.
myLogHook h = do

    ewmhDesktopsLogHook
    dynamicLogWithPP $ def 

        { ppCurrent             = xmobarColor active "" . wrap "[" "]"
        , ppTitle               = xmobarColor active "" . shorten 40
        , ppVisible             = xmobarColor base0  "" . wrap "(" ")"
        , ppUrgent              = xmobarColor red    "" . wrap " " " "
        , ppHiddenNoWindows     = xmobarColor inactive ""
        , ppSep                 = xmobarColor red "#000000" "  :  "
        , ppWsSep               = " | "
        , ppLayout              = xmobarColor yellow ""
        , ppOrder               = id
        , ppOutput              = hPutStrLn h  
	}


------------------------------------------------------------------------}}}
-- Startup                                                              {{{
---------------------------------------------------------------------------

myStartupHook = do

    -- init-tilingwm sets up all major "desktop environment" like components
    -- spawnOnce "$HOME/bin/wm/init-tilingwm"
    -- spawn "/home/ethan/bin/wm/init-tilingwm"
    spawn "feh --bg-fill ~/.wallpapers/frosted.jpg"

    --setDefaultCursor xC_left_ptr

quitXmonad :: X ()
quitXmonad = io (exitWith ExitSuccess)

rebuildXmonad :: X ()
rebuildXmonad = do
    spawn "xmonad --recompile && xmonad --restart"

restartXmonad :: X ()
restartXmonad = do
    spawn "xmonad --restart"

---------------------------------------------------------------------------
-- Custom hook helpers
---------------------------------------------------------------------------

-- from:
-- https://github.com/pjones/xmonadrc/blob/master/src/XMonad/Local/Action.hs
--
-- Useful when a floating window requests stupid dimensions.  There
-- was a bug in Handbrake that would pop up the file dialog with
-- almost no height due to one of my rotated monitors.

forceCenterFloat :: ManageHook
forceCenterFloat = doFloatDep move
  where
    move :: W.RationalRect -> W.RationalRect
    move _ = W.RationalRect x y w h

    w, h, x, y :: Rational
    w = 1/3
    h = 2/3
    x = (1-w)/2
    y = (1-h)/2

-- vim: ft=haskell:foldmethod=marker:expandtab:ts=4:shiftwidth=4
