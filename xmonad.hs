{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.WorkspaceNames
import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Config.Xfce
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Groups.Wmii
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- constants
myActiveBG = "#586e75"
myInactiveBG = "#839496"
launcher = "/home/alex/bin/akmenu"
locker = "/usr/bin/slock"
xmobarCommand = "xmobar"
themeFont = "xft:DejaVuSans:size=7.5"
themeHeight = 14
statusBarFG = "#839496" -- inactive is from Xresources
statusBarBG = "#fdf6e3"
myTerminal = "scratchpad"
-- Who knows?  Maybe I'll go back to dzen someday.
-- dzenCommand = "dzen2 -x 0 -y -1 -w 900 -ta l -fn 'AK Sans Mono-7.5' -bg '#000000' -dock"
dzenCommand = "dzen2 -x 0 -y -1 -ta l -w  900 -fn 'AK Sans Mono-7.5' -bg '#000000' -dock"


-- not really a constant, but feels like it goes here
workSpaceName :: [Char] -> String
workSpaceName x = pad $ case x of
                          "NSP" -> "🌙"
                          _     -> x

-- Layouts
myLayoutHook = smartBorders $ avoidStruts $ wmii shrinkText myTheme

myTheme :: Theme
myTheme = defaultTheme { fontName = themeFont
                       , decoHeight = themeHeight
                       , activeColor = myActiveBG
                       , activeBorderColor = myActiveBG
                       , inactiveColor = myInactiveBG
                       , inactiveBorderColor = myInactiveBG
                       }

-- Keybindings
myModMask :: KeyMask
myModMask = mod4Mask

myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList
    [ ((modMask, xK_f), sendMessage NextLayout)
    , ((modMask, xK_s), groupToTabbedLayout)
    , ((modMask, xK_d), groupToVerticalLayout)
    , ((modMask, xK_w), kill)
    , ((modMask, xK_j), swapDown)
    , ((modMask, xK_k), swapUp)
    , ((mod1Mask, xK_j), focusDown)
    , ((mod1Mask, xK_k), focusUp)
    , ((modMask, xK_h), moveToGroupUp False)
    , ((modMask, xK_l), moveToGroupDown False)
    , ((modMask, xK_a), renameWorkspace defaultXPConfig)
    , ((modMask, xK_space), scratchpadSpawnAction myConfig)
    , ((mod1Mask, xK_space), safeSpawnProg launcher)
    , ((0, 0x1008ff41), safeSpawnProg locker) -- blue button
    , ((0, 0x1008ff2d), safeSpawnProg locker) -- lock
    , ((modMask, xK_equal), zoomGroupIn)
    , ((modMask, xK_minus), zoomGroupOut)
    , ((modMask, xK_Tab), cycleRecentWS [xK_Super_R, xK_Super_L, xK_VoidSymbol] xK_Tab xK_grave)
    ]

-- Mouse bindings
myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((myModMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((myModMask, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    , ((myModMask, button5), nextNonEmptyWS)
    , ((myModMask, button4), prevNonEmptyWS)
    , ((mod1Mask, button5), nextNonEmptyWS)
    , ((mod1Mask, button4), prevNonEmptyWS)
    ]
    where
      nextNonEmptyWS = \_ -> moveTo Next NonEmptyWS
      prevNonEmptyWS = \_ -> moveTo Prev NonEmptyWS

-- Statusbar
myDzenPP :: PP
myDzenPP =  dzenPP { ppLayout   = const ""
                   , ppTitle    = const ""
                   , ppHidden   = workSpaceName
                   , ppCurrent  = dzenColor statusBarFG statusBarBG . workSpaceName
                   , ppVisible  = workSpaceName
                   , ppUrgent   = dzenColor "red" "yellow" . workSpaceName
                   , ppSep      = ""
                   }

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myManageHook = composeAll
                 [ isFullscreen --> doFullFloat
                 , className =? "Plasma-desktop" --> doFloat
                 , scratchpadManageHook (W.RationalRect 0.25 0.25 0.5 0.5)
                 ]

-- Main
main :: IO ()
main = myConfigWithBar >>= xmonad
--main = xmonad myConfig

baseConfig = defaultConfig
myConfig = withUrgencyHook NoUrgencyHook $ baseConfig
               { manageHook         = myManageHook <+> manageHook baseConfig
               , modMask            = myModMask
               , keys               = myKeys <+> keys baseConfig
               , mouseBindings      = myMouseBindings
               , layoutHook         = myLayoutHook
               , borderWidth        = 1
               , focusedBorderColor = statusBarFG
               , normalBorderColor  = statusBarBG
               , startupHook        = setWMName "LG3D"
               , logHook            = takeTopFocus >> setWMName "LG3D"
               , terminal           = myTerminal
               , workspaces         = ["1 edit", "2 web", "3", "4"]
               }
myConfigWithBar = statusBar dzenCommand myDzenPP toggleStrutsKey myConfig
