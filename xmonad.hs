{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
import XMonad
import XMonad.Operations
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.LayoutHints
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.MouseResizableTile
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import qualified Data.Map as M
import Data.Bits ((.|.))
import Data.Ratio
import System.IO

layout' = smartBorders $ layoutHints $ avoidStruts $ mouseResizableTile ||| Full

-- A tweak to the Tall layout where the number of windows in the master area is
-- calculated on the fly

modMask' = mod4Mask

defKeys    = keys defaultConfig
newKeys x  = foldr (uncurry M.insert) (defKeys x) (toAdd    x)
-- These are my personal key bindings
toAdd x =
    [ ((modMask x,  xK_s), sendMessage NextLayout)
    , ((modMask x,  xK_w), kill)
    , ((modMask x, xK_space), spawn "dlaunch")
    , ((0 , 0x1008FF41), spawn "blank")
    , ((modMask x, xK_Tab), cycleRecentWS [xK_Alt_L] xK_Tab xK_grave)
    ]

myWorkspaces = map show [1..6]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask', button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask', button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    -- cycle through workspaces
    , ((modMask', button5), nextNonEmptyWS)
    , ((modMask', button4), prevNonEmptyWS)
    , ((mod1Mask, button5), nextNonEmptyWS)
    , ((mod1Mask, button4), prevNonEmptyWS)
    ]
    where
      nextNonEmptyWS = \_ -> moveTo Next NonEmptyWS
      prevNonEmptyWS = \_ -> moveTo Prev NonEmptyWS

main = do
  spawn "xfce4-panel"
  xmonad $ ewmh defaultConfig
                     { layoutHook         = layout'
                     -- #adff2f is yellow-green
                     -- #a4c98b is neutral green
                     -- #147427 is darker green
                     , normalBorderColor  = "#888888"
                     , focusedBorderColor = "#147427"
                     , modMask            = modMask'
                     , logHook            = do
                                              fadeInactiveLogHook 0xaa000000
                     , focusFollowsMouse  = False
                     , borderWidth        = 2
                     , keys               = newKeys
                     , mouseBindings     = myMouseBindings
                     , workspaces        = myWorkspaces
                     , manageHook = composeOne [isFullscreen -?> doFullFloat] <+> manageDocks
                     }
