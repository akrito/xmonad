{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
import Monad (liftM)
import XMonad
import XMonad.Operations
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.LayoutHints
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import Control.Monad (msum)
import qualified Data.Map as M
import Data.Bits ((.|.))
import Data.Ratio
import System.IO
import XMonad.Actions.GridSelect

myHandleEventHook = ewmhDesktopsEventHook

layout' = smartBorders $ layoutHints $ avoidStruts $ fair ||| Full
  where
    tiled   = Tall nmaster delta ratio
    fair    = Fair delta ratio
    three   = ThreeCol 1 (3/100) (1/2)
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

-- A tweak to the Tall layout where the number of windows in the master area is
-- calculated on the fly
data Fair a = Fair { fairRatio :: !Rational
                   , fairRatioIncrement :: !Rational }
                deriving (Show, Read)

instance LayoutClass Fair a where
    description _ = "Fair"
    pureLayout (Fair delta frac) r s = pureLayout (Tall nmaster delta frac) r s
      where
        n = length $ W.integrate s
        nmaster = floor $ fromIntegral n / 2

    pureMessage (Fair delta frac) m =
            msum [fmap resize     (fromMessage m)]

      where resize Shrink             = Fair delta (max 0 $ frac-delta)
            resize Expand             = Fair delta (min 1 $ frac+delta)

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
    , ((modMask x, xK_g), goToSelected defaultGSConfig)
    ]

myWorkspaces = map show [1..6]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask', button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask', button2), (\w -> focus w >> windows W.swapMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask', button3), (\w -> focus w >> mouseResizeWindow w))
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
  xmonad $ defaultConfig
                     { layoutHook         = layout'
                     -- #adff2f is yellow-green
                     -- #a4c98b is neutral green
                     , normalBorderColor  = "#888888"
                     , focusedBorderColor = "#888888"
                     , modMask            = modMask'
                     , logHook            = do
                                              ewmhDesktopsLogHook
                                              fadeInactiveLogHook 0xcc000000
                     , borderWidth        = 1
                     , keys               = newKeys
                     , mouseBindings     = myMouseBindings
                     , workspaces        = myWorkspaces
                     , manageHook = composeOne [isFullscreen -?> doFullFloat] <+> manageDocks
                     , handleEventHook = myHandleEventHook
                     }
