{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
import Control.Monad (msum)
-- import Data.Bits ((.|.))
-- import Data.Ratio
-- import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Gnome
-- import XMonad.Config.Xfce
-- import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
-- import XMonad.Layout
import XMonad.Layout.BoringWindows
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
-- import XMonad.Operations
import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- Layouts
layout' = smartBorders $ layoutHints $ avoidStruts $ boringAuto $ spacing 2 $ fair ||| Simplest
    where
      fair = Fair delta ratio
      ratio = 1/2
      delta = 3/100

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

-- Keybindings
modMask' = mod4Mask
defKeys    = keys defaultConfig
delKeys x  = foldr M.delete           (defKeys x) (toRemove x)
newKeys x  = foldr (uncurry M.insert) (delKeys x) (toAdd    x)
toAdd x =
    [ ((modMask x, xK_s), sendMessage NextLayout)
    , ((modMask x, xK_w), kill)
    , ((modMask x, xK_j), focusDown)
    , ((modMask x, xK_k), focusUp)
    ]
toRemove XConfig{modMask = modm} =
    [ (modm              , xK_space ) ]

-- Mouse bindings
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

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0x99999999

-- Gnome-specific
gnomeManageHook = composeAll ([manageHook gnomeConfig, isFullscreen --> doFullFloat])
main = xmonad gnomeConfig
       { manageHook    = gnomeManageHook
       , modMask       = modMask'
       , keys          = newKeys
       , mouseBindings = myMouseBindings
       , layoutHook    = layout'
       , logHook       = myLogHook
       , borderWidth   = 0
       }
