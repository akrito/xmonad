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
import Control.Monad (msum)
import XMonad.Hooks.SetWMName

layout' = smartBorders $ layoutHints $ avoidStruts $ fair ||| Full
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

modMask' = mod4Mask

defKeys    = keys defaultConfig
delKeys x  = foldr M.delete           (defKeys x) (toRemove x)
newKeys x  = foldr (uncurry M.insert) (delKeys x) (toAdd    x)
toAdd x =
    [ ((modMask x,  xK_s), sendMessage NextLayout)
    , ((modMask x,  xK_w), kill)
--    , ((modMask x, xK_space), spawn "dlaunch")
    , ((0 , 0x1008FF41), spawn "blank")
    , ((modMask x, xK_Tab), cycleRecentWS [xK_Alt_L] xK_Tab xK_grave)
    ]
toRemove XConfig{modMask = modm} =
    [ (modm              , xK_space ) ]

myWorkspaces = map show [1..4]

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
  xmonad conf
       { startupHook = startupHook conf >> setWMName "LG3D"
       }


conf = ewmh defaultConfig
                     { layoutHook         = layout'
                     -- #adff2f is yellow-green
                     -- #a4c98b is neutral green
                     -- #147427 is darker green
                     , normalBorderColor  = "#888888"
                     , focusedBorderColor = "#adff2f"
                     , modMask            = modMask'
--                     , logHook            = do
--                                              fadeInactiveLogHook 0xaa000000
--                     , focusFollowsMouse  = False
                     , borderWidth        = 2
                     , keys              = newKeys
                     , mouseBindings     = myMouseBindings
                     , workspaces        = myWorkspaces
                     , manageHook = composeAll [
                         isFullscreen --> doFullFloat,
                         className =? "/usr/lib/gnome-do/Do.exe" --> doIgnore
                         ] <+> manageDocks
                     }
