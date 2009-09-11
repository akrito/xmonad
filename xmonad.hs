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
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import Control.Monad (msum)
import qualified Data.Map as M
import Data.Bits ((.|.))
import Data.Ratio
import System.IO

myHandleEventHook = ewmhDesktopsEventHook

layout' = smartBorders $ layoutHints $ avoidStruts $ fair ||| Full
  where
    tiled   = Tall nmaster delta ratio
    fair    = Fair delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

-- A tweak to the Tall layout whereby the number of windows in the master area
-- is calculated on the fly
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

modMask' = mod1Mask

defKeys    = keys defaultConfig
delKeys x  = foldr M.delete           (defKeys x) (toRemove x)
newKeys x  = foldr (uncurry M.insert) (delKeys x) (toAdd    x)
-- remove some of the default key bindings
toRemove x =
    [ (modMask x              , xK_period)
    , (modMask x              , xK_p     )
    , (modMask x .|. shiftMask, xK_p     )
    , (modMask x              , xK_r     )
    ]
-- These are my personal key bindings
toAdd x =
    [ ((modMask x, xK_s), sendMessage NextLayout)
    , ((modMask x, xK_w), kill)
    , ((modMask x, xK_space), spawn "dlaunch")
    , ((mod4Mask,  xK_space), spawn "roxterm")
    --XF86Launch1 :1008FF41
    , ((0 , 0x1008FF41), spawn "blank")
    , ((modMask x, xK_Tab), cycleRecentWS [xK_Alt_L] xK_Tab xK_grave)
    ]

myWorkspaces    = map show [1..6]

alexPP :: Handle -> PP
alexPP h = defaultPP {
                     ppCurrent  = \x -> "^fg(black)^bg(white) " ++ x ++ " "
                   , ppVisible  = \x -> "^fg(white)^bg(black) " ++ x ++ " "
                   , ppHidden   = \x -> "^fg(white)^bg(black) " ++ x ++ " "
                   , ppHiddenNoWindows = const ""
                   , ppWsSep    = ""
                   , ppSep      = ""
                   , ppLayout   = dzenColor "white" "black" .
                                  (\x -> case x of
                                            "Hinted Full"          -> "  ^i(/home/alex/images/bitmaps/full.xbm)"
                                            "Hinted Tall"          -> "  ^i(/home/alex/images/bitmaps/tall.xbm)"
                                            "Hinted Fair"          -> "  ^i(/home/alex/images/bitmaps/fair.xbm)"
                                            _                      -> "  " ++ x
                                  )
                   , ppTitle    = \x -> ""
                   , ppOutput   = hPutStrLn h
                   }

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

--no xft
--statusBarCmd = "dzen2 -ta l -tw 180 -bg black -fg white -fn '-*-proggyclean-*-*-*-*-*-*-*-*-*-*-*-*' -e 'button5=exec:desknext;button4=exec:deskprev'"
--with xft
statusBarCmd = "dzen2 -ta l -tw 300 -bg black -fg white -fn 'DejaVu Sans Mono-8' -e 'button5=exec:desknext;button4=exec:deskprev'"

main = do
  din <- spawnPipe statusBarCmd
  xmonad $ defaultConfig
                     { layoutHook         = layout'
                     -- #adff2f is yellow-green
                     -- #a4c98b is neutral green
                     , normalBorderColor  = "#888888"
                     , focusedBorderColor = "#888888"
                     , modMask            = modMask'
                     , logHook            = do
                                              ewmhDesktopsLogHook
                                              --dynamicLogWithPP (alexPP din)
                                              fadeInactiveLogHook 0xaaffffff
                     , borderWidth        = 1
                     -- , keys            = \c -> keys' `M.union` keys defaultConfig c
                     , keys               = newKeys
                     , mouseBindings     = myMouseBindings
                     , workspaces        = myWorkspaces
                     , manageHook = composeOne [isFullscreen -?> doFullFloat] <+> manageDocks
                     , handleEventHook = myHandleEventHook
                     }
