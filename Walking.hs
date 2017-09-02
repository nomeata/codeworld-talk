{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Walking (walkingMB) where

import CodeWorld

import Abstractions (Interaction(Interaction), PseudoCollaboration(PseudoCollaboration))
import Prelude hiding (init)

data PlayerState = PlayerState
    { pos :: Point
    , dir :: Double
    , ddir :: Double
    , speed :: Double
    }

data State = State
    { playerA :: PlayerState
    , playerB :: PlayerState
    }

playerR = 2
playerSpeed = 3
playerRot = 1

drawPlayer active color (PlayerState{..}) =
    uncurry translated pos $
    rotated dir $
    scaled playerR playerR $
    mconcat [ if active then thickCircle 0.06 (1+0.025) else blank
            , translated 0.4 0.5 eye
            , translated (-0.4) 0.5 eye
            , colored color $ solidCircle 1
            ]
  where eye = mconcat [ translated 0.0 0.1 (solidCircle 0.1)
                      , colored white (solidCircle 0.25)
                      ]

draw num (State{..}) = mconcat
    [ drawPlayer (num == 0) green playerA
    , drawPlayer (num == 1) blue  playerB
    ]

(x,y) `plus` (dx,dy) = (x+dx, y+dy)
d `times` (dx,dy) = (d*dx, d*dy)

step d (State{..}) = State { playerA = stepPlayer d playerA
                           , playerB = stepPlayer d playerB
                           }

stepPlayer d ps = ps { pos = pos', dir = dir' }
  where pos' = clamp2 $ pos ps `plus` ((d * speed ps) `times` (-sin dir', cos dir'))
        dir' = dir ps + (d * ddir ps)

clamp2 (x,y) = (clamp x, clamp y)

clamp x = (x `min` (10 - playerR))  `max` (-10 + playerR)


handle (KeyPress   "W")     s = s { playerA = (playerA s) { speed = playerSpeed }}
handle (KeyRelease "W")     s = s { playerA = (playerA s) { speed = 0 }}
handle (KeyPress   "A")     s = s { playerA = (playerA s) { ddir  = playerRot }}
handle (KeyRelease "A")     s = s { playerA = (playerA s) { ddir  = 0 }}
handle (KeyPress   "D")     s = s { playerA = (playerA s) { ddir  = - playerRot }}
handle (KeyRelease "D")     s = s { playerA = (playerA s) { ddir  = 0 }}
handle (KeyPress   "Up")    s = s { playerB = (playerB s) { speed = playerSpeed }}
handle (KeyRelease "Up")    s = s { playerB = (playerB s) { speed = 0 }}
handle (KeyPress   "Left")  s = s { playerB = (playerB s) { ddir  = playerRot }}
handle (KeyRelease "Left")  s = s { playerB = (playerB s) { ddir  = 0 }}
handle (KeyPress   "Right") s = s { playerB = (playerB s) { ddir  = - playerRot }}
handle (KeyRelease "Right") s = s { playerB = (playerB s) { ddir  = 0 }}
handle _ ps = ps

mine 0 (KeyPress   "W")     _ = True
mine 0 (KeyRelease "W")     _ = True
mine 0 (KeyPress   "A")     _ = True
mine 0 (KeyRelease "A")     _ = True
mine 0 (KeyPress   "D")     _ = True
mine 0 (KeyRelease "D")     _ = True
mine 1 (KeyPress   "Up")    _ = True
mine 1 (KeyRelease "Up")    _ = True
mine 1 (KeyPress   "Left")  _ = True
mine 1 (KeyRelease "Left")  _ = True
mine 1 (KeyPress   "Right") _ = True
mine 1 (KeyRelease "Right") _ = True
mine _ _ _ = False


init = State
    (PlayerState (-3,-5) 0 0 0)
    (PlayerState ( 3,-5) 0 0 0)

walkingMB :: PseudoCollaboration
walkingMB = PseudoCollaboration init step mine handle draw


