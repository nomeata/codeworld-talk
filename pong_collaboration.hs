{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StaticPointers #-}

import CodeWorld
import System.Random
import Prelude hiding (init)
import Data.Text (pack)

main = collaborationOf 2 (static initPong) (static stepPong) (static handlePong) (static (const drawPong))

-- Pong

data PongState = PongState
    { rndGen :: StdGen
    , paddleA :: Double
    , paddleASpeed :: Double
    , paddleB :: Double
    , paddleBSpeed :: Double
    , scoreA :: Integer
    , scoreB :: Integer
    , ballPos :: (Double, Double)
    , ballSpeed :: (Double, Double)
    }

paddleWidth = 0.2
paddleLength = 1.5
padding = 1
maxX = 10 - padding
maxY = 10
ballRadius = 0.2
paddleSpeed = 4
initSpeed = 3
dirChange = 1
speedIncrease = 0.5
maxSpeed = 5

addPadding px py sx sy = scaled ((sx - px - px) / sx) ((sy - py - py) / sy)

paddleAt x y = translated x y $
    solidRectangle paddleWidth paddleLength

ballAt (x, y) = translated x y $
    solidCircle ballRadius

drawPong (PongState {..}) = mconcat
    [ paddleAt (-maxX) paddleA
    , paddleAt maxX paddleB
    , ballAt ballPos
    , translated (-9.5) (-9.5) (text (showt scoreA))
    , translated  (9.5) (-9.5) (text (showt scoreB))
    ]

showt x = pack (show x)

(x,y) `plus` (dx,dy) = (x+dx, y+dy)
d `times` (dx,dy) = (d*dx, d*dy)
turnY (dx, dy) = (dx, -dy)

stepPong d ps = reactTop $ reactSide $ boundPaddleA $ boundPaddleB $
    ps { ballPos = pos', paddleA = pa, paddleB = pb }
  where pos' = ballPos ps `plus` (d `times` ballSpeed ps)
        pa   = paddleA ps + (d * paddleASpeed ps)
        pb   = paddleB ps + (d * paddleBSpeed ps)

clamp (l,u) x = (x `min` u) `max` l 

boundPaddleA :: PongState -> PongState
boundPaddleA ps = ps { paddleA = pos }
  where
    pos = clamp (-maxY+paddleLength/2, maxY-paddleLength/2) (paddleA ps)

boundPaddleB :: PongState -> PongState
boundPaddleB ps = ps { paddleB = pos }
  where
    pos = clamp (-maxY+paddleLength/2, maxY-paddleLength/2) (paddleB ps)

reactTop :: PongState -> PongState
reactTop ps@PongState{..}
    | by > border
    = ps { ballSpeed = turnY ballSpeed, ballPos = (bx, border - (by - border)) }
    | by < -border
    = ps { ballSpeed = turnY ballSpeed, ballPos = (bx, (-border) - (by - (-border))) }
    | otherwise = ps
  where
    (bx,by) = ballPos
    border = maxY - ballRadius

reactSide :: PongState -> PongState
reactSide ps@PongState{..}
    | bx > border  && abs (by - paddleB) < (paddleLength/2 + ballRadius)
    = ps { ballSpeed = (-dx, jitter + dy), ballPos = (border - (bx - border), by), rndGen = gen' }
    | bx > border
    = restart (- newSpeed) (ps { scoreA = scoreA + 1, paddleA = 0 })
    | bx < -border && abs (by - paddleA) < (paddleLength/2 + ballRadius)
    = ps { ballSpeed = (-dx, jitter + dy), ballPos = (-border - (bx - (- border)), by), rndGen = gen' }
    | bx < -border
    = restart newSpeed (ps { scoreB = scoreB + 1, paddleB = 0 })
    | otherwise = ps
  where
    (bx,by) = ballPos
    (dx,dy) = ballSpeed
    border = maxX - paddleWidth/2 - ballRadius
    (jitter, gen') = randomR (-dirChange * abs dx, dirChange * abs dx) rndGen
    newSpeed = (abs dx + speedIncrease) `min` maxSpeed

handlePong 0 (KeyPress   "W")    ps = ps { paddleASpeed = paddleSpeed }
handlePong 0 (KeyRelease "W")    ps = ps { paddleASpeed = 0 }
handlePong 0 (KeyPress   "S")    ps = ps { paddleASpeed = -paddleSpeed }
handlePong 0 (KeyRelease "S")    ps = ps { paddleASpeed = 0 }
handlePong 0 (KeyPress   "Up")   ps = ps { paddleASpeed = paddleSpeed }
handlePong 0 (KeyRelease "Up")   ps = ps { paddleASpeed = 0 }
handlePong 0 (KeyPress   "Down") ps = ps { paddleASpeed = -paddleSpeed }
handlePong 0 (KeyRelease "Down") ps = ps { paddleASpeed = 0 }
handlePong 1 (KeyPress   "W")    ps = ps { paddleBSpeed = paddleSpeed }
handlePong 1 (KeyRelease "W")    ps = ps { paddleBSpeed = 0 }
handlePong 1 (KeyPress   "S")    ps = ps { paddleBSpeed = -paddleSpeed }
handlePong 1 (KeyRelease "S")    ps = ps { paddleBSpeed = 0 }
handlePong 1 (KeyPress   "Up")   ps = ps { paddleBSpeed = paddleSpeed }
handlePong 1 (KeyRelease "Up")   ps = ps { paddleBSpeed = 0 }
handlePong 1 (KeyPress   "Down") ps = ps { paddleBSpeed = -paddleSpeed }
handlePong 1 (KeyRelease "Down") ps = ps { paddleBSpeed = 0 }
handlePong _ _ ps = ps


initPong gen = PongState gen 0 0 0 0 0 0 (0,maxY-ballRadius) (initSpeed, 1.1*initSpeed)

restart d ps = ps { ballPos = pos, ballSpeed = (d, dx), rndGen = gen' }
  where
    pos | d < 0     =  (maxX-ballRadius-paddleWidth/2, maxY-ballRadius)
        | otherwise = (-maxX+ballRadius+paddleWidth/2, maxY-ballRadius)
    (dx, gen') = randomR (-abs d,0) (rndGen ps)



