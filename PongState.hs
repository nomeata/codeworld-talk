{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module PongState (pong, pongMB) where

import CodeWorld
import System.Random
import Prelude hiding (init)
import Data.Text (pack)
import System.IO.Unsafe
import Data.IORef

import Abstractions

-- Pong

data PongState = PongState
    { rndGen :: StdGen
    , paddleA :: Double
    , paddleASpeed :: Double
    , paddleB :: Double
    , paddleBSpeed :: Double
    , scoreA :: IORef Integer
    , scoreB :: IORef Integer
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

paddleAt x y = translated x y $
    solidRectangle paddleWidth paddleLength

ballAt (x, y) = translated x y $
    solidCircle ballRadius

drawPong PongState{..} = mconcat
    [ colored green $ paddleAt (-maxX) paddleA
    , colored blue  $ paddleAt maxX paddleB
    , ballAt ballPos
    , colored green $ translated (-9.5) (-9.5) (text (showt (unsafePerformIO (readIORef scoreA))))
    , colored blue  $ translated    9.5 (-9.5) (text (showt (unsafePerformIO (readIORef scoreB))))
    ]

showt x = pack (show x)

(x,y) `plus` (dx,dy) = (x+dx, y+dy)
d `times` (dx,dy) = (d*dx, d*dy)
turnY (dx, dy) = (dx, -dy)

stepPong d ps = reactTop $ reactSide $ boundPaddleA $ boundPaddleB $
    ps { ballPos = pos', ballSpeed = bs',  paddleA = pa, paddleB = pb }
  where pos' = ballPos ps `plus` (d `times` ballSpeed ps)
        bs'  = ballSpeed ps `plus` (d `times` (0,-0.6)) -- gravity
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
    = unsafePerformIO (modifyIORef scoreA (+1)) `seq`
      restart (- newSpeed) (ps { paddleA = 0 })
    | bx < -border && abs (by - paddleA) < (paddleLength/2 + ballRadius)
    = ps { ballSpeed = (-dx, jitter + dy), ballPos = (-border - (bx - (- border)), by), rndGen = gen' }
    | bx < -border
    = unsafePerformIO (modifyIORef scoreB (+1)) `seq`
      restart newSpeed (ps { paddleB = 0 })
    | otherwise = ps
  where
    (bx,by) = ballPos
    (dx,dy) = ballSpeed
    border = maxX - paddleWidth/2 - ballRadius
    (jitter, gen') = randomR (-dirChange * abs dx, dirChange * abs dx) rndGen
    newSpeed = (abs dx + speedIncrease) `min` maxSpeed

handlePong (KeyPress   "W")    ps = ps { paddleASpeed = paddleSpeed }
handlePong (KeyRelease "W")    ps = ps { paddleASpeed = 0 }
handlePong (KeyPress   "S")    ps = ps { paddleASpeed = -paddleSpeed }
handlePong (KeyRelease "S")    ps = ps { paddleASpeed = 0 }
handlePong (KeyPress   "Up")   ps = ps { paddleBSpeed = paddleSpeed }
handlePong (KeyRelease "Up")   ps = ps { paddleBSpeed = 0 }
handlePong (KeyPress   "Down") ps = ps { paddleBSpeed = -paddleSpeed }
handlePong (KeyRelease "Down") ps = ps { paddleBSpeed = 0 }
handlePong _ ps = ps

minePong 0 (KeyPress   "W")    _ = True
minePong 0 (KeyRelease "W")    _ = True
minePong 0 (KeyPress   "S")    _ = True
minePong 0 (KeyRelease "S")    _ = True
minePong 1 (KeyPress   "Up")   _ = True
minePong 1 (KeyRelease "Up")   _ = True
minePong 1 (KeyPress   "Down") _ = True
minePong 1 (KeyRelease "Down") _ = True
minePong _ _ _ = False


initPong gen = PongState gen 0 0 0 0 ref1 ref2 (0,maxY-ballRadius) (initSpeed, 0.8*initSpeed)
  where (ref1, ref2) = unsafePerformIO $ gen `seq` ((,) <$> newIORef 0 <*> newIORef 0)

restart d ps = ps { ballPos = pos, ballSpeed = (d', dx), rndGen = gen'' }
  where
    pos | d < 0     =  (maxX-ballRadius-paddleWidth/2, maxY-ballRadius)
        | otherwise = (-maxX+ballRadius+paddleWidth/2, maxY-ballRadius)
    (d', gen') = randomR (d-0.5,d+0.5) (rndGen ps)
    (dx, gen'') = randomR (-abs d',0) gen'


pong :: StdGen -> Interaction
pong gen = Interaction (initPong gen) stepPong handlePong drawPong

pongMB :: StdGen -> PseudoCollaboration
pongMB gen = PseudoCollaboration (initPong gen) stepPong minePong handlePong (const drawPong)


