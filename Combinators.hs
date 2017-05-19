{-# LANGUAGE OverloadedStrings #-}
module Combinators where

import CodeWorld
import Data.Text (pack, Text)
import Data.List

import Abstractions
import Prediction
import qualified PredictionLargeStep as PLS
import Unsafe.Coerce

drawDelayed :: Int -> Double -> Double -> [(Double, Double, Event)] -> Picture -> Picture
drawDelayed me now delay xs p
    = translated 8 9.2 (text (showt (round (delay*1000)::Int) <> "ms"))
    {-
    <> flip' (mconcat [ translated (10 - x*20) (-8.5) triangle
                      | (end, start, _) <- xs
                      , end > start
                      , let x = (now-start)/(end-start)
                      ])
    -}
    <> p
  where
    triangle = colored red $ solidPolygon [ (0,0), (1,0.5), (1,-0.5) ]
    flip' = if me == 0 then id else scaled (-1) 1

delayEvents :: Int -> PseudoCollaboration -> Interaction
delayEvents me (PseudoCollaboration initPC stepPC minePC handlePC drawPC)
    = Interaction init step handle draw
  where
    other = 1 - me
    init = (0, 0, [], initPC)
    step d (now, delay, xs, s)
      = (now + d, delay, postpone, s')
      where (todo, postpone) = span (\ (when, _, _) -> when < now) xs
            s' = stepPC d $ foldl' go s todo
              where go s'' (_, _, e) = handlePC e s''
    handle (KeyPress "=") (now, delay, xs, s) = (now, delay + 0.1, xs, s)
    handle (KeyPress "Unknown:173") s = handle (KeyPress "-") s
    handle (KeyPress "-") (now, delay, xs, s) = (now, max 0 (delay - 0.1), xs, s)
    handle e (now, delay, xs, s)
      | minePC me    e s = (now, delay, xs, handlePC e s)
      | minePC other e s = (now, delay, xs ++ [(now + delay, now, e)], s)
      | otherwise     = (now, delay, xs, s)
    draw (now, delay, xs, s)
        = drawDelayed me now delay xs $ drawPC me s

delayEventsPrediction :: Int -> PseudoCollaboration -> Interaction
delayEventsPrediction me
    PseudoCollaboration{initPC = initPC, stepPC = stepPC, handlePC = handlePC, drawPC = drawPC, minePC = minePC}
  = Interaction init step handle draw
  where
    other = 1 - me
    rate = 1 / 16
    init
      = (0, 0, [], initFuture initPC 2)
    step d (now, delay, xs, s)
      = (now + d, delay, postpone, s')
      where (todo, postpone) = span (\ (when, _, _) -> when < now) xs
            s' = currentTimePasses stepPC rate (now + d) $
                 addEvent stepPC rate me    now Nothing $
                 addEvent stepPC rate other (now - delay) Nothing $
                 foldl' go s $
                 todo
              where go s'' (_, a, e) = addEvent stepPC rate other a (Just (handlePC e)) s''

    handle (KeyPress "=") (now, delay, xs, s) = (now, delay + 0.1, xs, s)
    handle (KeyPress "Unknown:173") s = handle (KeyPress "-") s
    handle (KeyPress "-") (now, delay, xs, s) = (now, max 0 (delay - 0.1), xs, s)
    handle e (now, delay, xs, s)
        | minePC me    e c = (now, delay, xs, addEvent stepPC rate me now (Just (handlePC e)) s)
        | minePC other e c = (now, delay, xs++[(now + delay, now, e)], s)
        | otherwise        = (now, delay, xs, s)
      where c = currentState stepPC rate now s

    draw (now, delay, xs, s)
        = drawDelayed me now delay xs $ drawPC me (currentState stepPC rate now s)


delayEventsLargeStepPrediction :: Int -> PseudoCollaboration -> Interaction
delayEventsLargeStepPrediction me
    PseudoCollaboration{initPC = initPC, stepPC = stepPC, handlePC = handlePC, drawPC = drawPC, minePC = minePC}
  = Interaction init step handle draw
  where
    other = 1 - me
    init
      = (0, 0, [], PLS.initFuture initPC 2)
    step d (now, delay, xs, s)
      = (now + d, delay, postpone, s')
      where (todo, postpone) = span (\ (when, _, _) -> when < now) xs
            s' = PLS.currentTimePasses stepPC (now + d) $
                 PLS.addEvent stepPC me    now Nothing $
                 PLS.addEvent stepPC other (now - delay) Nothing $
                 foldl' go s $
                 todo
              where go s'' (_, a, e) = PLS.addEvent stepPC other a (Just (handlePC e)) s''

    handle (KeyPress "=") (now, delay, xs, s) = (now, delay + 0.1, xs, s)
    handle (KeyPress "Unknown:173") s = handle (KeyPress "-") s
    handle (KeyPress "-") (now, delay, xs, s) = (now, max 0 (delay - 0.1), xs, s)
    handle e (now, delay, xs, s)
        | minePC me    e c = (now, delay, xs, PLS.addEvent stepPC me now (Just (handlePC e)) s)
        | minePC other e c = (now, delay, xs++[(now + delay, now, e)], s)
        | otherwise        = (now, delay, xs, s)
      where c = PLS.currentState stepPC now s

    draw (now, delay, xs, s)
        = drawDelayed me now delay xs $ drawPC me (PLS.currentState stepPC now s)
showt :: Show a => a -> Text
showt x = pack (show x)

runInteraction :: Interaction -> IO ()
runInteraction (Interaction init step handle draw)
    = interactionOf init step handle draw

-- Slideshow

data SlideShowState = Showing Int | Sliding Double Int Int

-- | Switches upon PageDown/PageUp/Home/End
-- Keeps state, but pauses invisible interactions
slideshow :: [Interaction] -> Interaction
slideshow slides = Interaction init step handle draw
  where
    speed = 2

    init = (Showing 0, map init' slides)

    step   d (Showing n, ss) = (Showing n, update n (step'   (slides !! n) d) ss)
    step   d (Sliding x f t, ss) | abs dist <= d * speed = (Showing t, ss)
                                 | otherwise             = (Sliding x' f t, ss)
        where x' = x + d*speed * signum dist
              dist = fromIntegral t - x

    next (KeyPress "PageDown") n = Just $ (n + 1) `min` (length slides -1)
    next (KeyPress "Right")    n = Just $ (n + 1) `min` (length slides -1)
    next (KeyPress "PageUp")   n = Just $ (n - 1) `max` 0
    next (KeyPress "Left")     n = Just $ (n - 1) `max` 0
    next (KeyPress "End")      n = Just $ length slides - 1
    next (KeyPress "Home")     n = Just  0
    next _ _ = Nothing

    handle e (Showing n, ss)     | Just n' <- next e n , n' /= n
        = (Sliding x n n', ss) where x = fromIntegral n
    handle e (Sliding x f t, ss) | Just t' <- next e t , t' /= t
        = (Sliding x f t', ss)
    handle e (Showing n, ss)
        = (Showing n, update n (handle' (slides !! n) e) ss)
    handle _ (s,ss) = (s,ss)

    draw     (Sliding x f t, ss) = mconcat
        [ scaled s s $
          mconcat [ translated (21 * (fromIntegral n - x)) 0 (drawSlide ss n)
                  | n <- range ]
        , background (gray 0.3)
        ]
      where
        range = [max 0 (floor (x-0.5)) .. min (ceiling (x+0.5)) (length slides -1)]
        -- s = (cos (x * 2 * pi) + 3)/4
        s | dist < 0.5 = 1 - (sin (dist * pi)/4)
          | otherwise  = 0.75
        dist = min (abs (x - fromIntegral t)) (abs (x - fromIntegral f))

    draw     (Showing n, ss) = drawSlide ss n

    background c = colored c (solidRectangle 20 20)

    drawSlide ss n = draw' (slides !! n) (ss !! n) <> background white

    -- ARG! How do do this in a type-safe way?
    init'   (Interaction init _    _      _   ) = unsafeCoerce init
    step'   (Interaction _    step _      _   ) = unsafeCoerce step
    handle' (Interaction _    _    handle _   ) = unsafeCoerce handle
    draw'   (Interaction _    _    _      draw) = unsafeCoerce draw

update :: Int -> (a -> a) -> [a] -> [a]
update _ _ [] = error "update: List too short"
update 0 f (x:xs) = f x : xs
update n f (x:xs) = x : update (n-1) f xs


-- Restart
restartable :: Interaction -> Interaction
restartable (Interaction init step handle1 draw)
    = Interaction init step handle draw
  where
    handle (KeyPress "Esc") _ = init
    handle e s = handle1 e s

pausable :: Interaction -> Interaction
pausable = genPausable False

paused :: Interaction -> Interaction
paused = genPausable True

genPausable :: Bool ->  Interaction -> Interaction
genPausable paused (Interaction init step handle draw)
    = Interaction init' step' handle' draw'
  where
    init' = (paused, init)
    step' _ (True,  s) = (True,  s)
    step' d (False, s) = (False, step d s)
    handle' (KeyPress "P") (p, s) = (not p, s)
    handle' _ (True,s)  = (True, s)
    handle' e (False,s) = (False, handle e s)
    -- if we start paused, do not visualize this
    draw' (True, s) | paused = draw s
    draw' (True, s) =
        text "- paused -" <>
        colored (translucent white) (solidRectangle 20 20) <>
        draw s
    draw' (False, s) = draw s

-- Overlay

overlay :: Interaction -> Interaction -> Interaction
overlay = tweenOn "O" overlayTween coord

coord :: Double -> Point -> Point
coord d (x,y) | d > 0.5, y > 0 = (2 * x + 5, 2*y - 10)
              | d > 0.5        = (2 * x - 5, 2*y + 10)
coord _ p = p

overlayTween :: Double -> Picture -> Picture -> Picture
overlayTween d p1 p2 = scaled s s $
       translated (d * (-5)) (d * 10)    (framed green <> p1)
    <> translated (d * 5)    (d * (-10)) (framed blue  <> p2)
  where
    -- s = 1-d*(1 - 0.5)
    s = 1/(1 + d)
    {-
    draw (True, s1, s2) =  colored (translucent green) (draw1 s1)
                        <> colored (translucent blue)  (draw2 s2)
    draw (False, s1, s2) = scaled 0.5 0.5 (translated (-5) 10 (framed green (draw1 s1)))
                        <> scaled 0.5 0.5 (translated 5 (-10) (framed blue (draw2 s2)))
    -}
    framed c | d > 0     = colored c     (rectangle (20-0.1) (20-0.1))
             | otherwise = blank

tweenOn :: Text -> (Double -> Picture -> Picture -> Picture) -> (Double -> Point -> Point)
    -> Interaction -> Interaction -> Interaction
tweenOn key combine coord
    (Interaction init1 step1 handle1 draw1)
    (Interaction init2 step2 handle2 draw2)
    = Interaction init step handle draw
  where
    init = (False, 0, init1, init2)
    step d (t, c, s1,s2) = (t, c', step1 d s1, step2 d s2)
      where c' = if t then (c + d) `min` 1
                      else (c - d) `max` 0

    handle (KeyPress k) (t, c, s1, s2) | k == key  = (not t, c, s1, s2)
    handle e            (t, c, s1, s2) = (t, c, handle1 f s1, handle2 f s2)
        where f = fixEvent c e

    fixEvent d (MousePress mb p)   = MousePress mb   (coord d p)
    fixEvent d (MouseRelease mb p) = MouseRelease mb (coord d p)
    fixEvent d (MouseMovement p)   = MouseMovement   (coord d p)
    fixEvent _ e = e

    draw (_, c, s1, s2)  = combine c (draw1 s1) (draw2 s2)

-- Animation

animate :: (Double -> Picture) -> Interaction
animate = Interaction 0 (+) (const id)
