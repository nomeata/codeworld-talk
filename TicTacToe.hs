{-# LANGUAGE OverloadedStrings #-}
module TicTacToe (ticTacToe, ticTacToeMP) where

import Data.Maybe
import CodeWorld
import Prelude hiding (init)

import Abstractions

type TTTState = [[Maybe Bool]]

boxWidth :: Double
boxWidth = 6

drawTTT :: TTTState -> Picture
drawTTT boxes = mconcat
    [ mconcat [
            at x y $ drawCell cell
        | (x, cell) <- zip [0..] row ]
    | (y, row) <- zip [0..] boxes ] <>
    mconcat [
        path [(-1.5*boxWidth,(-1.5+x)*boxWidth), (1.5*boxWidth,(-1.5+x)*boxWidth)]
    | x <- [0..3] ] <>
    mconcat [
        path [((-1.5+x)*boxWidth,-1.5*boxWidth), ((-1.5+x)*boxWidth,1.5*boxWidth)]
    | x <- [0..3] ]

drawCell :: Maybe Bool -> Picture
drawCell Nothing = mempty
drawCell (Just True)
    = colored green $
       thickPath 0.4 [(-0.4 * boxWidth, -0.4 * boxWidth), (0.4 * boxWidth,  0.4 * boxWidth)]
    <> thickPath 0.4 [(-0.4 * boxWidth,  0.4 * boxWidth), (0.4 * boxWidth, -0.4 * boxWidth)]
drawCell (Just False)
    = colored blue $ thickCircle 0.4 (-0.4*boxWidth)

at :: Int -> Int -> Picture -> Picture
at x y = translated ((-1 + fromIntegral x) * boxWidth) ((-1 + fromIntegral y) * boxWidth)

cellOf :: Double -> Double -> Maybe (Int,Int)
cellOf x y
    | 0 <= i, i < 3, 0 <= j, j < 3
    = Just (i,j)
    | otherwise
    = Nothing
  where
    i = round (x/boxWidth + 1)
    j = round (y/boxWidth + 1)

stepTTT :: Double -> TTTState -> TTTState
stepTTT _ ps = ps

handleTTT :: Event -> TTTState -> TTTState
handleTTT (MousePress LeftButton (x,y)) s
    | Just (i,j) <- cellOf x y
    , Nothing <- s !! j !! i
    , let player = even (length (filter isJust (concat s)))
    = update j (update i (const (Just (turn s)))) s
handleTTT _ s = s

turn :: TTTState -> Bool
turn s = even (length (filter isJust (concat s)))

update :: Int -> (a -> a) -> [a] -> [a]
update _ _ [] = error "update: List too short"
update 0 f (x:xs) = f x : xs
update n f (x:xs) = x : update (n-1) f xs


mineTTT :: Int -> Event -> TTTState -> Bool
mineTTT 0 (MousePress _ _) s = turn s
mineTTT 1 (MousePress _ _) s = not (turn s)
mineTTT _ _ _ = False

initTTT :: TTTState
initTTT = replicate 3 (replicate 3 Nothing)

ticTacToe :: Interaction
ticTacToe = Interaction initTTT stepTTT handleTTT drawTTT

ticTacToeMP :: PseudoCollaboration
ticTacToeMP = PseudoCollaboration initTTT stepTTT mineTTT handleTTT (const drawTTT)


