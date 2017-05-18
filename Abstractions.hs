{-# LANGUAGE ExistentialQuantification #-}
module Abstractions where

import CodeWorld

data Interaction = forall a. Interaction
    { init :: a
    , step :: Double -> a -> a
    , handle :: Event -> a -> a
    , draw :: a -> Picture
    }

data PseudoCollaboration = forall a. PseudoCollaboration
    { initPC :: a
    , stepPC :: Double -> a -> a
    , minePC :: Int -> Event -> a -> Bool
    , handlePC :: Event -> a -> a
    , drawPC :: Int -> a -> Picture
    }

static :: Picture -> Interaction
static pic = Interaction () (const id) (const id) (const pic)

