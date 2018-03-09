{-# LANGUAGE OverloadedStrings #-}
import Pong
import TicTacToe
import Walking
import qualified PongState
import Abstractions
import CodeWorld
import Combinators
import System.Random
import qualified Data.Text as T

main :: IO ()
main = do
    gen1 <- newStdGen
    gen2 <- newStdGen
    runInteraction $ slideshow $
        [ title
        , static codeWorldLogo
        , drawingsSlide
        , smileyCode
        , restartable $ paused $ smileySlide
        , interactionsSlide1
        , restartable $ ticTacToe
        , interactionsSlide2
        , restartable $ pausable $ pong gen1
        , restartable $ paused $ groupSlide
        , collaborationSlide
        , restartable $ paused $ transmitSlide1
        , restartable $ paused $ transmitSlide2
        , restartable $ paused $ transmitSlide3
        , restartable $ pausable $
            delayEvents 0 ticTacToeMP `overlay` delayEvents 1 ticTacToeMP
        , restartable $ pausable $
            delayEvents 0 (pongMB gen1) `overlay` delayEvents 1 (pongMB gen1)
        , static $ titleText "Lock-step simulation"
        , restartable $ paused $ transmitSlide4
        --, restartable $ pausable $
        --    delayEventsLargeStepPrediction 0 (pongMB gen1) `overlay` delayEventsLargeStepPrediction 1 (pongMB gen1)
        , restartable $ pausable $
            delayEventsPrediction 0 (pongMB gen1) `overlay` delayEventsPrediction 1 (pongMB gen1)
        , static $ titleText "What can go wrong?"
        , quote
        , restartable $ pausable $
            delayEventsPrediction 0 (PongState.pongMB gen1) `overlay` delayEventsPrediction 1 (PongState.pongMB gen1)
        , restartable $ pausable $
            delayEventsPrediction 0 (pongMB gen1) `overlay` delayEventsPrediction 1 (pongMB gen2)
        , static $ titleText "How?"
        , howSlide
        , static $ titleText "Interpolation"
        , restartable $ pausable $
            delayEventsPrediction 0 walkingMB `overlay` delayEventsInterpolation 1 walkingMB
        , restartable $ pausable $
            delayEventsPrediction 0 (pongMB gen1) `overlay` delayEventsInterpolation 1 (pongMB gen1)
        , moreSlide
        , playSlide
        ]

-- Slides

title :: Interaction
title = static $ vcat
    [ titleText "Lock-step simulation"
    , blank
    , titleText "is child’s play"
    , blank, blank, blank, blank, blank, blank
    , slideText "Joachim Breitner"
    , slideText "University of Pennsylvania"
    ]

titleText, slideText, codeText :: T.Text -> Picture
titleText = scaled 1.8 1.8 . styledText Plain Serif
slideText = styledText Plain SansSerif
codeText = styledText Plain Monospace

vcat :: [Picture] -> Picture
vcat xs = translated 0 (fromIntegral (length xs - 1)/2) $ mconcat $
    zipWith (\n p -> translated 0 (-fromIntegral n) p) [0::Int ..] xs


codeListing :: [T.Text] -> Picture
codeListing ls = vcat $ map (codeText . padTo l) ls
  where
    l = maximum (map T.length ls)

padTo :: Int -> T.Text -> T.Text
padTo n s = s <> T.replicate (n - T.length s) " "

drawingsSlide :: Interaction
drawingsSlide = static $ codeListing
    [ "drawingOf :: Picture -> IO ()" ]

smileyCode :: Interaction
smileyCode = static $ codeListing
    [ " main = drawingOf"
    , "   (thickArc 2 (-pi) 0 6 <>"
    , "    translated (-4) 4"
    , "       (solidCircle 2) <>"
    , "    translated 4 4"
    , "       (solidCircle 2) <>"
    , "    colored yellow"
    , "       (solidCircle 10))"
    ]

smileySlide :: Interaction
smileySlide = animate $ \x -> smiley ((x/2) `min` 1)

smiley :: Double -> Picture
smiley x =
    translated 0 (-3) (scaled 1 x' (translated 0 3 (thickArc 2 (-pi) 0 6))) &
    translated (-4) 4 (solidCircle 2) &
    translated 4 4 (solidCircle 2) &
    colored yellow (solidCircle 10)
  where x' = 1 - 1.8 * x

groupSlide :: Interaction
groupSlide = animate $ \x ->
    let x1 = ((1 - (x-1)/2)) `max` 0 `min` 1
        x2 = ((x - 4) `max` 0) `min` 1 in
    mconcat $
        [ translated 0 7.8 $ scaled 0.2 0.2 $ smiley (1-x2) ] ++
        [ translated 0 3 $ scaled 1 1 $ computer  | x > 0 ] ++
        (if x > 0 then
        [ translated (-7 - (x1 * 10)) (-5)  $
            scaled 0.2 0.2 (smiley 0) <> translated 4 (-1) computer
        , translated (7 + (x1 * 10)) (-5)   $
            scaled 0.2 0.2 (smiley 0) <> translated (-4) (-1) computer
        ] ++
        [  path [ (0.2, 1.5), (2,-3) ]
        <>  path [ (-0.2,1.5),(-2,-3) ]
        <>  path [ (1.3,-5),  (-1.3,-5) ]
        | x > 3.5 ]
        else [])

interactionsSlide1 :: Interaction
interactionsSlide1 = static $ codeListing
    [ "interactionOf"
    , "  :: s"
    , "  -> (Event -> s -> s)"
    , "  -> (s -> Picture)"
    , "  -> IO ()"
    ]
interactionsSlide2 :: Interaction
interactionsSlide2 = static $ codeListing
    [ "interactionOf"
    , "  :: s"
    , "  -> (Double -> s -> s)"
    , "  -> (Event  -> s -> s)"
    , "  -> (s -> Picture)"
    , "  -> IO ()"
    ]
collaborationSlide :: Interaction
collaborationSlide = static $ codeListing
    [ "collaborationOf"
    , "  :: Int"
    , "  -> s"
    , "  -> (Double -> s -> s)"
    , "  -> (Int -> Event -> s -> s)"
    , "  -> (Int -> s -> Picture)"
    , "  -> IO ()"
    ]


transmitSlide1 :: Interaction
transmitSlide1 = animate $ animateTransmit
        [ tictactoePicture0, tictactoePicture1, tictactoePicture1, tictactoePicture1, tictactoePicture2 ]
        [ tictactoePicture0, tictactoePicture0, tictactoePicture1, tictactoePicture2, tictactoePicture2 ]
        [ tttStatePicture1, tttStatePicture2 ]

transmitSlide2 :: Interaction
transmitSlide2 = animate $ animateTransmit
        [ tictactoePicture0, tictactoePicture1, tictactoePicture1, tictactoePicture1, tictactoePicture2, tictactoePicture2, tictactoePicture2]
        [ tictactoePicture0, tictactoePicture0, tictactoePicture1, tictactoePicture1, tictactoePicture1, tictactoePicture1, tictactoePicture2 ]
        [ tictactoePicture1, keyPicture "2", tictactoePicture2 ]

transmitSlide3 :: Interaction
transmitSlide3 = animate $ animateTransmit
        [ tictactoePicture0, tictactoePicture1, tictactoePicture1, tictactoePicture1, tictactoePicture2 ]
        [ tictactoePicture0, tictactoePicture0, tictactoePicture1, tictactoePicture2, tictactoePicture2 ]
        [ keyPicture "2", keyPicture "1" ]

transmitSlide4 :: Interaction
transmitSlide4 = animate $ animateTransmit
        [ tictactoePicture0, tictactoePicture1, tictactoePicture1, tictactoePicture1, tictactoePicture2 ]
        [ tictactoePicture0, tictactoePicture0, tictactoePicture1, tictactoePicture2, tictactoePicture2 ]
        [ timedKeyPicture "2" 0, timedKeyPicture "1" 0.3 ]

animateTransmit :: [Picture] -> [Picture] -> [Picture] -> Double -> Picture
animateTransmit screens1 screens2 msgs d = mconcat $
    [ translated ((-5) + x') (5 - x') (fitToScreen msg)
    | i < length msgs && 0 < x' && x' < 10 ] ++
    [ translated (-5) 5 (fitToScreen screen1)
    , translated 5 (-5) (fitToScreen screen2)
    , translated (-5) 5 computer
    , translated 5 (-5) computer
    ]
 where
    i = floor (3*d / 13)
    i' = if x > 0 then 2*i+1 else 2*i
    x = ((3*d - 3 - 13*fromIntegral i) `max` (-3)) `min` 10
    x' = if odd i then 10 - x else x

    screen1 = screens1 !! min i' (length screens1 - 1)
    screen2 = screens2 !! min i' (length screens2 - 1)
    msg = msgs !! i

fitToScreen :: Picture -> Picture
fitToScreen p = translated 0 (1.5*(0.8*0.8+0.2)) $ scaled s s p
  where s = 1.5 * 0.8 * 1.6/20

computer :: Picture
computer = scaled 1.5 1.5 $ mconcat $
    [ path [ (-1,0), (-1,1.6), (1, 1.6), (1,0), (-1,0) ]
    , translated 0 0.2 $ scaled 0.9 0.8 $  path [ (-1,0), (-1,1.6), (1, 1.6), (1,0), (-1,0) ]
    , path [ (-1,0), (-1.5,-0.6), (1.5, -0.6), (1,0), (-1,0) ]
    ] ++
    [  path [ (-1 - 0.5*x,-0.6 * x), (1 +0.5*x, -0.6 * x)] | x <- [0.2,0.4,0.6,0.8]] ++
    [  path [ (-1 + x * 2,0), (-1.5 + x*3, -0.6)] | x <- [0.2,0.4,0.6,0.8]]

pongPicture :: Picture
pongPicture =
    case pong (mkStdGen 0) of
        Interaction init step handle draw ->
            let s = step 1 (handle (KeyPress "S") init)
            in draw s <> colored (gray 0.7) (solidRectangle 20 20)

tictactoePicture0 :: Picture
tictactoePicture0 =
    case ticTacToe of
        Interaction init _step handle draw ->
            let s = init
            in draw s <> colored (gray 0.7) (solidRectangle 20 20)

tictactoePicture1 :: Picture
tictactoePicture1 =
    case ticTacToe of
        Interaction init _step handle draw ->
            let s = handle (MousePress LeftButton  (0,7)) init
            in draw s <> colored (gray 0.7) (solidRectangle 20 20)

tictactoePicture2 :: Picture
tictactoePicture2 =
    case ticTacToe of
        Interaction init _step handle draw ->
            let s = handle (MousePress LeftButton (-7,7)) $
                    handle (MousePress LeftButton  (0,7)) init
            in draw s <> colored (gray 0.7) (solidRectangle 20 20)

tttStatePicture1 :: Picture
tttStatePicture1 = scaled 5 5 $ codeText "(1,2)"
tttStatePicture2 :: Picture
tttStatePicture2 = scaled 5 5 $ codeText "(1,1)"

timedKeyPicture c t = scaled 0.8 0.8 $
    translated (-5) 0 (keyPicture c) <> translated 7 1 (clock t)

clock :: Double -> Picture
clock t = scaled 1.5 1.5 $ mconcat $
    [ thickCircle 0.2 3
    , rotated (0.4 - t) (thickPath 0.4 [(0,0), (2.8,0)])
    , rotated (-1.2 + t) (thickPath 0.4 [(0,0), (2,0)])
    ] ++
    [ rotated (n/12*2 * pi) (thickPath 0.1 [(2.6,0), (3,0)]) |n <- [1..12]] ++
    [ colored (grey 0.8) (solidCircle 3) ]

keyPicture c = scaled 6 6 $ mconcat
  [ polygon [ (-1,0), (1,0), (0.8, 1), (-0.8, 1) ]
  , polygon [ (-1,0), (-1.2,-0.6), (1.2, -0.6), (1,0)]
  , polygon [ (-1.2,-0.6), (-1,0.4),(-0.8, 1) ]
  , polygon [ (1.2,-0.6),  (1,0.4),(0.8, 1) ]
  , translated 0 0.4 $ dilated 0.6 $ text c
  , colored white $ mconcat $
      [ solidPolygon [ (-1,0), (1,0), (0.8, 1), (-0.8, 1), (-1,0) ]
      , solidPolygon [ (-1,0), (-1.2,-0.6), (1.2, -0.6), (1,0), (-1,0) ]
      , solidPolygon [ (-1.2,-0.6), (-1,0.4),(-0.8, 1) ]
      , solidPolygon [ (1.2,-0.6),  (1,0.4),(0.8, 1) ]
      ]
  ]

quote :: Interaction
quote = static $
  translated 3.3 4.5 (rotated 0.1 (
    colored white (scaled  0.75 0.75 (slideText "Code of Conduct")) <> solidRectangle 6.5 1.2))
  <> vcat
  [ slideText "“One of the most vile bugs in"
  , slideText "the universe is the desync bug."
  , slideText "They’re mean sons of bitches."
  , blank
  , slideText "The grand assumption to the"
  , slideText "entire engine architecture is"
  , slideText "all players being fully synchronous."
  , blank
  , slideText "What happens if they aren’t?"
  , slideText "What if the simulations diverge?"
  , blank
  , slideText "Chaos. Anger. Suffering.”"
  , blank
  , slideText "                            – Forrest Smith."
  ]

howSlide :: Interaction
howSlide = static $ codeListing
    [ "type Log = [(Double, Int, Event)]"
    , ""
    , "addEvent :: (Double, Int, Event)"
    , "         -> Log -> Log"
    , "addEvent (t,p,e) l"
    , "  = sort ((t,p,e) : l)"
    , ""
    , "currentState :: Log -> s"
    , "currentState l"
    , "  = foldl go initialState l"
    , "  where"
    , "  go s (t,p,e) = applyEvent p e s"
    ]

moreSlide :: Interaction
moreSlide = static $ vcat
    [ titleText "What else?"
    , blank
    , blank
    , slideText "Handle time"
    , slideText "Caching unchanging history"
    , slideText "Better data structures"
    , blank
    , slideText "Ensuring equal code"
    , slideText "Trouble with sin() & cos()"
    ]


playSlide :: Interaction
playSlide = static $ vcat
    [ titleText "Play now!"
    , blank
    , blank
    , slideText "https://is.gd/codeworldpong"
    , blank
    , slideText "https://is.gd/pongcode"
    , blank
    , blank
    , scaled 0.9 0.9 $ slideText "https://github.com/nomeata/codeworld-talk"
    , blank
    ]


